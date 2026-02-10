from typing import Callable

from fastapi import FastAPI
from loguru import logger

from app.core.settings.app import AppSettings
from app.core.settings.base import AppEnvTypes
from app.db.events import (
    close_db_connection,
    connect_to_db,
    create_tables,
    create_initial_data_test,
    delete_entries_from_db,
)
from app.services.erlang import ErlangService, RabbitMQConnectionError


def create_start_app_handler(
    app: FastAPI,
    settings: AppSettings,
) -> Callable:  # type: ignore
    async def start_app() -> None:
        await connect_to_db(app, settings)
        await create_tables(app)
        if settings.crt_data:
            await create_initial_data_test(app)

        try:
            app.state.erlang_service = await ErlangService.create(
                connection_string=settings.rabbitmq_cnt_str,
                max_retries=5,
                retry_delay=2.0,
            )
            logger.info("RabbitMQ connection established successfully")
        except RabbitMQConnectionError as e:
            logger.error(f"Failed to connect to RabbitMQ: {e}")
            raise

    return start_app


def create_stop_app_handler(app: FastAPI, settings: AppSettings) -> Callable:  # type: ignore
    @logger.catch
    async def stop_app() -> None:
        await close_db_connection(app)
        if settings.app_env == AppEnvTypes.test:
            await delete_entries_from_db(app)

        if hasattr(app.state, "erlang_service"):
            await app.state.erlang_service.close()

    return stop_app
