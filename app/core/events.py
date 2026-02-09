import aio_pika
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
    delete_entries_from_db
    )
from app.services.erlang import ErlangService


def create_start_app_handler(
    app: FastAPI,
    settings: AppSettings,
) -> Callable:  # type: ignore
    async def start_app() -> None:
        await connect_to_db(app, settings)
        await create_tables(app)
        if settings.crt_data:
            await create_initial_data_test(app)
        app.state.rabbit_connection = await aio_pika.connect_robust(
            settings.rabbitmq_cnt_str
        )

        channel = await app.state.rabbit_connection.channel()
        await channel.set_qos(prefetch_count=10)
        
        app.state.erlang_service = await ErlangService.create(channel)

    return start_app


def create_stop_app_handler(
        app: FastAPI,
        settings : AppSettings) -> Callable:  # type: ignore
    @logger.catch
    async def stop_app() -> None:
        await close_db_connection(app)
        if settings.app_env == AppEnvTypes.test:
            await delete_entries_from_db(app)
        await app.state.rabbit_connection.close()

    return stop_app