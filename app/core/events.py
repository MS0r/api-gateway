from typing import Callable

from fastapi import FastAPI
from loguru import logger

from app.core.settings.app import AppSettings
from app.core.settings.base import AppEnvTypes
from app.db.events import close_db_connection, connect_to_db, create_tables, delete_tables


def create_start_app_handler(
    app: FastAPI,
    settings: AppSettings,
) -> Callable:  # type: ignore
    async def start_app() -> None:
        await connect_to_db(app, settings)
        await create_tables(app)

    return start_app


def create_stop_app_handler(
        app: FastAPI,
        settings : AppSettings) -> Callable:  # type: ignore
    @logger.catch
    async def stop_app() -> None:
        logger.info(settings.app_env)
        await close_db_connection(app)

    return stop_app