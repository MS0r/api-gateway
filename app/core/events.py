from typing import Callable

from fastapi import FastAPI
from loguru import logger

from app.core.settings.app import AppSettings
from app.core.settings.base import AppEnvTypes
from app.db.events import close_db_connection, connect_to_db, create_tables, create_initial_data, delete_entries_from_db


def create_start_app_handler(
    app: FastAPI,
    settings: AppSettings,
) -> Callable:  # type: ignore
    async def start_app() -> None:
        await connect_to_db(app, settings)
        await create_tables(app)
        #await delete_entries_from_db(app)
        #await create_initial_data(app)

    return start_app


def create_stop_app_handler(
        app: FastAPI,
        settings : AppSettings) -> Callable:  # type: ignore
    @logger.catch
    async def stop_app() -> None:
        await close_db_connection(app)
        #await delete_entries_from_db(app)

    return stop_app