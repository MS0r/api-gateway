from sqlalchemy.ext.asyncio import create_async_engine, AsyncSession, async_sessionmaker
from contextlib import asynccontextmanager
from fastapi import FastAPI
from loguru import logger
from app.models.common import Base
from typing import AsyncGenerator

import app.models.domain

from app.core.settings.app import AppSettings


async def connect_to_db(app : FastAPI, settings: AppSettings) -> None:
    """Connect to the database."""
    logger.info("Connecting to PostgreSQL")

    app.state.engine = create_async_engine(
        str(settings.database_url),
        echo=True
    )
    app.state.async_session = async_sessionmaker(app.state.engine, expire_on_commit=False)

    logger.info("Database connection established.")


async def create_tables(app:FastAPI) -> None:
    """Initialize the database tables."""
    async with app.state.engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)
    
    logger.info("Database tables initialized.")

async def delete_tables(app: FastAPI) -> None:
    """Delete the database tables."""
    async with app.state.engine.begin() as conn:
        await conn.run_sync(Base.metadata.drop_all)

    logger.info("Database tables deleted.")


async def close_db_connection(app: FastAPI) -> None:
    logger.info("Closing database connection.")

    await app.state.engine.dispose()

    logger.info("Database connection closed.")

@asynccontextmanager
async def get_db_session() -> AsyncGenerator[AsyncSession, None]:
    session : AsyncSession = app.state.async_session()
    try:
        yield session
    finally:
        await session.close()
        logger.info("Database session closed.")
