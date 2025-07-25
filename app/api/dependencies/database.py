from typing import AsyncGenerator, Callable

from fastapi.concurrency import asynccontextmanager
from sqlalchemy.ext.asyncio import AsyncSession
from fastapi import Depends
from starlette.requests import Request


def _get_db_session_maker(request: Request) -> Callable[[], AsyncSession]:
    """Create a session maker for the given engine."""
    return request.app.state.session_maker

@asynccontextmanager
async def get_db_session(
    session_maker : Callable[[], AsyncSession] = Depends(_get_db_session_maker)
) -> AsyncGenerator[AsyncSession, None]:
    session : AsyncSession = session_maker()
    try:
        yield session
    finally:
        await session.close()