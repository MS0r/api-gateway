import os
from dotenv import load_dotenv
from pathlib import Path
from pydantic import PostgresDsn
import pytest
import pytest_asyncio
from asgi_lifespan import LifespanManager
from collections.abc import AsyncGenerator, Generator
from sqlalchemy import text
from sqlalchemy.ext.asyncio import create_async_engine, AsyncSession, async_sessionmaker
from fastapi import FastAPI
from httpx import AsyncClient, ASGITransport
from app.services import jwt

from app.models.domain.user import User, UserRole, UserStatus
from app.models.domain.publication import Question, Answer

load_dotenv(dotenv_path=Path(__file__).parent.parent / ".env")

os.environ["APP_ENV"] = "test"
os.environ["DATABASE_URL"] = os.getenv("TEST_DATABASE_URL")

@pytest.fixture
def app() -> FastAPI:
    from app.main import get_application

    return get_application()

@pytest_asyncio.fixture
async def initialized_app(app: FastAPI) -> AsyncGenerator[FastAPI, None]:
    async with LifespanManager(app):
        from app.core.config import get_app_settings
        settings = get_app_settings()
        app.state.engine = create_async_engine(
            str(settings.database_url),
            echo=True,
        )
        app.state.session_maker = async_sessionmaker(app.state.engine, expire_on_commit=False, class_=AsyncSession)
        yield app
        await app.state.engine.dispose()

@pytest_asyncio.fixture
async def session(initialized_app: FastAPI) -> AsyncGenerator[AsyncSession,None]:
    from app.models.common import Base
    from app.models import domain
    async with initialized_app.state.session_maker() as session:

        for table in reversed(Base.metadata.sorted_tables):
            await session.execute(text(f'TRUNCATE TABLE "{table.name}" RESTART IDENTITY CASCADE;'))
        await session.commit()

        yield session
        

@pytest_asyncio.fixture
async def client(initialized_app: FastAPI) -> AsyncGenerator[AsyncClient, None]:
    transport = ASGITransport(app=initialized_app)
    async with AsyncClient(
        transport=transport,
        base_url="http://test",
        headers={"Content-Type": "application/json"}
    ) as client:
        yield client

@pytest.fixture
def authorization_prefix() -> str:
    from app.core.config import get_app_settings
    settings = get_app_settings()
    jwt_token_secret = settings.jwt_token_prefix

    return jwt_token_secret


@pytest_asyncio.fixture
async def test_user(session: AsyncSession) -> User:
    user = User(
        username="testuser",
        email="testuser@example.com",
        password="password",
        role=UserRole.USER,
        status=UserStatus.ACTIVE
        )
    session.add(user)
    await session.commit()
    await session.refresh(user)
    return user

@pytest_asyncio.fixture
async def test_question(session: AsyncSession, test_user: User) -> Question:
    question = Question(
            title="Test Question 1",
            body="This is a test question.",
            tags=["erlang"],
            user_id=test_user.id
    )
    session.add(question)
    await session.commit()
    await session.refresh(question)
    return question

@pytest_asyncio.fixture
async def token(test_user: User) -> str:
    return jwt.create_access_token_for_user(test_user, os.environ["SECRET_KEY"])

@pytest.fixture
def authorized_client(client: AsyncClient, authorization_prefix: str, token: str) -> AsyncClient:
    client.headers ={
        "Authorization": f"{authorization_prefix} {token}",
        **client.headers
    }
    return client