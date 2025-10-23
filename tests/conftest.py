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
from app.core.config import get_app_settings
from app.services import jwt

# This module initializes the domain models for the application.
from app.models.domain import course, exercise, publication, quiz, quiz_pass, submission, subunit, unit, user, vote

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
async def test_user(session: AsyncSession) -> user.User:
    user_created = user.User(
        username="testuser",
        email="testuser@example.com",
        password="password",
        role=user.UserRole.USER,
        status=user.UserStatus.ACTIVE
        )
    session.add(user_created)
    await session.commit()
    await session.refresh(user_created)
    return user_created

@pytest_asyncio.fixture
async def test_course(session: AsyncSession) -> course.Course:
    course_created = course.Course(
        title="Test Course 1",
        description="This is a test course."
    )
    session.add(course_created)
    await session.commit()
    await session.refresh(course_created)
    return course_created

@pytest_asyncio.fixture
async def test_unit(session: AsyncSession, test_course: course.Course) -> unit.Unit:
    unit_created = unit.Unit(
        title="Test Unit 1",
        description="This is a test unit.",
        order=1,
        course_id=test_course.id
    )
    session.add(unit_created)
    await session.commit()
    await session.refresh(unit_created)
    return unit_created

@pytest_asyncio.fixture
async def test_subunit(session: AsyncSession, test_unit: unit.Unit) -> subunit.Subunit:
    subunit_created = subunit.Subunit(
        title="Test Subunit 1",
        description="This is a test subunit.",
        order=1,
        blocks=[{"type" : "text", "value": "hola"}],
        unit_id=test_unit.id
    )
    session.add(subunit_created)
    await session.commit()
    await session.refresh(subunit_created)
    return subunit_created

@pytest_asyncio.fixture
async def test_quiz(session: AsyncSession, test_subunit: subunit.Subunit) -> quiz.Quiz:
    quiz_created = quiz.Quiz(
        title="Test Quiz 1",
        description="This is a test quiz.",
        subunit_id=test_subunit.id
    )
    session.add(quiz_created)
    await session.commit()
    await session.refresh(quiz_created)
    quiz_question = quiz.QuizQuestion(
        question_text="What is the capital of France?",
        quiz_id=quiz_created.id
    )
    session.add(quiz_question)
    await session.commit()
    await session.refresh(quiz_question)
    option = quiz.Option(
        text="Paris",
        is_correct=True,
        quiz_question_id=quiz_question.id
    )
    session.add(option)
    await session.commit()
    await session.refresh(quiz_created)
    return quiz_created
    
@pytest_asyncio.fixture
async def test_quiz_pass(session: AsyncSession, test_user: user.User, test_quiz: quiz.Quiz) -> quiz_pass.QuizPass:
    quiz_pass_created = quiz_pass.QuizPass(
        user_id=test_user.id,
        quiz_id=test_quiz.id
    )
    session.add(quiz_pass_created)
    await session.commit()
    await session.refresh(quiz_pass_created)
    return quiz_pass_created

@pytest_asyncio.fixture
async def test_exercise(session: AsyncSession, test_unit: unit.Unit) -> exercise.Exercise:
    exercise_created = exercise.Exercise(
        title="Test Exercise 1",
        description="This is a test exercise.",
        exercise_schema="",
        test_cases="""defmodule Test do use ExUnit.Case
test "sum" do assert :test.sum(1,1) == 2 end end""",
        unit_id=test_unit.id
    )
    session.add(exercise_created)
    await session.commit()
    await session.refresh(exercise_created)
    return exercise_created

@pytest_asyncio.fixture
async def test_submission(session: AsyncSession, test_exercise: exercise.Exercise, test_user: user.User) -> submission.Submission:
    submission_created = submission.Submission(
        exercise_id=test_exercise.id,
        user_id=test_user.id,
        code_snippet="-module(test). -export([sum/2]). sum(A, B) -> A + B."
    )
    session.add(submission_created)
    await session.commit()
    await session.refresh(submission_created)
    return submission_created

@pytest_asyncio.fixture
async def test_question(session: AsyncSession, test_user: user.User) -> publication.Question:
    question = publication.Question(
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
async def test_answer(session: AsyncSession, test_user: user.User, test_question: publication.Question) -> publication.Answer:
    answer_created = publication.Answer(
        body="Response",
        user_id=test_user.id,
        question_id=test_question.id
    )
    session.add(answer_created)
    await session.commit()
    await session.refresh(answer_created)
    return answer_created

# Vote fixtures
@pytest_asyncio.fixture
async def question_upvote(session: AsyncSession, test_user: user.User, test_question: publication.Question) -> vote.Vote:
    vote_created = vote.Vote(
        user_id=test_user.id,
        question_id=test_question.id,
        vote=vote.VoteType.UPVOTE
    )
    session.add(vote_created)
    await session.commit()
    await session.refresh(vote_created)
    return vote_created

@pytest_asyncio.fixture
async def question_downvote(session: AsyncSession, test_user: user.User, test_question: publication.Question) -> vote.Vote:
    vote_created = vote.Vote(
        user_id=test_user.id,
        question_id=test_question.id,
        vote=vote.VoteType.DOWNVOTE
    )
    session.add(vote_created)
    await session.commit()
    await session.refresh(vote_created)
    return vote_created

@pytest_asyncio.fixture
async def answer_upvote(session: AsyncSession, test_user: user.User, test_answer: publication.Answer) -> vote.Vote:
    vote_created = vote.Vote(
        user_id=test_user.id,
        answer_id=test_answer.id,
        vote=vote.VoteType.UPVOTE
    )
    session.add(vote_created)
    await session.commit()
    await session.refresh(vote_created)
    return vote_created

@pytest_asyncio.fixture
async def answer_downvote(session: AsyncSession, test_user: user.User, test_answer: publication.Answer) -> vote.Vote:
    vote_created = vote.Vote(
        user_id=test_user.id,
        answer_id=test_answer.id,
        vote=vote.VoteType.DOWNVOTE
    )
    session.add(vote_created)
    await session.commit()
    await session.refresh(vote_created)
    return vote_created

@pytest_asyncio.fixture
async def token(test_user: user.User) -> str:
    from app.core.config import get_app_settings
    settings = get_app_settings()
    return jwt.create_access_token_for_user(test_user, settings.secret_key.get_secret_value())

@pytest.fixture
def authorized_client(client: AsyncClient, authorization_prefix: str, token: str) -> AsyncClient:
    client.headers ={
        "Authorization": f"{authorization_prefix} {token}",
        **client.headers
    }
    return client