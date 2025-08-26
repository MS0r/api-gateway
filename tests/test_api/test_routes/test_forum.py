import pytest
from fastapi import FastAPI
from httpx import AsyncClient
from starlette.status import HTTP_403_FORBIDDEN

from app.models.domain.user import User
from app.models.domain.publication import Question, Answer
from app.models.schemas.publication import QuestionCreate, QuestionRead, AnswerCreate, AnswerRead

@pytest.mark.asyncio
async def test_create_question(app: FastAPI, client: AsyncClient, token: str):
    response = await client.post(
        app.url_path_for("forum:create_question"),
        json={
            "question": {
                "title": "Test Question",
                "body": "This is a test question.",
                "tags": ["erlang"]
            }
        },
        headers={"Authorization": f"Token {token}"}
    )

    assert response.status_code == 200
    data = response.json()
    assert data["title"] == "Test Question"
    assert "erlang" in data["tags"]
    assert data["body"] == "This is a test question."

@pytest.mark.asyncio
async def test_create_question_unauthorized(app: FastAPI, client: AsyncClient):
    response = await client.post(
        app.url_path_for("forum:create_question"),
        json={
            "question": {
                "title": "Unauthorized",
                "body": "Should fail",
                "tags": []
            }
        }
    )
    assert response.status_code == HTTP_403_FORBIDDEN

@pytest.mark.asyncio
async def test_list_questions(app: FastAPI, client: AsyncClient):
    response = await client.get(app.url_path_for("forum:search_questions"))
    assert response.status_code == 200
    data = response.json()
    assert isinstance(data, list)

@pytest.mark.asyncio
async def test_create_answer(
    app: FastAPI, client: AsyncClient, token: str, test_question: Question
):
    # Create an answer
    response = await client.post(
        app.url_path_for("forum:create_answer", question_id=test_question.id),
        json={"body": "This is the answer."},
        headers={"Authorization": f"Token {token}"}
    )

    assert response.status_code == 200
    data = response.json()
    assert data["body"] == "This is the answer."
    assert data["question_id"] == test_question.id

@pytest.mark.asyncio
async def test_get_question(app: FastAPI, client: AsyncClient, test_question: Question):
    response = await client.get(app.url_path_for("forum:get_question", question_id=test_question.id))
    assert response.status_code == 200
    data = response.json()
    assert data["title"] == test_question.title
    assert data["body"] == test_question.body

@pytest.mark.asyncio
async def test_get_question_not_found(app: FastAPI, client: AsyncClient):
    response = await client.get(app.url_path_for("forum:get_question", question_id=9999))
    assert response.status_code == 404

@pytest.mark.asyncio
async def test_get_answers(app: FastAPI, client: AsyncClient, test_question: Question, token: str):
    # Create an answer first
    await client.post(
        app.url_path_for("forum:create_answer", question_id=test_question.id),
        json={"body": "Answer for get_answers"},
        headers={"Authorization": f"Token {token}"}
    )
    response = await client.get(app.url_path_for("forum:get_answers", question_id=test_question.id))
    assert response.status_code == 200
    data = response.json()
    assert isinstance(data, list)
    assert any(answer["body"] == "Answer for get_answers" for answer in data)

@pytest.mark.asyncio
async def test_create_answer_unauthorized(app: FastAPI, client: AsyncClient):
    response = await client.post(
        app.url_path_for("forum:create_answer", question_id=1),
        json={"body": "Should fail"}
    )
    assert response.status_code == HTTP_403_FORBIDDEN

@pytest.mark.asyncio
async def test_search_questions(app: FastAPI, client: AsyncClient):
    response = await client.get(app.url_path_for("forum:search_questions"))
    assert response.status_code == 200
    assert isinstance(response.json(), list)