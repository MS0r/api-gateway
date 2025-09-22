import pytest
from fastapi import FastAPI
from httpx import AsyncClient
from starlette.status import HTTP_403_FORBIDDEN, HTTP_404_NOT_FOUND

from app.models.domain.publication import Question, Answer
from app.models.domain.vote import Vote

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
async def test_list_questions(app: FastAPI, client: AsyncClient, test_question : Question):
    response = await client.get(app.url_path_for("forum:search_questions"))
    assert response.status_code == 200
    data = response.json()
    assert isinstance(data, list)
    assert any(q["id"] == test_question.id for q in data)

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
async def test_create_answer_unauthorized(app: FastAPI, client: AsyncClient):
    response = await client.post(
        app.url_path_for("forum:create_answer", question_id=1),
        json={"body": "Should fail"}
    )
    assert response.status_code == HTTP_403_FORBIDDEN

@pytest.mark.asyncio
async def test_get_question(app: FastAPI, client: AsyncClient, test_answer: Answer):
    question_id = test_answer.question_id
    response = await client.get(app.url_path_for("forum:get_question", question_id=question_id))
    assert response.status_code == 200
    data = response.json()
    assert data["title"] == test_answer.question.title
    assert data["body"] == test_answer.question.body
    assert data["user"] == test_answer.question.user.username
    assert isinstance(data["answers"], list)
    assert any(answer["id"] == test_answer.id for answer in data["answers"])
    assert any(answer["body"] == test_answer.body for answer in data["answers"])

@pytest.mark.asyncio
async def test_get_question_not_found(app: FastAPI, client: AsyncClient):
    response = await client.get(app.url_path_for("forum:get_question", question_id=9999))
    assert response.status_code == 404

@pytest.mark.asyncio
async def test_get_answers(app: FastAPI, client: AsyncClient, test_answer: Answer):
    # Create an answer first
    question_id = test_answer.question_id
    response = await client.get(app.url_path_for("forum:get_answers", question_id=question_id))
    assert response.status_code == 200
    data = response.json()
    assert isinstance(data, list)
    assert any(answer["id"] == test_answer.id for answer in data)
    assert any(answer["body"] == test_answer.body for answer in data)


@pytest.mark.asyncio
async def test_search_questions(app: FastAPI, client: AsyncClient, test_question : Question):
    response = await client.get(app.url_path_for("forum:search_questions"), params={"s": "Test"})
    assert response.status_code == 200
    assert isinstance(response.json(), list)
    assert any(q["id"] == test_question.id for q in response.json())

@pytest.mark.asyncio
async def test_search_questions_empty(app: FastAPI, client: AsyncClient, mocker):
    # Return empty list
    mocker.patch("app.db.crud.publication.get_last_questions", return_value=[])
    resp = await client.get(app.url_path_for("forum:search_questions"))
    assert resp.status_code == 200
    assert resp.json() == []

@pytest.mark.asyncio
async def test_view_question_not_found(app: FastAPI, client: AsyncClient, mocker):
    mocker.patch("app.services.forum.get_question", side_effect=Exception("Not found"))
    resp = await client.get(app.url_path_for("forum:view_question", question_id=9999))
    assert resp.status_code == HTTP_404_NOT_FOUND
    assert "Question not found" in resp.json()["errors"]

@pytest.mark.asyncio
async def test_get_answers_not_found(app: FastAPI, client: AsyncClient, mocker):
    mocker.patch("app.services.forum.get_answers_by_question", side_effect=Exception("No answers"))
    resp = await client.get(app.url_path_for("forum:get_answers", question_id=9999))
    assert resp.status_code == HTTP_404_NOT_FOUND
    assert "Answers not found" in resp.json()["errors"]

@pytest.mark.asyncio
async def test_vote_question(app: FastAPI, client: AsyncClient, token: str, test_question: Question):
    headers = {"Authorization": f"Token {token}"}
    resp = await client.post(app.url_path_for("forum:vote_question", question_id=test_question.id),
                             json={"vote": "upvote"}, headers=headers)
    assert resp.status_code == 200
    assert resp.json()["id"] == test_question.id

@pytest.mark.asyncio
async def test_vote_question_upvote(app: FastAPI, client: AsyncClient, token: str, test_question: Question):
    headers = {"Authorization": f"Token {token}"}
    resp = await client.post(
        app.url_path_for("forum:vote_question", question_id=test_question.id),
        json={"vote": "upvote"},
        headers=headers
    )
    assert resp.status_code == 200
    assert resp.json()["id"] == test_question.id

@pytest.mark.asyncio
async def test_vote_question_downvote(app: FastAPI, client: AsyncClient, token: str, test_question: Question):
    headers = {"Authorization": f"Token {token}"}
    resp = await client.post(
        app.url_path_for("forum:vote_question", question_id=test_question.id),
        json={"vote": "downvote"},
        headers=headers
    )
    assert resp.status_code == 200
    assert resp.json()["id"] == test_question.id

@pytest.mark.asyncio
async def test_vote_same_question_upvote(app: FastAPI, client: AsyncClient, token: str, question_upvote: Vote):
    headers = {"Authorization": f"Token {token}"}
    resp = await client.post(
        app.url_path_for("forum:vote_question", question_id=question_upvote.question_id),
        json={"vote": "upvote"},
        headers=headers
    )
    assert resp.status_code == 200
    # The API may return the updated vote, check ID matches
    assert resp.json()["id"] == question_upvote.id

@pytest.mark.asyncio
async def test_vote_question_switch_vote(app: FastAPI, client: AsyncClient, token: str, question_upvote: Vote):
    headers = {"Authorization": f"Token {token}"}
    resp = await client.post(
        app.url_path_for("forum:vote_question", question_id=question_upvote.question_id),
        json={"vote": "downvote"},
        headers=headers
    )
    assert resp.status_code == 200
    assert resp.json()["id"] == question_upvote.id

@pytest.mark.asyncio
async def test_vote_answer_upvote(app: FastAPI, client: AsyncClient, token: str, test_answer: Answer):
    headers = {"Authorization": f"Token {token}"}
    resp = await client.post(
        app.url_path_for("forum:vote_answer", answer_id=test_answer.id),
        json={"vote": "upvote"},
        headers=headers
    )
    assert resp.status_code == 200
    assert resp.json()["id"] == test_answer.id

@pytest.mark.asyncio
async def test_vote_answer_downvote(app: FastAPI, client: AsyncClient, token: str, test_answer: Answer):
    headers = {"Authorization": f"Token {token}"}
    resp = await client.post(
        app.url_path_for("forum:vote_answer", answer_id=test_answer.id),
        json={"vote": "downvote"},
        headers=headers
    )
    assert resp.status_code == 200
    assert resp.json()["id"] == test_answer.id

@pytest.mark.asyncio
async def test_vote_same_answer_downvote(app: FastAPI, client: AsyncClient, token: str, answer_downvote: Vote):
    headers = {"Authorization": f"Token {token}"}
    resp = await client.post(
        app.url_path_for("forum:vote_answer", answer_id=answer_downvote.answer_id),
        json={"vote": "downvote"},
        headers=headers
    )
    assert resp.status_code == 200
    assert resp.json()["id"] == answer_downvote.id

@pytest.mark.asyncio
async def test_vote_answer_switch_vote(app: FastAPI, client: AsyncClient, token: str, answer_upvote: Vote):
    headers = {"Authorization": f"Token {token}"}
    resp = await client.post(
        app.url_path_for("forum:vote_answer", answer_id=answer_upvote.answer_id),
        json={"vote": "downvote"},
        headers=headers
    )
    assert resp.status_code == 200
    assert resp.json()["id"] == answer_upvote.id

@pytest.mark.asyncio
async def test_vote_question_forbidden(app: FastAPI, client: AsyncClient, token: str, mocker):
    headers = {"Authorization": f"Token {token}"}
    mocker.patch("app.services.forum.vote_publication", side_effect=Exception("Vote fail"))
    resp = await client.post(app.url_path_for("forum:vote_question", question_id=1),
                             json={"vote": "upvote"}, headers=headers)
    assert resp.status_code == HTTP_403_FORBIDDEN
    assert "Question not found" in resp.json()["errors"]

@pytest.mark.asyncio
async def test_vote_answer_forbidden(app: FastAPI, client: AsyncClient, token: str, mocker):
    headers = {"Authorization": f"Token {token}"}
    mocker.patch("app.services.forum.vote_publication", side_effect=Exception("Vote fail"))
    resp = await client.post(app.url_path_for("forum:vote_answer", answer_id=1),
                             json={"vote": "downvote"}, headers=headers)
    assert resp.status_code == HTTP_403_FORBIDDEN
    assert "Answer not found" in resp.json()["errors"]