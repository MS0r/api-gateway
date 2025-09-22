import pytest
from fastapi import FastAPI
from httpx import AsyncClient
from starlette.status import HTTP_403_FORBIDDEN, HTTP_400_BAD_REQUEST

from app.models.domain.quiz_pass import QuizPass
from app.models.domain.subunit import Subunit

@pytest.mark.asyncio
async def test_create_quiz_via_api_and_submit(app: FastAPI, client: AsyncClient, test_subunit : Subunit, token : str):    
    quiz_payload = {"quiz": {"title": "Submit API Quiz", "description": "Created for submit test", "subunit_id": test_subunit.id}}
    create_resp = await client.post(app.url_path_for("quiz:create_quiz"), json=quiz_payload)
    # Accept creation or validation/other acceptable responses
    assert create_resp.status_code == 200
    response = create_resp.json()
    quiz_id = response["id"]

    # Now submit the quiz pass
    resp = await client.post(app.url_path_for("quiz:submit", quiz_id=quiz_id), headers={"Authorization": f"Token {token}"})
    assert resp.status_code == 200
    data = resp.json()
    assert "user_id" in data and "quiz_id" in data

@pytest.mark.asyncio
async def test_submit_quiz_unauthorized(app: FastAPI, client: AsyncClient):
    resp = await client.post(app.url_path_for("quiz:submit", quiz_id=1))
    assert resp.status_code == HTTP_403_FORBIDDEN

@pytest.mark.asyncio
async def test_get_quiz_passes_list(app: FastAPI, client: AsyncClient, test_quiz_pass: QuizPass, token: str):
    # Endpoint named "quiz:get_quiz_passes" may return quiz passes or require different params.
    resp = await client.get(app.url_path_for("quiz:get_quiz_passes"), headers={"Authorization": f"Token {token}"})
    # Accept list or validation error / empty results
    assert resp.status_code == 200
    data = resp.json()
    assert isinstance(data, list)
    assert any(quiz_p["id"] == test_quiz_pass.id for quiz_p in data)

@pytest.mark.asyncio
async def test_create_quiz_failure(app: FastAPI, client: AsyncClient, mocker):
    # Mock quiz creation to fail
    mocker.patch("app.db.crud.quiz.create_quiz", return_value=None)
    resp = await client.post(app.url_path_for("quiz:create_quiz"), json={"quiz": {"title": "Fail Quiz", "description" : "", "subunit_id" : 1 }})
    assert resp.status_code == HTTP_400_BAD_REQUEST
    assert "Failed to create quiz" in resp.json()["errors"] 

@pytest.mark.asyncio
async def test_submit_quiz_failure(app: FastAPI, client: AsyncClient, token: str, mocker):
    # Mock no existing quiz pass and creation failure
    mocker.patch("app.db.crud.quiz_pass.get_quiz_passes_by_user_quiz", return_value=None)
    mocker.patch("app.db.crud.quiz_pass.create_quiz_pass", return_value=None)

    headers = {"Authorization": f"Token {token}"}
    resp = await client.post(app.url_path_for("quiz:submit", quiz_id=1), headers=headers)
    assert resp.status_code == HTTP_400_BAD_REQUEST
    assert "Failed to submit quiz" in resp.json()["errors"]