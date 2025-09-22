import pytest
from fastapi import FastAPI
from httpx import AsyncClient
from starlette.status import HTTP_403_FORBIDDEN, HTTP_404_NOT_FOUND, HTTP_400_BAD_REQUEST
from app.models.domain.unit import Unit
from app.models.domain.exercise import Exercise
from app.models.domain.submission import Submission

@pytest.mark.asyncio
async def test_create_exercise(app: FastAPI, client: AsyncClient, test_unit : Unit):
    # create exercise
    exercise_payload = {
        "exercise": {
            "title": "Test Exercise",
            "description": "An exercise for tests",
            "exercise_schema": "{}",
            "test_cases": "[]",
            "unit_id": test_unit.id
        }
    }
    resp = await client.post(app.url_path_for("exercise:create_exercise"), json=exercise_payload)
    assert resp.status_code == 200
    data = resp.json()
    assert data["title"] == "Test Exercise"
    assert data["unit_id"] == test_unit.id

@pytest.mark.asyncio
async def test_get_exercise(app: FastAPI, client: AsyncClient, test_exercise : Exercise):
    exercise_id = test_exercise.id
    # get exercise
    resp = await client.get(app.url_path_for("exercise:get_exercise", exercise_id=exercise_id))
    assert resp.status_code == 200
    data = resp.json()
    assert data["id"] == exercise_id
    assert data["title"] == "Test Exercise 1"

@pytest.mark.asyncio
async def test_update_exercise(app: FastAPI, client: AsyncClient, test_exercise : Exercise):
    exercise_id = test_exercise.id
    # update exercise (note: Body embed uses param name "update")
    update_payload = {"update": {"title": "New Title", "description": "New desc"}}
    resp = await client.put(app.url_path_for("exercise:update_exercise", exercise_id=exercise_id), json=update_payload)
    assert resp.status_code == 200
    data = resp.json()
    assert data["title"] == "New Title"
    assert data["description"] == "New desc"

@pytest.mark.asyncio
async def test_get_submissions_for_exercise(app: FastAPI, client: AsyncClient,test_submission : Submission):
    exercise_id = test_submission.exercise_id
    resp = await client.get(app.url_path_for("exercise:get_submissions", exercise_id=exercise_id))
    assert resp.status_code in (200, 404)
    if resp.status_code == 200:
        assert isinstance(resp.json(), list)

@pytest.mark.asyncio
async def test_submit_exercise_unauthorized(app: FastAPI, client: AsyncClient):
    # try to submit without auth
    resp = await client.post(app.url_path_for("exercise:submit_exercise", exercise_id=1), json={"submission": {"code_snippet": "ok"}})
    assert resp.status_code == HTTP_403_FORBIDDEN

@pytest.mark.asyncio
async def test_submit_exercise_authorized(app: FastAPI, client: AsyncClient, token: str, test_exercise : Exercise):
    exercise_id = test_exercise.id

    headers = {"Authorization": f"Token {token}"}
    # submit a minimal code snippet; backend erlang service may accept or return an error,
    # accept both successful test run or a handled failure (400/200)
    resp = await client.post(app.url_path_for("exercise:submit_exercise", exercise_id=exercise_id),
                             json={"submission": {"code_snippet": "-module(test). -export([sum/2]). sum(A, B) -> A + B."}},
                             headers=headers)
    assert resp.status_code == 200
    response = resp.json()
    assert response['status'] == "ok"
    assert response['test_results']['failures'] == 0

@pytest.mark.asyncio
async def test_submit_bad_exercise_authorized(app: FastAPI, client: AsyncClient, token: str, test_exercise : Exercise):
    exercise_id = test_exercise.id

    headers = {"Authorization": f"Token {token}"}
    # submit a minimal code snippet; backend erlang service may accept or return an error,
    # accept both successful test run or a handled failure (400/200)
    resp = await client.post(app.url_path_for("exercise:submit_exercise", exercise_id=exercise_id),
                             json={"submission": {"code_snippet": "-module(test). -export([sum/2]). sum(A, B) -> A - B."}},
                             headers=headers)
    assert resp.status_code == 200
    response = resp.json()
    assert response['status'] == "ok"
    assert response['test_results']['failures'] == 1


@pytest.mark.asyncio
async def test_create_exercise_failure(app: FastAPI, client: AsyncClient, test_unit : Unit, mocker):
    exercise_payload = {
        "exercise": {
            "title": "Fail Exercise",
            "description": "Should fail",
            "exercise_schema": "{}",
            "test_cases": "[]",
            "unit_id": test_unit.id
        }
    }
    mocker.patch("app.db.crud.exercise.create_exercise", return_value=None)
    resp = await client.post(app.url_path_for("exercise:create_exercise"), json=exercise_payload)
    assert resp.status_code == HTTP_400_BAD_REQUEST
    assert "Failed to create exercise" in resp.json()["errors"]

@pytest.mark.asyncio
async def test_get_exercise_not_found(app: FastAPI, client: AsyncClient, mocker):
    mocker.patch("app.db.crud.exercise.get_exercise", return_value=None)
    resp = await client.get(app.url_path_for("exercise:get_exercise", exercise_id=9999))
    assert resp.status_code == HTTP_404_NOT_FOUND
    assert "Exercise not found" in resp.json()["errors"]

@pytest.mark.asyncio
async def test_get_submissions_not_found(app: FastAPI, client: AsyncClient, mocker):
    mocker.patch("app.db.crud.submission.get_submissions_by_exercise_id", return_value=[])
    resp = await client.get(app.url_path_for("exercise:get_submissions", exercise_id=9999))
    assert resp.status_code == HTTP_404_NOT_FOUND
    assert "No submissions found for this exercise" in resp.json()["errors"]

@pytest.mark.asyncio
async def test_submit_exercise_exception(app: FastAPI, client: AsyncClient, token: str, mocker):
    headers = {"Authorization": f"Token {token}"}
    mocker.patch("app.services.erlang.submit_code_erlang", side_effect=Exception("Boom"))
    resp = await client.post(
        app.url_path_for("exercise:submit_exercise", exercise_id=1),
        json={"submission": {"code_snippet": "error"}},
        headers=headers
    )
    assert resp.status_code == HTTP_400_BAD_REQUEST
    assert "Boom" in resp.json()["errors"]

@pytest.mark.asyncio
async def test_delete_submission(app: FastAPI, client: AsyncClient, mocker):
    mocker.patch("app.db.crud.submission.delete_submission", return_value=True)
    resp = await client.delete(app.url_path_for("exercise:delete_submission", submission_id=1))
    assert resp.status_code == 200
    assert resp.json() is True

@pytest.mark.asyncio
async def test_update_exercise_not_found(app: FastAPI, client: AsyncClient, mocker):
    update_payload = {"update": {"title": "New Title", "description": "New desc"}}
    mocker.patch("app.db.crud.exercise.update_exercise", return_value=None)
    resp = await client.put(app.url_path_for("exercise:update_exercise", exercise_id=9999), json=update_payload)
    assert resp.status_code == HTTP_404_NOT_FOUND
    assert "Exercise not found" in resp.json()["errors"]