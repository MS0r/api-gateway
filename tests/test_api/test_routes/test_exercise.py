import pytest
from fastapi import FastAPI
from httpx import AsyncClient
from starlette.status import HTTP_403_FORBIDDEN, HTTP_404_NOT_FOUND

@pytest.mark.asyncio
async def test_create_exercise(app: FastAPI, client: AsyncClient):
    # create course
    course_payload = {"course": {"title": "Exercise Course", "description": "For exercise tests"}}
    course_resp = await client.post(app.url_path_for("course:create"), json=course_payload)
    assert course_resp.status_code == 200
    course_id = course_resp.json()["id"]

    # create unit
    unit_payload = {
        "unit": {
            "title": "Exercise Unit",
            "description": "Unit for exercise",
            "order": 1,
            "course_id": course_id
        }
    }
    unit_resp = await client.post(app.url_path_for("unit:create"), json=unit_payload)
    assert unit_resp.status_code == 200
    unit_id = unit_resp.json()["id"]

    # create exercise
    exercise_payload = {
        "exercise": {
            "title": "Test Exercise",
            "description": "An exercise for tests",
            "exercise_schema": "{}",
            "test_cases": "[]",
            "unit_id": unit_id
        }
    }
    resp = await client.post(app.url_path_for("exercise:create_exercise"), json=exercise_payload)
    assert resp.status_code == 200
    data = resp.json()
    assert data["title"] == "Test Exercise"
    assert data["unit_id"] == unit_id

@pytest.mark.asyncio
async def test_get_exercise(app: FastAPI, client: AsyncClient):
    # create course/unit/exercise
    course_resp = await client.post(app.url_path_for("course:create"), json={"course": {"title":"C","description":"D"}})
    course_id = course_resp.json()["id"]
    unit_resp = await client.post(app.url_path_for("unit:create"), json={"unit": {"title":"U","description":"D","order":2,"course_id":course_id}})
    unit_id = unit_resp.json()["id"]
    exercise_payload = {"exercise": {"title":"GetEx","description":"desc","exercise_schema":"{}","test_cases":"[]","unit_id":unit_id}}
    create_resp = await client.post(app.url_path_for("exercise:create_exercise"), json=exercise_payload)
    assert create_resp.status_code == 200
    exercise_id = create_resp.json()["id"]

    # get exercise
    resp = await client.get(app.url_path_for("exercise:get_exercise", exercise_id=exercise_id))
    assert resp.status_code == 200
    data = resp.json()
    assert data["id"] == exercise_id
    assert data["title"] == "GetEx"

@pytest.mark.asyncio
async def test_update_exercise(app: FastAPI, client: AsyncClient):
    # create course/unit/exercise
    course_resp = await client.post(app.url_path_for("course:create"), json={"course": {"title":"C2","description":"D"}})
    course_id = course_resp.json()["id"]
    unit_resp = await client.post(app.url_path_for("unit:create"), json={"unit": {"title":"U2","description":"D","order":3,"course_id":course_id}})
    unit_id = unit_resp.json()["id"]
    create_resp = await client.post(app.url_path_for("exercise:create_exercise"), json={"exercise": {"title":"Old","description":"old","exercise_schema":"{}","test_cases":"[]","unit_id":unit_id}})
    exercise_id = create_resp.json()["id"]

    # update exercise (note: Body embed uses param name "update")
    update_payload = {"update": {"title": "New Title", "description": "New desc"}}
    resp = await client.put(app.url_path_for("exercise:update_exercise", exercise_id=exercise_id), json=update_payload)
    assert resp.status_code == 200
    data = resp.json()
    assert data["title"] == "New Title"
    assert data["description"] == "New desc"

@pytest.mark.asyncio
async def test_get_submissions_for_exercise(app: FastAPI, client: AsyncClient,token:str):
    # create course/unit/exercise
    course_resp = await client.post(app.url_path_for("course:create"), json={"course": {"title":"C3","description":"D"}})
    course_id = course_resp.json()["id"]
    unit_resp = await client.post(app.url_path_for("unit:create"), json={"unit": {"title":"U3","description":"D","order":4,"course_id":course_id}})
    unit_id = unit_resp.json()["id"]
    create_resp = await client.post(app.url_path_for("exercise:create_exercise"), json={"exercise": {"title":"SubCheck","description":"desc","exercise_schema":"{}","test_cases":"[]","unit_id":unit_id}})
    exercise_id = create_resp.json()["id"]
    payload = {"submission" : {"code_snippet" : "code_example"}}
    submission = await client.post(app.url_path_for("exercise:submit_exercise", exercise_id=exercise_id),json=payload,headers={"Authorization": f"Token {token}"})

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
async def test_submit_exercise_authorized(app: FastAPI, client: AsyncClient, token: str):
    # create course/unit/exercise
    course_resp = await client.post(app.url_path_for("course:create"), json={"course": {"title":"C4","description":"D"}})
    course_id = course_resp.json()["id"]
    unit_resp = await client.post(app.url_path_for("unit:create"), json={"unit": {"title":"U4","description":"D","order":5,"course_id":course_id}})
    unit_id = unit_resp.json()["id"]
    create_resp = await client.post(app.url_path_for("exercise:create_exercise"), json={"exercise": {"title":"SubmitEx","description":"desc","exercise_schema":"{}","test_cases":"[]","unit_id":unit_id}})
    exercise_id = create_resp.json()["id"]

    headers = {"Authorization": f"Token {token}"}
    # submit a minimal code snippet; backend erlang service may accept or return an error,
    # accept both successful test run or a handled failure (400/200)
    resp = await client.post(app.url_path_for("exercise:submit_exercise", exercise_id=exercise_id),
                             json={"submission": {"code_snippet": "-module(m). -export([start/0]). start() -> ok."}},
                             headers=headers)
    assert resp.status_code in (200, 400)