import pytest
from fastapi import FastAPI
from httpx import AsyncClient
from starlette.status import HTTP_403_FORBIDDEN, HTTP_404_NOT_FOUND

from app.models.domain.user import User
from app.services.jwt import create_access_token_for_user

@pytest.mark.asyncio
async def test_get_user_progress(app: FastAPI, client: AsyncClient, token: str):
    # This endpoint requires a course_id
    # First, create a course
    course_data = {
        "course": {
            "title": "Progress Test Course",
            "description": "Course for progress test"
        }
    }
    course_response = await client.post(app.url_path_for("course:create"), json=course_data)
    assert course_response.status_code == 200
    course_id = course_response.json()["id"]

    response = await client.get(
        app.url_path_for("user:course_progress", course_id=course_id),
        headers={"Authorization": f"Token {token}"}
    )
    # 200 if progress exists, 404 if not
    assert response.status_code in (200, 404)
    if response.status_code == 200:
        data = response.json()
        assert "progress" in data

@pytest.mark.asyncio
async def test_get_user_progress_unauthorized(app: FastAPI, client: AsyncClient):
    response = await client.get(app.url_path_for("user:course_progress", course_id=1))
    assert response.status_code == HTTP_403_FORBIDDEN

@pytest.mark.asyncio
async def test_get_user_profile(app: FastAPI, client: AsyncClient, token: str):
    response = await client.get(
        app.url_path_for("user:get_current_user"),
        headers={"Authorization": f"Token {token}"}
    )
    assert response.status_code == 200
    data = response.json()
    assert "username" in data
    assert "email" in data

@pytest.mark.asyncio
async def test_get_user_profile_unauthorized(app: FastAPI, client: AsyncClient):
    response = await client.get(app.url_path_for("user:get_current_user"))
    assert response.status_code == HTTP_403_FORBIDDEN

@pytest.mark.asyncio
async def test_enroll_user_in_course(app: FastAPI, client: AsyncClient, token: str):
    # Create a course first
    course_data = {
        "course": {
            "title": "Test Course",
            "description": "Course for enrollment"
        }
    }
    course_response = await client.post(
        app.url_path_for("course:create"),
        json=course_data
    )
    assert course_response.status_code == 200
    course_id = course_response.json()["id"]

    # Enroll user in the course
    enroll_response = await client.post(
        app.url_path_for("course:enroll", course_id=course_id),
        headers={"Authorization": f"Token {token}"}
    )
    assert enroll_response.status_code == 200
    enroll_data = enroll_response.json()
    assert enroll_data["course_id"] == course_id

@pytest.mark.asyncio
async def test_enroll_user_in_course_unauthorized(app: FastAPI, client: AsyncClient):
    enroll_response = await client.post(
        app.url_path_for("course:enroll", course_id=1)
    )
    assert enroll_response.status_code == HTTP_403_FORBIDDEN

@pytest.mark.asyncio
async def test_get_user_enrollments(app: FastAPI, client: AsyncClient, token: str):
    response = await client.get(
        app.url_path_for("user:enrollments"),
        headers={"Authorization": f"Token {token}"}
    )
    assert response.status_code == 200
    assert isinstance(response.json(), list)

@pytest.mark.asyncio
async def test_get_user_enrollments_unauthorized(app: FastAPI, client: AsyncClient):
    response = await client.get(app.url_path_for("user:enrollments"))
    assert response.status_code == HTTP_403_FORBIDDEN

@pytest.mark.asyncio
async def test_get_user_submissions(app: FastAPI, client: AsyncClient, token: str):
    response = await client.get(
        app.url_path_for("user:get_user_submissions"),
        headers={"Authorization": f"Token {token}"}
    )
    # 200 if submissions exist, 404 if not
    assert response.status_code in (200, 404)
    if response.status_code == 200:
        assert isinstance(response.json(), list)

@pytest.mark.asyncio
async def test_get_user_submissions_unauthorized(app: FastAPI, client: AsyncClient):
    response = await client.get(app.url_path_for("user:get_user_submissions"))
    assert response.status_code == HTTP_403_FORBIDDEN