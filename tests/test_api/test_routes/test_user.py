import pytest
from fastapi import FastAPI
from httpx import AsyncClient
from starlette.status import HTTP_403_FORBIDDEN, HTTP_404_NOT_FOUND

from app.models.domain.user import User
from app.models.domain.course import Course
from app.models.domain.quiz_pass import QuizPass
from app.models.domain.submission import Submission
from app.models.domain.exercise import Exercise

@pytest.mark.asyncio
async def test_get_user_progress(app: FastAPI, client: AsyncClient, token: str, test_course : Course, test_quiz_pass : QuizPass, test_exercise : Exercise):

    response = await client.get(
        app.url_path_for("user:course_progress", course_id=test_course.id),
        headers={"Authorization": f"Token {token}"}
    )
    # 200 if progress exists, 404 if not
    assert response.status_code == 200
    data = response.json()
    assert data["progress"] == 50.0
    assert data["total_quizzes"] == 1
    assert data["completed_quizzes"] == 1
    assert data["total_exercises"] == 1
    assert data["completed_exercises"] == 0

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
async def test_get_user_submissions(app: FastAPI, client: AsyncClient, token: str, test_submission : Submission):
    response = await client.get(
        app.url_path_for("user:get_user_submissions"),
        headers={"Authorization": f"Token {token}"}
    )
    # 200 if submissions exist, 404 if not
    assert response.status_code == 200
    assert isinstance(response.json(), list)
    assert any(sub["id"] == test_submission.id for sub in response.json())

@pytest.mark.asyncio
async def test_get_user_submissions_unauthorized(app: FastAPI, client: AsyncClient):
    response = await client.get(app.url_path_for("user:get_user_submissions"))
    assert response.status_code == HTTP_403_FORBIDDEN

@pytest.mark.asyncio
async def test_get_course_progress_not_found(app: FastAPI, client: AsyncClient, token: str, mocker):
    headers = {"Authorization": f"Token {token}"}
    # Mock user_service to return None
    mocker.patch("app.services.user.get_user_progress", return_value=None)
    resp = await client.get(app.url_path_for("user:course_progress", course_id=1), headers=headers)
    assert resp.status_code == HTTP_404_NOT_FOUND
    assert "No progress found for this course" in resp.json()["errors"]

@pytest.mark.asyncio
async def test_get_user_submissions_not_found(app: FastAPI, client: AsyncClient, token: str, mocker):
    headers = {"Authorization": f"Token {token}"}
    # Mock current_user with no submissions
    user = User(id=1, username="test", email="test@example.com", submissions=[])
    mocker.patch("app.api.dependencies.auth.get_current_user_authorize", return_value=lambda: user)
    
    resp = await client.get(app.url_path_for("user:get_user_submissions"), headers=headers)
    assert resp.status_code == HTTP_404_NOT_FOUND
    assert "No submissions found for this user" in resp.json()["errors"]