import pytest
from fastapi import FastAPI
from httpx import AsyncClient
from starlette.status import HTTP_403_FORBIDDEN, HTTP_404_NOT_FOUND, HTTP_400_BAD_REQUEST
from app.models.domain.course import Course
from app.models.domain.subunit import Subunit

@pytest.mark.asyncio
async def test_create_course(app: FastAPI, client: AsyncClient):
    course_data = {
        "course": {
            "title": "Sample Course",
            "description": "A course for testing"
        }
    }
    response = await client.post(
        app.url_path_for("course:create"),
        json=course_data
    )
    assert response.status_code == 200
    data = response.json()
    assert data["title"] == "Sample Course"
    assert data["description"] == "A course for testing"

@pytest.mark.asyncio
async def test_create_course_returns_none(app: FastAPI, client: AsyncClient, mocker):
    mocker.patch("app.db.crud.course.create_course", return_value=None)

    response = await client.post(
        app.url_path_for("course:create"),
        json={"course": {"title": "anytitle", "description": "anydescription"}},
    )
    assert response.status_code == 400

@pytest.mark.asyncio
async def test_create_course_invalid(app: FastAPI, client: AsyncClient):
    # Missing required fields
    response = await client.post(
        app.url_path_for("course:create"),
        json={"course": {}}
    )
    assert response.status_code == HTTP_400_BAD_REQUEST or response.status_code == 422

@pytest.mark.asyncio
async def test_get_course(app: FastAPI, client: AsyncClient, test_course: Course):
    course_id = test_course.id
    # Retrieve the course
    response = await client.get(app.url_path_for("course:get", course_id=course_id))
    assert response.status_code == 200
    data = response.json()
    assert data["title"] == "Test Course 1"
    assert data["id"] == course_id

@pytest.mark.asyncio
async def test_get_course_not_found(app: FastAPI, client: AsyncClient):
    response = await client.get(app.url_path_for("course:get", course_id=9999))
    assert response.status_code == HTTP_404_NOT_FOUND

@pytest.mark.asyncio
async def test_update_course(app: FastAPI, client: AsyncClient, test_course: Course):
    course_id = test_course.id
    # Update the course
    update_data = {
        "course": {
            "title": "New Title",
            "description": "New Description"
        }
    }
    response = await client.put(
        app.url_path_for("course:update", course_id=course_id),
        json=update_data
    )
    assert response.status_code == 200
    data = response.json()
    assert data["title"] == "New Title"
    assert data["description"] == "New Description"

@pytest.mark.asyncio
async def test_update_course_not_found(app: FastAPI, client: AsyncClient):
    update_data = {
        "course": {
            "title": "Doesn't Exist",
            "description": "No course"
        }
    }
    response = await client.put(
        app.url_path_for("course:update", course_id=9999),
        json=update_data
    )
    assert response.status_code == HTTP_404_NOT_FOUND

@pytest.mark.asyncio
async def test_enroll_user_in_course(app: FastAPI, client: AsyncClient, token: str, test_course : Course):
    # Create a course first
    course_id = test_course.id

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
async def test_enroll_user_in_course_returns_none(app: FastAPI, client: AsyncClient, token: str, mocker):
    mocker.patch("app.db.crud.course.enroll_user_in_course", return_value=None)

    response = await client.post(
        app.url_path_for("course:enroll", course_id=1),
        headers={"Authorization": f"Token {token}"}
    )
    assert response.status_code == 400

@pytest.mark.asyncio
async def test_get_course_units(app: FastAPI, client: AsyncClient, test_subunit: Subunit):
    course_id = test_subunit.unit.course_id
    # Get units for the course (should be empty list)
    response = await client.get(app.url_path_for("course:get_units", course_id=course_id))
    assert response.status_code == 200
    data = response.json()
    assert isinstance(data, list)

@pytest.mark.asyncio
async def test_get_course_units_not_found(app: FastAPI, client: AsyncClient):
    response = await client.get(app.url_path_for("course:get_units", course_id=9999))
    assert response.status_code == HTTP_404_NOT_FOUND

@pytest.mark.asyncio
async def test_get_course_units_returns_none(app: FastAPI, client: AsyncClient, mocker):
    mocker.patch("app.db.crud.course.get_course_units", return_value=None)

    response = await client.get(app.url_path_for("course:get_units", course_id=1))
    assert response.status_code == 404