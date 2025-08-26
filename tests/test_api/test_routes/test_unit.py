import pytest
from fastapi import FastAPI
from httpx import AsyncClient
from starlette.status import HTTP_403_FORBIDDEN, HTTP_404_NOT_FOUND

from app.models.schemas.unit import UnitCreate

@pytest.mark.asyncio
async def test_create_unit(app: FastAPI, client: AsyncClient):
    # First, create a course to attach the unit to
    course_data = {
        "course": {
            "title": "Unit Test Course",
            "description": "Course for unit tests"
        }
    }
    course_response = await client.post(app.url_path_for("course:create"), json=course_data)
    assert course_response.status_code == 200
    course_id = course_response.json()["id"]

    # Create a unit
    unit_data = {
        "unit": {
            "title": "Test Unit",
            "description": "Unit for testing",
            "order": 1,
            "course_id": course_id
        }
    }
    response = await client.post(app.url_path_for("unit:create"), json=unit_data)
    assert response.status_code == 200
    data = response.json()
    assert data["title"] == "Test Unit"
    assert data["description"] == "Unit for testing"
    assert data["order"] == 1
    assert data["course_id"] == course_id

@pytest.mark.asyncio
async def test_get_unit(app: FastAPI, client: AsyncClient):
    # First, create a course and a unit
    course_data = {
        "course": {
            "title": "Get Unit Course",
            "description": "Course for get unit test"
        }
    }
    course_response = await client.post(app.url_path_for("course:create"), json=course_data)
    assert course_response.status_code == 200
    course_id = course_response.json()["id"]

    unit_data = {
        "unit": {
            "title": "Get Unit",
            "description": "Unit to get",
            "order": 2,
            "course_id": course_id
        }
    }
    create_response = await client.post(app.url_path_for("unit:create"), json=unit_data)
    assert create_response.status_code == 200
    unit_id = create_response.json()["id"]

    # Now, get the unit
    response = await client.get(app.url_path_for("unit:get", unit_id=unit_id))
    assert response.status_code == 200
    data = response.json()
    assert data["title"] == "Get Unit"
    assert data["id"] == unit_id

@pytest.mark.asyncio
async def test_update_unit(app: FastAPI, client: AsyncClient):
    # Create a course and a unit
    course_data = {
        "course": {
            "title": "Update Unit Course",
            "description": "Course for update unit test"
        }
    }
    course_response = await client.post(app.url_path_for("course:create"), json=course_data)
    assert course_response.status_code == 200
    course_id = course_response.json()["id"]

    unit_data = {
        "unit": {
            "title": "Old Title",
            "description": "Old description",
            "order": 3,
            "course_id": course_id
        }
    }
    create_response = await client.post(app.url_path_for("unit:create"), json=unit_data)
    assert create_response.status_code == 200
    unit_id = create_response.json()["id"]

    # Update the unit
    update_data = {
        "unit": {
            "title": "New Title",
            "description": "New description"
        }
    }
    response = await client.put(app.url_path_for("unit:update", unit_id=unit_id), json=update_data)
    assert response.status_code == 200
    data = response.json()
    assert data["title"] == "New Title"
    assert data["description"] == "New description"

@pytest.mark.asyncio
async def test_get_subunits_for_unit(app: FastAPI, client: AsyncClient):
    # Create a course and a unit
    course_data = {
        "course": {
            "title": "Subunit Course",
            "description": "Course for subunit test"
        }
    }
    course_response = await client.post(app.url_path_for("course:create"), json=course_data)
    assert course_response.status_code == 200
    course_id = course_response.json()["id"]

    unit_data = {
        "unit": {
            "title": "Unit With Subunits",
            "description": "Unit for subunit test",
            "order": 4,
            "course_id": course_id
        }
    }
    create_response = await client.post(app.url_path_for("unit:create"), json=unit_data)
    assert create_response.status_code == 200
    unit_id = create_response.json()["id"]

    # Try to get subunits (should be empty or 404)
    response = await client.get(app.url_path_for("unit:get_subunits", unit_id=unit_id))
    if response.status_code == 404:
        print(response.json())
        assert "No subunits found for this unit" in response.json()["errors"]
    else:
        assert response.status_code == 200
        assert isinstance(response.json(), list)