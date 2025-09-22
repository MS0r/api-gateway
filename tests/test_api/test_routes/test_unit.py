import pytest
from fastapi import FastAPI
from httpx import AsyncClient
from starlette.status import HTTP_403_FORBIDDEN, HTTP_404_NOT_FOUND, HTTP_400_BAD_REQUEST

from app.models.schemas.unit import UnitCreate
from app.models.domain.course import Course
from app.models.domain.unit import Unit
from app.models.domain.subunit import Subunit

@pytest.mark.asyncio
async def test_create_unit(app: FastAPI, client: AsyncClient, test_course : Course):
    # First, create a course to attach the unit to
    course_id = test_course.id

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
async def test_get_unit(app: FastAPI, client: AsyncClient, test_unit : Unit):

    response = await client.get(app.url_path_for("unit:get", unit_id=test_unit.id))
    assert response.status_code == 200
    data = response.json()
    assert data["title"] == "Test Unit 1"
    assert data["id"] == test_unit.id

@pytest.mark.asyncio
async def test_update_unit(app: FastAPI, client: AsyncClient, test_unit : Unit):
    update_data = {
        "unit": {
            "title": "New Title",
            "description": "New description"
        }
    }
    response = await client.put(app.url_path_for("unit:update", unit_id=test_unit.id), json=update_data)
    assert response.status_code == 200
    data = response.json()
    assert data["title"] == "New Title"
    assert data["description"] == "New description"

@pytest.mark.asyncio
async def test_create_subunit(app: FastAPI, client: AsyncClient, test_unit: Unit):
    subunit_data = {
        "subunit": {
            "title": "Test Subunit",
            "description": "Subunit for testing",
            "order": 1,
            "blocks" : [{"type" : "text", "value" : ""}]
        }
    }
    response = await client.post(app.url_path_for("unit:create_subunit", unit_id=test_unit.id), json=subunit_data)
    assert response.status_code == 200
    data = response.json()
    assert data["title"] == "Test Subunit"
    assert data["description"] == "Subunit for testing"
    assert data["order"] == 1
    assert data["unit_id"] == test_unit.id  

@pytest.mark.asyncio
async def test_get_subunits_for_unit(app: FastAPI, client: AsyncClient, test_subunit : Subunit):

    # Try to get subunits (should be empty or 404)
    response = await client.get(app.url_path_for("unit:get_subunits", unit_id=test_subunit.unit_id))
    assert response.status_code == 200
    assert isinstance(response.json(), list)

@pytest.mark.asyncio
async def test_get_subunits_for_bad_unit(app: FastAPI, client: AsyncClient):

    # Try to get subunits (should be empty or 404)
    response = await client.get(app.url_path_for("unit:get_subunits", unit_id=9999))
    assert response.status_code == 404
    assert "No subunits found for this unit" in response.json()["errors"]
        
@pytest.mark.asyncio
async def test_create_unit_failure(app: FastAPI, client: AsyncClient, mocker):
    mocker.patch("app.db.crud.unit.create_unit", return_value=None)
    unit_data = {
        "unit": {
            "title": "",
            "description": "",
            "order": 1,
            "course_id": 1
        }
    }
    resp = await client.post(app.url_path_for("unit:create"), json=unit_data)
    assert resp.status_code == HTTP_400_BAD_REQUEST
    assert "Failed to create unit" in resp.json()["errors"] 
    
@pytest.mark.asyncio
async def test_get_unit_not_found(app: FastAPI, client: AsyncClient, mocker):
    mocker.patch("app.db.crud.unit.get_unit", return_value=None)
    resp = await client.get(app.url_path_for("unit:get", unit_id=9999))
    assert resp.status_code == HTTP_404_NOT_FOUND
    assert "Unit not found" in resp.json()["errors"]

@pytest.mark.asyncio
async def test_update_unit_not_found(app: FastAPI, client: AsyncClient, mocker):
    mocker.patch("app.db.crud.unit.update_unit", return_value=None)
    resp = await client.put(app.url_path_for("unit:update", unit_id=9999), json={"unit": {"title": "Updated"}})
    assert resp.status_code == HTTP_404_NOT_FOUND
    assert "Unit not found" in resp.json()["errors"]

@pytest.mark.asyncio
async def test_create_subunit_failure(app: FastAPI, client: AsyncClient, mocker):
    subunit_data = {
        "subunit": {
            "title": "",
            "description": "",
            "order": 1,
            "blocks" : []
        }
    }
    mocker.patch("app.db.crud.subunit.create_subunit", return_value=None)
    resp = await client.post(app.url_path_for("unit:create_subunit", unit_id=1),
                             json=subunit_data)
    assert resp.status_code == HTTP_400_BAD_REQUEST
    assert "Failed to create subunit" in resp.json()["errors"]

@pytest.mark.asyncio
async def test_get_subunits_not_found(app: FastAPI, client: AsyncClient, mocker):
    mocker.patch("app.db.crud.unit.get_subunits_by_unit_id", return_value=[])
    resp = await client.get(app.url_path_for("unit:get_subunits", unit_id=1))
    assert resp.status_code == HTTP_404_NOT_FOUND
    assert "No subunits found for this unit" in resp.json()["errors"]