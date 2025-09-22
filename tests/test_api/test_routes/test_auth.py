import pytest
from fastapi import FastAPI
from httpx import AsyncClient
from starlette.status import HTTP_403_FORBIDDEN, HTTP_409_CONFLICT, HTTP_400_BAD_REQUEST, HTTP_401_UNAUTHORIZED

from app.models.domain.user import User
from app.services.jwt import create_access_token_for_user
from sqlalchemy.exc import IntegrityError

@pytest.mark.asyncio
async def test_unable_to_login_with_wrong_jwt_prefix(
    app: FastAPI, client: AsyncClient, token: str
) -> None:
    response = await client.get(
        app.url_path_for("user:get_current_user"),
        headers={"Authorization": f"WrongPrefix {token}"},
    )
    assert response.status_code == HTTP_403_FORBIDDEN

@pytest.mark.asyncio
async def test_unable_to_login_when_user_does_not_exist_any_more(
    app: FastAPI, client: AsyncClient, authorization_prefix: str
) -> None:
    token = create_access_token_for_user(
        User(username="user", email="email@email.com"), "secret"
    )
    response = await client.get(
        app.url_path_for("user:get_current_user"),
        headers={"Authorization": f"{authorization_prefix} {token}"},
    )
    assert response.status_code == HTTP_403_FORBIDDEN

@pytest.mark.asyncio
async def test_register_user_when_it_exists(
    app: FastAPI, client: AsyncClient, test_user: User
) -> None:
    response = await client.post(
        app.url_path_for("user:register"),
        json={"user" : {"username": test_user.username,"email" : test_user.email ,"password" : "password"}},
    )
    assert response.status_code == HTTP_409_CONFLICT

@pytest.mark.asyncio
async def test_register_user_success(app: FastAPI, client: AsyncClient):
    response = await client.post(
        app.url_path_for("user:register"),
        json={"user": {"username": "newuser", "email": "newuser@email.com", "password": "password123"}},
    )
    assert response.status_code == 200
    data = response.json()
    assert "token" in data
    assert data["username"] == "newuser"
    assert data["email"] == "newuser@email.com"

@pytest.mark.asyncio
async def test_register_user_invalid(app: FastAPI, client: AsyncClient):
    # Missing required fields
    response = await client.post(
        app.url_path_for("user:register"),
        json={"user": {"username": "", "email": "", "password": ""}},
    )
    assert response.status_code == HTTP_400_BAD_REQUEST or response.status_code == 422 or response.status_code == HTTP_409_CONFLICT

@pytest.mark.asyncio
async def test_login_user_success(app: FastAPI, client: AsyncClient):
    # Register first
    await client.post(
        app.url_path_for("user:register"),
        json={"user": {"username": "loginuser", "email": "loginuser@email.com", "password": "password123"}},
    )
    # Login
    response = await client.post(
        app.url_path_for("user:login"),
        json={"user": {"username": "loginuser", "password": "password123"}},
    )
    assert response.status_code == 200
    data = response.json()
    assert "token" in data
    assert data["username"] == "loginuser"

@pytest.mark.asyncio
async def test_login_user_invalid_password(app: FastAPI, client: AsyncClient):
    # Register first
    await client.post(
        app.url_path_for("user:register"),
        json={"user": {"username": "badpassuser", "email": "badpass@email.com", "password": "password123"}},
    )
    # Try login with wrong password
    response = await client.post(
        app.url_path_for("user:login"),
        json={"user": {"username": "badpassuser", "password": "wrongpassword"}},
    )
    assert response.status_code == HTTP_401_UNAUTHORIZED

@pytest.mark.asyncio
async def test_login_user_not_found(app: FastAPI, client: AsyncClient):
    response = await client.post(
        app.url_path_for("user:login"),
        json={"user": {"username": "nouser", "password": "password123"}},
    )
    assert response.status_code == HTTP_401_UNAUTHORIZED

@pytest.mark.asyncio
async def test_register_user_create_returns_none(app: FastAPI, client: AsyncClient, mocker):
    mocker.patch("app.db.crud.user.create_user", return_value=None)

    response = await client.post(
        app.url_path_for("user:register"),
        json={"user": {"username": "anyuser", "email": "any@email.com", "password": "password"}},
    )
    assert response.status_code == 400

@pytest.mark.asyncio
async def test_register_user_integrity_error(app: FastAPI, client: AsyncClient, mocker):
    mocker.patch(
        "app.db.crud.user.create_user", side_effect=IntegrityError("msg", "params", "orig")
    )

    response = await client.post(
        app.url_path_for("user:register"),
        json={"user": {"username": "anyuser", "email": "any@email.com", "password": "password"}},
    )
    assert response.status_code == 409
