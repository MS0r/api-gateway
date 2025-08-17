import pytest
from fastapi import FastAPI
from httpx import AsyncClient
from starlette.status import HTTP_403_FORBIDDEN, HTTP_409_CONFLICT

from app.models.domain.user import User
from app.services.jwt import create_access_token_for_user

@pytest.mark.asyncio
async def test_get_user_progress(
    app : FastAPI, client : AsyncClient, token : str
):
    response = await client.get(
        app.url_path_for("user:get_progress"),
        headers={"Authorization": f"Bearer {token}"}
    )
    assert response.status_code == 200