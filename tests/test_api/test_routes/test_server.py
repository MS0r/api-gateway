import pytest
from fastapi import FastAPI
from httpx import AsyncClient

@pytest.mark.asyncio
async def test_health_check_endpoint(
    app: FastAPI, client: AsyncClient
) -> None:
    response = await client.get(
        app.url_path_for("healthcheck")
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "OK"