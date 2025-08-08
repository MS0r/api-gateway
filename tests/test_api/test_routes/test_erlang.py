import pytest
from fastapi import FastAPI
from httpx import AsyncClient
from starlette.status import HTTP_403_FORBIDDEN

from app.models.domain.user import User
from app.services.jwt import create_access_token_for_user

@pytest.mark.asyncio
async def test_execute_valid_erlang_code(
    app : FastAPI, client: AsyncClient
    ):
    code = """
    -module(hello).
    -export([start/0]).

    start() ->
        io:format("Hello, world!~n").
    """

    response = await client.post(
        app.url_path_for("erlang:compile"), 
        json={"erlang_payload" : {"source_code": code}}
    )

    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "ok"
    assert "Hello, world!" in data["result"]
    assert data["reason"] is None

@pytest.mark.asyncio
async def test_execute_syntax_error(
    app : FastAPI, client: AsyncClient   
):
    code = """
    -module(bad).
    -export([start/0]).

    start() ->
        io:format("Missing quote).
    """

    response = await client.post(
        app.url_path_for("erlang:compile"), json={
            "erlang_payload": {"source_code": code}
        }
    )

    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "error"
    assert data["result"] is None
    assert data["reason"] is not None
    assert "unterminated" in data["reason"].lower()

@pytest.mark.asyncio
async def test_execute_runtime_error(
    app: FastAPI, client: AsyncClient
):
    code = """
    -module(runtime).
    -export([start/0]).

    start() ->
        1 div 0.
    """

    response = await client.post(
        app.url_path_for("erlang:compile"), json={
            "erlang_payload": {"source_code": code}
        }
    )

    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "error"
    assert data["result"] is None
    assert "arithmetic" in data["reason"].lower() or "badarith" in data["reason"].lower()

@pytest.mark.asyncio
async def test_execute_timeout(
    app: FastAPI, client: AsyncClient
):
    code = """
    -module(loop).
    -export([start/0]).

    start() ->
        loop().

    loop() ->
        loop().
    """

    response = await client.post(
        app.url_path_for("erlang:compile"), json={
            "erlang_payload": {"source_code": code}
        }
    )

    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "error"
    assert data["result"] is None
    assert "timeout" in data["reason"].lower()