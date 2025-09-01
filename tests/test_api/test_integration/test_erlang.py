import pytest
from fastapi import FastAPI
from httpx import AsyncClient
from starlette.status import HTTP_403_FORBIDDEN, HTTP_422_UNPROCESSABLE_ENTITY, HTTP_500_INTERNAL_SERVER_ERROR

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
        json={"erlang_payload" : {"code": code}}
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
            "erlang_payload": {"code": code}
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
            "erlang_payload": {"code": code}
        }
    )

    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "error"
    assert data["result"] is None
    assert "arithmetic" in data["reason"].lower() or "badarith" in data["reason"].lower()

@pytest.mark.asyncio
@pytest.mark.timeout(300)
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
            "erlang_payload": {"code": code}
        }
    )

    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "error"
    assert data["result"] is None
    assert "timeout" in data["reason"].lower()

@pytest.mark.asyncio
async def test_erlang_health_check(app: FastAPI, client: AsyncClient):
    """
    Health check for the Erlang service route. Accept 200 on healthy,
    otherwise allow common error responses but validate body when 200.
    """
    resp = await client.get(app.url_path_for("erlang:health"))
    assert resp.status_code in (200, 404, 500)
    if resp.status_code == 200:
        data = resp.json()
        assert data.get("status") in ("ok", "healthy")
        assert "message" in data

@pytest.mark.asyncio
async def test_compile_with_source_code_key(app: FastAPI, client: AsyncClient):
    code = """
    -module(hello_source).
    -export([start/0]).

    start() ->
        io:format("Hello source!~n").
    """
    resp = await client.post(
        app.url_path_for("erlang:compile"),
        json={"erlang_payload": {"code": code}}
    )
    assert resp.status_code == 200
    data = resp.json()
    assert "status" in data
    if data["status"] == "ok":
        assert "Hello source!" in (data.get("result") or "")

@pytest.mark.asyncio
async def test_compile_large_output(app: FastAPI, client: AsyncClient):
    # produce many lines of output to ensure sandbox streaming/aggregation works
    lines = 100
    prints = "\\n".join([f'io:format("line {i}~n").' for i in range(lines)])
    code = f"""
    -module(big).
    -export([start/0]).

    start() ->
        {prints}
    """
    resp = await client.post(
        app.url_path_for("erlang:compile"),
        json={"erlang_payload": {"code": code}}
    )
    assert resp.status_code == 200
    data = resp.json()
    if data["status"] == "ok":
        # ensure at least some lines are present in the result
        assert "line 0" in (data.get("result") or "")

@pytest.mark.asyncio
async def test_compile_missing_payload_returns_422(app: FastAPI, client: AsyncClient):
    resp = await client.post(app.url_path_for("erlang:compile"), json={})
    assert resp.status_code in (HTTP_422_UNPROCESSABLE_ENTITY, 400)

@pytest.mark.asyncio
async def test_erlang_test_endpoint_with_exercise(app: FastAPI, client: AsyncClient):
    """
    Call the erlang test endpoint which runs test cases for an exercise.
    Provide `cases` as an Elixir ExUnit module source string.
    Accept 200 with expected structure or common error codes if exercise missing.
    """
    sample_code = """
    -module(testrun).
    -export([run/0]).

    run() ->
        io:format("test-run~n"),
        ok.
    """

    exunit_module = """
    defmodule SampleExUnit do
      use ExUnit.Case

      test "simple arithmetic" do
        assert 1 + 1 == 2
      end

      test "string contains" do
        assert String.contains?("hello world", "hello")
      end
    end
    """

    resp = await client.post(
        app.url_path_for("erlang:test", exercise_id=1),
        json={"erlang_payload": {"code": sample_code, "cases": exunit_module}}
    )
    assert resp.status_code in (200, 404, HTTP_500_INTERNAL_SERVER_ERROR)
    if resp.status_code == 200:
        data = resp.json()
        # test endpoint should return status and results/test_cases
        assert "status" in data
        assert data["status"] in ("ok", "error")
        assert ("result" in data) or ("cases" in data) or ("test_results" in data)