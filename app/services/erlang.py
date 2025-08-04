import socket
import json
from sqlalchemy.ext.asyncio import AsyncSession
from app.db.crud import exercise as exercise_crud
from app.models.schemas.erlang import ErlangPayload, ErlangCompileResponse

async def compile_erlang_code(payload : ErlangPayload) -> ErlangCompileResponse:
    request_data = {
        "op": "compile",
        "code": payload.source_code,
    }
    try:
        with socket.create_connection(("sandbox", 4000)) as s:
            s.sendall(json.dumps(request_data).encode())
            response = s.recv(4000)
        response_data = json.loads(response.decode())
        return ErlangCompileResponse.model_validate(response_data)
    except json.JSONDecodeError:
        raise RuntimeError("Received invalid JSON from Erlang sandbox")
    except Exception as e:
        raise RuntimeError(f"Error communicating with Erlang sandbox: {e}")

async def health_check() -> dict:
    """Health check endpoint for the Erlang service."""
    # This could be a simple ping to the Erlang service or a more complex check
    # For now, we just return a static response
    # In a real application, you might want to check if the Erlang service is reachable
    # and functioning correctly.
    return {"status": "ok", "message": "Erlang service is running"}

async def test_code_erlang(db : AsyncSession, payload: ErlangPayload, exercise_id : int) -> ErlangCompileResponse:

    exercise = await exercise_crud.get_exercise(db, exercise_id)
    if not exercise:
        raise ValueError(f"Exercise with ID {exercise_id} not found")
    test_cases = exercise.test_cases
    request_data = {
        "op": "test",
        "code": payload.source_code,
        "cases" : test_cases,
    }
    try:
        with socket.create_connection(("sandbox", 4000)) as s:
            s.sendall(json.dumps(request_data).encode())
            response = s.recv(4000)
        response_data = json.loads(response.decode())
        return ErlangCompileResponse.model_validate(response_data)
    except json.JSONDecodeError:
        raise RuntimeError("Received invalid JSON from Erlang sandbox")
    except Exception as e:
        raise RuntimeError(f"Error communicating with Erlang sandbox: {e}")