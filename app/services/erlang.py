import socket
import json
from sqlalchemy.ext.asyncio import AsyncSession
from app.db.crud import exercise as exercise_crud
from app.db.crud import submission as submission_crud
from app.models.schemas.submission import SubmissionCreate
from app.models.schemas.erlang import ErlangPayload, ErlangCompileResponse, ErlangTestResponse,ErlangTestPayload

async def health_check() -> dict:
    """Health check endpoint for the Erlang service."""
    # This could be a simple ping to the Erlang service or a more complex check
    # For now, we just return a static response
    # In a real application, you might want to check if the Erlang service is reachable
    # and functioning correctly.
    return {"status": "ok", "message": "Erlang service is running"}

def send_data_sandbox(payload : dict) -> ErlangTestResponse | ErlangCompileResponse:
    try:
        with socket.create_connection(("sandbox", 4000)) as s:
            s.sendall(json.dumps(payload).encode())
            response = s.recv(4000)
        response_data = json.loads(response.decode())
        return ErlangTestResponse.model_validate(response_data) if 'cases' in payload else ErlangCompileResponse.model_validate(response_data)
    except json.JSONDecodeError:
        raise RuntimeError("Received invalid JSON from Erlang sandbox")
    except Exception as e:
        raise RuntimeError(f"Error communicating with Erlang sandbox: {e}")

def compile_erlang_code(payload : ErlangPayload) -> ErlangCompileResponse:
    return send_data_sandbox(payload.model_dump(by_alias=True))

async def test_code_erlang(db : AsyncSession, source_code : str, exercise_id : int) -> ErlangTestResponse:
    exercise = await exercise_crud.get_exercise(db, exercise_id)
    if not exercise:
        raise ValueError(f"Exercise with ID {exercise_id} not found")
    test_cases = exercise.test_cases

    payload = ErlangTestPayload(code=source_code, cases=test_cases)
    return send_data_sandbox(payload.model_dump(by_alias=True))

async def submit_code_erlang(db: AsyncSession, submission: SubmissionCreate) -> ErlangTestResponse:
    existing_submission = await submission_crud.get_submissions_by_user_exercise(db, submission.exercise_id, submission.user_id)
    results = await test_code_erlang(db, submission.code_snippet, submission.exercise_id)
    if existing_submission and results.test_results.failures == 0 and results.status == "ok":
        submission_created = await submission_crud.create_submission(db, submission)
        if not submission_created:
            raise ValueError("Failed to create submission")
    return results