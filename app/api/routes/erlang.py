from fastapi import APIRouter, Depends, HTTPException, Body

from app.models.schemas.erlang import ErlangPayload, ErlangCompileResponse
from app.models.schemas.exercise import ExerciseRead
from app.services import erlang as erlang_service

router = APIRouter()

@router.post("/compile", response_model=ErlangCompileResponse, name="erlang:compile")
async def compile_erlang_code(
    payload: ErlangPayload = Body(..., embed=True, alias="erlang_payload")
) -> ErlangCompileResponse:
    # Here you would add the logic to compile the Erlang code
    result = await erlang_service.compile_erlang_code(payload)
    return ErlangCompileResponse(result="Compilation successful")

@router.get("/health", name="erlang:health")
async def health_check() -> dict:
    return {"status": "ok", "message": "Erlang service is running"}

@router.get("/test/{exercise_id}", name="erlang:test")
async def test_erlang_code(
    exercise_id: int,
    payload: ErlangPayload = Body(..., embed=True, alias="erlang_payload")
) -> dict:
    return {"status": "ok", "message": f"Testing Erlang code for exercise {exercise_id}"}