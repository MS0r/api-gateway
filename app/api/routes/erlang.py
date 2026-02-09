from fastapi import APIRouter, Depends, HTTPException, Body
from sqlalchemy.ext.asyncio import AsyncSession

from app.models.schemas.erlang import (
    ErlangPayload, 
    ErlangCompileResponse, 
    ErlangTestResponse
    )
from app.services.erlang import ErlangService
from app.api.dependencies.database import get_db_session
from app.api.dependencies.erlang import get_erlang_service

router = APIRouter()

@router.post("/compile", response_model=ErlangCompileResponse, name="erlang:compile")
async def compile_erlang_code(
    payload: ErlangPayload,
    service : ErlangService = Depends(get_erlang_service)
) -> ErlangCompileResponse:
    try:
        result = await service.compile_erlang_code(payload)
        return result
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.get("/health", name="erlang:health")
async def health_check() -> dict:
    return {"status": "ok", "message": "Erlang service is running"}

@router.post("/test/{exercise_id}", response_model=ErlangTestResponse, name="erlang:test")
async def test_erlang_code(
    exercise_id: int,
    db : AsyncSession = Depends(get_db_session),
    source_code: str = Body(..., embed=True),
    service : ErlangService = Depends(get_erlang_service)
) -> ErlangTestResponse:
    try:
        result = await service._test_code_erlang(db, source_code, exercise_id)
        return result
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

