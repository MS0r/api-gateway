from fastapi import APIRouter, Depends, HTTPException, Body
from sqlalchemy.ext.asyncio import AsyncSession

from app.models.schemas.erlang import (
    ErlangPayload,
    ErlangCompileResponse,
    ErlangTestResponse,
)
from app.services.erlang import ErlangService
from app.api.dependencies.database import get_db_session
from app.api.dependencies.erlang import get_erlang_service
from app.services.base import NotFoundError, ValidationError

router = APIRouter()


@router.post("/compile", response_model=ErlangCompileResponse, name="erlang:compile")
async def compile_erlang_code(
    payload: ErlangPayload, service: ErlangService = Depends(get_erlang_service)
) -> ErlangCompileResponse:

    result = await service.compile_erlang_code(payload)

    if result.is_fail():
        if isinstance(result.error, ValidationError):
            raise HTTPException(status_code=400, detail=result.error.message)
        raise HTTPException(status_code=500, detail=result.error.message)

    return result.data


@router.get("/health", name="erlang:health")
async def health_check(service: ErlangService = Depends(get_erlang_service)) -> dict:
    """Health check endpoint for Erlang service."""
    return await service.health_check()


@router.post(
    "/test/{exercise_id}", response_model=ErlangTestResponse, name="erlang:test"
)
async def test_erlang_code(
    exercise_id: int,
    db: AsyncSession = Depends(get_db_session),
    source_code: str = Body(..., embed=True),
    service: ErlangService = Depends(get_erlang_service),
) -> ErlangTestResponse:

    result = await service.test_code_erlang(db, source_code, exercise_id)

    if result.is_fail():
        if isinstance(result.error, NotFoundError):
            raise HTTPException(status_code=404, detail=result.error.message)
        elif isinstance(result.error, ValidationError):
            raise HTTPException(status_code=400, detail=result.error.message)
        raise HTTPException(status_code=500, detail=result.error.message)

    return result.data
