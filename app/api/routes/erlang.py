from fastapi import APIRouter, Depends, HTTPException, Body
from sqlalchemy.ext.asyncio import AsyncSession

from app.models.schemas.erlang import ErlangPayload, ErlangCompileResponse, ErlangTestResponse, ErlangTestPayload
from app.api.dependencies.database import get_db_session
from app.services import erlang as erlang_service

router = APIRouter()

@router.post("/compile", response_model=ErlangCompileResponse, name="erlang:compile")
async def compile_erlang_code(
    payload: ErlangPayload = Body(..., embed=True, alias="erlang_payload")
) -> ErlangCompileResponse:
    try:
        result = erlang_service.compile_erlang_code(payload)
        return result
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.get("/health", name="erlang:health")
async def health_check() -> dict:
    return {"status": "ok", "message": "Erlang service is running"}

# @router.post("/test/{exercise_id}", response_model=ErlangTestResponse, name="erlang:test")
# async def test_erlang_code(
#     exercise_id: int,
#     db : AsyncSession = Depends(get_db_session),
#     payload: ErlangTestPayload = Body(..., embed=True, alias="erlang_payload")
# ) -> ErlangTestResponse:
#     try:
#         result = await erlang_service.test_code_erlang(db, payload, exercise_id)
#         return result
#     except Exception as e:
#         raise HTTPException(status_code=500, detail=str(e))

