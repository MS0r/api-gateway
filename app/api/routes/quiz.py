from fastapi import APIRouter, Body, Depends, HTTPException
from sqlalchemy.ext.asyncio import AsyncSession
from app.api.dependencies.auth import get_current_user_authorize
from app.api.dependencies.database import get_db_session

from app.models.domain.user import User
from app.models.schemas.quiz_pass import QuizPassCreate, QuizPassRead
from app.db.crud import quiz_pass as quiz_pass_crud

router = APIRouter()

@router.post("/{quiz_id}/success", response_model=QuizPassRead, name="quiz:submit")
async def submit_quiz(
    quiz_id: int,
    user: User = Depends(get_current_user_authorize),
    db: AsyncSession = Depends(get_db_session)
) -> QuizPassRead:
    quiz_pass = await quiz_pass_crud.create_quiz_pass(
        db, QuizPassCreate(user_id=user.id, quiz_id=quiz_id)
    )
    if not quiz_pass:
        raise HTTPException(status_code=400, detail="Failed to submit quiz")
    return QuizPassRead.model_validate(quiz_pass)
