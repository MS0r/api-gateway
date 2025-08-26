from typing import List
from sqlalchemy import select

from sqlalchemy.ext.asyncio import AsyncSession
from app.models.domain.quiz_pass import QuizPass
from app.models.domain.unit import Unit
from app.models.domain.subunit import Subunit
from app.models.domain.quiz import Quiz
from app.models.schemas.quiz_pass import QuizPassCreate

async def create_quiz_pass(db: AsyncSession, quiz_pass_create: QuizPassCreate) -> QuizPass:
    quiz_pass = QuizPass(**quiz_pass_create.model_dump(by_alias=True))
    db.add(quiz_pass)
    await db.commit()
    await db.refresh(quiz_pass)
    return quiz_pass

async def get_quiz_pass(db: AsyncSession, quiz_pass_id: int) -> QuizPass | None:
    quiz_pass = await db.get(QuizPass, quiz_pass_id)
    return quiz_pass if quiz_pass else None

async def get_quiz_passes_by_user_quiz(db: AsyncSession, user_id: int, quiz_id: int) -> QuizPass | None:
    quiz_pass = await db.execute(
        select(QuizPass).where(QuizPass.user_id == user_id, QuizPass.quiz_id == quiz_id)
    )
    return quiz_pass.scalar_one_or_none()

async def get_quiz_passes_by_user(db: AsyncSession, user_id :  int) -> List[QuizPass]:
    quiz_passes = await db.execute(
        select(QuizPass).where(QuizPass.user_id == user_id)
    )
    return quiz_passes.scalars().all()

async def get_quiz_passes_from_user_course(db: AsyncSession, user_id: int, course_id: int) -> List[QuizPass]:
    quiz_passes = await db.execute(
        select(QuizPass)
        .join(QuizPass.quiz)
        .join(Quiz.subunit)
        .join(Subunit.unit)
        .where(Unit.course_id == course_id, QuizPass.user_id == user_id)
        .distinct(QuizPass.id)
    )
    return quiz_passes.scalars().unique().all()