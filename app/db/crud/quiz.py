from typing import List, Optional

from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select

from app.models.domain.quiz import Quiz
from app.models.domain.subunit import Subunit
from app.models.domain.unit import Unit
from app.models.schemas.quiz import (QuizCreate, QuizUpdate)

async def create_quiz(db: AsyncSession, quiz_create: QuizCreate) -> Quiz:
    quiz = Quiz(**quiz_create.model_dump(by_alias=True))
    db.add(quiz)
    await db.commit()
    await db.refresh(quiz)
    return quiz

async def get_quiz(db: AsyncSession, quiz_id: int) -> Quiz | None:
    quiz = await db.get(Quiz, quiz_id)
    return quiz if quiz else None

async def update_quiz(db: AsyncSession, quiz_id: int, quiz_update: QuizUpdate) -> Quiz | None:
    quiz = await db.get(Quiz, quiz_id)
    if not quiz:
        return None
    for key, value in quiz_update.model_dump(exclude_unset=True).items():
        setattr(quiz, key, value)
    await db.commit()
    await db.refresh(quiz)
    return quiz

async def get_course_quizzes(db: AsyncSession, course_id: int) -> List[Quiz | None]:
    quizzes = await db.execute(
        select(Quiz).join(Quiz.subunit).join(Subunit.unit).where(Unit.course_id == course_id)
    )
    return [quiz for quiz in quizzes.scalars().all()]

async def get_subunit_by_quiz(db: AsyncSession, quiz_id: int) -> Subunit | None:
    subunit = await db.execute(
        select(Subunit).join(Subunit.quizzes).where(Quiz.id == quiz_id)
    )
    return subunit.scalar_one_or_none()