from typing import List
from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession
from app.models.domain.quiz_answers import QuizAnswers
from app.models.schemas.quiz_answers import (QuizAnswersCreate, QuizAnswersUpdate, QuizAnswersRead)

async def create_quiz_answers(db: AsyncSession, quiz_answers_create: QuizAnswersCreate) -> QuizAnswers:
    quiz_answers = QuizAnswers(**quiz_answers_create.model_dump(by_alias=True))
    db.add(quiz_answers)
    await db.commit()
    await db.refresh(quiz_answers)
    return quiz_answers

async def get_quiz_answers(db: AsyncSession, quiz_answers_id: int) -> QuizAnswersRead | None:
    quiz_answers = await db.get(QuizAnswers, quiz_answers_id)
    if quiz_answers:
        return QuizAnswersRead.model_validate(quiz_answers)
    return None

async def get_quiz_answers_from_user_course(db: AsyncSession, user_id: int, course_id: int) -> List[QuizAnswersRead]:
    quiz_answers = await db.execute(
        select(QuizAnswers).where(QuizAnswers.user_id == user_id, QuizAnswers.course_id == course_id)
    )
    return [QuizAnswersRead.model_validate(q) for q in quiz_answers.scalars().all()]

async def update_quiz_answers(db: AsyncSession, quiz_answers_id: int, quiz_answers_update: QuizAnswersUpdate) -> QuizAnswersRead | None:
    quiz_answers = await db.get(QuizAnswers, quiz_answers_id)
    if not quiz_answers:
        return None
    for key, value in quiz_answers_update.model_dump(exclude_unset=True).items():
        setattr(quiz_answers, key, value)
    await db.commit()
    await db.refresh(quiz_answers)
    return QuizAnswersRead.model_validate(quiz_answers)