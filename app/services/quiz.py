from sqlalchemy.ext.asyncio import AsyncSession
from app.models.domain.quiz import Quiz
from app.models.schemas.quiz import (QuizCreate, QuizUpdate, QuizRead)

async def create_quiz(db: AsyncSession, quiz_create: QuizCreate) -> Quiz:
    quiz = Quiz(**quiz_create.model_dump(by_alias=True))
    db.add(quiz)
    await db.commit()
    await db.refresh(quiz)
    return quiz

async def get_quiz(db: AsyncSession, quiz_id: int) -> QuizRead | None:
    quiz = await db.get(Quiz, quiz_id)
    if quiz:
        return QuizRead.model_validate(quiz)
    return None

async def update_quiz(db: AsyncSession, quiz_id: int, quiz_update: QuizUpdate) -> QuizRead | None:
    quiz = await db.get(Quiz, quiz_id)
    if not quiz:
        return None
    for key, value in quiz_update.model_dump(exclude_unset=True).items():
        setattr(quiz, key, value)
    await db.commit()
    await db.refresh(quiz)
    return QuizRead.model_validate(quiz)