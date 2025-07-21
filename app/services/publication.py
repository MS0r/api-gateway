from sqlalchemy.ext.asyncio import AsyncSession
from app.models.domain.publication import Question, Answer
from app.models.schemas.publication import (QuestionCreate, QuestionUpdate, QuestionRead, AnswerCreate, AnswerRead)

async def create_question(db: AsyncSession, question_create: QuestionCreate) -> Question:
    question = Question(**question_create.model_dump(by_alias=True))
    db.add(question)
    await db.commit()
    await db.refresh(question)
    return question

async def get_question(db: AsyncSession, question_id: int) -> QuestionRead | None:
    question = await db.get(Question, question_id)
    if question:
        return QuestionRead.model_validate(question)
    return None

async def update_question(db: AsyncSession, question_id: int, question_update: QuestionUpdate) -> QuestionRead | None:
    question = await db.get(Question, question_id)
    if not question:
        return None
    for key, value in question_update.model_dump(exclude_unset=True).items():
        setattr(question, key, value)
    await db.commit()
    await db.refresh(question)
    return QuestionRead.model_validate(question)

async def create_answer(db: AsyncSession, answer_create: AnswerCreate) -> AnswerRead:
    answer = Answer(**answer_create.model_dump(by_alias=True))
    db.add(answer)
    await db.commit()
    await db.refresh(answer)
    return AnswerRead.model_validate(answer)

async def get_answer(db: AsyncSession, answer_id: int) -> AnswerRead | None:
    answer = await db.get(Answer, answer_id)
    if answer:
        return AnswerRead.model_validate(answer)
    return None

async def update_answer(db: AsyncSession, answer_id: int, answer_update: AnswerCreate) -> AnswerRead | None:    
    answer = await db.get(Answer, answer_id)
    if not answer:
        return None
    for key, value in answer_update.model_dump(exclude_unset=True).items():
        setattr(answer, key, value)
    await db.commit()
    await db.refresh(answer)
    return AnswerRead.model_validate(answer)
