from typing import List
from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession
from app.models.domain.publication import Question, Answer
from app.models.domain.vote import Vote
from app.models.schemas.publication import (QuestionCreate, QuestionUpdate, AnswerCreate)
from app.models.schemas.vote import VoteCreate, VoteUpdate

async def create_question(db: AsyncSession, question_create: QuestionCreate) -> Question:
    question = Question(**question_create.model_dump(by_alias=True))
    db.add(question)
    await db.commit()
    await db.refresh(question)
    return question

async def get_question(db: AsyncSession, question_id: int) -> Question | None:
    question = await db.get(Question, question_id)
    return question if question else None

async def get_questions_from_user(db: AsyncSession, user_id: int) -> List[Question]:
    questions = await db.execute(
        select(Question).where(Question.user_id == user_id)
    )
    return questions.scalars().all()

async def get_last_questions(db: AsyncSession) -> List[Question]:
    questions = await db.execute(
        select(Question).order_by(Question.created_at.desc()).limit(5)
    )
    return questions.scalars().all()

async def update_question(db: AsyncSession, question_id: int, question_update: QuestionUpdate) -> Question | None:
    question = await db.get(Question, question_id)
    if not question:
        return None
    for key, value in question_update.model_dump(exclude_unset=True).items():
        setattr(question, key, value)
    await db.commit()
    await db.refresh(question)
    return question

async def create_answer(db: AsyncSession, answer_create: AnswerCreate) -> Answer:
    answer = Answer(**answer_create.model_dump(by_alias=True))
    db.add(answer)
    await db.commit()
    await db.refresh(answer)
    return answer

async def get_answer(db: AsyncSession, answer_id: int) -> Answer | None:
    answer = await db.get(Answer, answer_id)
    return answer if answer else None

async def get_answers_for_question(db: AsyncSession, question_id: int) -> List[Answer]:
    answers = await db.execute(
        select(Answer).where(Answer.question_id == question_id)
    )
    return answers.scalars().all()

async def get_answers_from_user(db: AsyncSession, user_id: int) -> List[Answer]:
    answers = await db.execute(
        select(Answer).where(Answer.user_id == user_id)
    )
    return answers.scalars().all()

async def update_answer(db: AsyncSession, answer_id: int, answer_update: AnswerCreate) -> Answer | None:
    answer = await db.get(Answer, answer_id)
    if not answer:
        return None
    for key, value in answer_update.model_dump(exclude_unset=True).items():
        setattr(answer, key, value)
    await db.commit()
    await db.refresh(answer)
    return answer

async def get_vote_question(db: AsyncSession, question_id: int, user_id: int) -> Vote | None:
    vote = await db.execute(
        select(Vote).where(Vote.question_id == question_id, Vote.user_id == user_id)
    )
    return vote.scalars().first()

async def get_vote_answer(db: AsyncSession, answer_id: int, user_id: int) -> Vote | None:
    vote = await db.execute(
        select(Vote).where(Vote.answer_id == answer_id, Vote.user_id == user_id)
    )
    return vote.scalars().first()

async def update_vote(db: AsyncSession,vote_id : int, vote: VoteUpdate) -> Vote:
    existing_vote = await db.get(Vote, vote_id)
    if not existing_vote:
        return None
    for key, value in vote.model_dump(exclude_unset=True).items():
        setattr(existing_vote, key, value)
    await db.commit()
    await db.refresh(existing_vote)
    return existing_vote

async def remove_vote(db: AsyncSession, vote: Vote) -> None:
    await db.delete(vote)
    await db.commit()

async def vote(db: AsyncSession, vote : VoteCreate) -> Vote | None:
    vote = VoteCreate(**vote.model_dump(by_alias=True))
    db.add(vote)
    await db.commit()
    await db.refresh(vote)
    return vote
