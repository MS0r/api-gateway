from typing import List, Tuple
from sqlalchemy import select, func, case, distinct, or_, update
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.orm import selectinload
from app.models.domain.publication import Question, Answer
from app.models.domain.vote import Vote, VoteType
from app.models.schemas.publication import (QuestionCreate, QuestionUpdate, AnswerCreate)
from app.models.schemas.vote import VoteCreate, VoteUpdate

async def create_question(db: AsyncSession, question_create: QuestionCreate) -> Question:
    question = Question(**question_create.model_dump(by_alias=True))
    db.add(question)
    await db.commit()
    await db.refresh(question)
    return question

async def get_question(db: AsyncSession, question_id: int, view : bool) -> Tuple[Question, int, int] | None:
    if view:
        await db.execute(
            update(Question).where(Question.id == question_id).values(views=Question.views + 1)
        )
    question = await db.execute(
        select(
            Question,
            func.count(distinct(case((Vote.vote == VoteType.UPVOTE and Vote.question_id == Question.id, Vote.id)))).label("upvote_count"),
            func.count(distinct(case((Vote.vote == VoteType.DOWNVOTE and Vote.question_id == Question.id, Vote.id)))).label("downvote_count")
        )
        .outerjoin(Question.votes)
        .where(Question.id == question_id)
        .group_by(Question.id)
        .options(selectinload(Question.user), selectinload(Question.answers).selectinload(Answer.user))
    )
    return question.first()

async def get_questions_from_user(db: AsyncSession, user_id: int) -> List[Tuple[Question, int, int, int]]:
    questions = await db.execute(
        select(Question).where(Question.user_id == user_id)
    )
    return questions.scalars().all()

async def get_last_questions(db: AsyncSession) -> List[Tuple[Question, int, int, int]]:
    questions = await db.execute(
        select(
            Question,
            func.count(distinct(Answer.id)).label("answer_count"),
            func.count(distinct(case((Vote.vote == VoteType.UPVOTE and Vote.question_id == Question.id, Vote.id)))).label("upvote_count"),
            func.count(distinct(case((Vote.vote == VoteType.DOWNVOTE and Vote.question_id == Question.id, Vote.id)))).label("downvote_count")
        )
        .outerjoin(Question.answers)
        .outerjoin(Question.votes)
        .group_by(Question.id)
        .order_by(Question.created_at.desc())
        .limit(5)
    )
    return questions.all()

async def search_questions(db: AsyncSession, search: str) -> List[Question]:
    questions = await db.execute(
        select(
            Question,
            func.count(distinct(Answer.id)).label("answer_count"),
            func.count(distinct(case((Vote.vote == VoteType.UPVOTE and Vote.question_id == Question.id, Vote.id)))).label("upvote_count"),
            func.count(distinct(case((Vote.vote == VoteType.DOWNVOTE and Vote.question_id == Question.id, Vote.id)))).label("downvote_count")
        )
        .outerjoin(Question.answers)
        .outerjoin(Question.votes)
        .where(Question.title.ilike(f"%{search}%"), Question.body.ilike(f"%{search}%"), or_(*[tag.ilike(f"%{search}%") for tag in Question.tags]))
        .group_by(Question.id)
    )
    return questions.all()

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
