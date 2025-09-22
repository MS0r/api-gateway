from typing import List
from unittest import result

from fastapi import APIRouter, Depends, HTTPException, Body
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies.auth import get_current_user_authorize
from app.api.dependencies.database import get_db_session

from app.models.domain.publication import Question, Answer
from app.models.domain.vote import VoteType
from app.models.domain.user import User
from app.models.schemas.publication import QuestionCreate, QuestionRead, AnswerCreate, AnswerRead, QuestionCreateNoID, QuestionReadSingle
from app.models.schemas.vote import VoteCreate, VoteRead

from app.db.crud import publication as publication_crud
from app.services import forum as forum_service

router = APIRouter()

@router.get("/questions",response_model=List[QuestionRead], name="forum:search_questions")
async def search_questions_route(
    s: str | None = None,
    db: AsyncSession = Depends(get_db_session)
) -> List[QuestionRead]:
    if s:
        questions = await publication_crud.search_questions(db, s)
    else:
        questions = await publication_crud.get_last_questions(db)
    result = []
    for question, a, u, d in questions:
        q = QuestionRead.model_validate(question)
        setattr(q, "answer_count", a)
        setattr(q, "upvote_count", u)
        setattr(q, "downvote_count", d)
        result.append(q)
    return result

@router.post("/questions", response_model=QuestionRead, name="forum:create_question")
async def create_question_route(
    question: QuestionCreateNoID = Body(..., embed=True),
    user: User = Depends(get_current_user_authorize()),
    db: AsyncSession = Depends(get_db_session)
) -> QuestionRead:
    question = await publication_crud.create_question(db, QuestionCreate(**question.model_dump(), user_id=user.id))
    return QuestionRead.model_validate(question)

@router.get("/questions/{question_id}", response_model=QuestionReadSingle, name="forum:get_question")
async def get_question_route(
    question_id: int,
    db: AsyncSession = Depends(get_db_session)
) -> QuestionReadSingle:
    try:
        return await forum_service.get_question(db, question_id,False)
    except Exception as e:
        raise HTTPException(status_code=404, detail="Question not found")
    
@router.get("/questions/view/{question_id}", response_model=QuestionReadSingle, name="forum:view_question")
async def view_question_route(
    question_id: int,
    db: AsyncSession = Depends(get_db_session)
) -> QuestionReadSingle:
    try:
        return await forum_service.get_question(db, question_id,True)
    except Exception as e:
        raise HTTPException(status_code=404, detail="Question not found")

@router.post("/questions/{question_id}",response_model=AnswerRead, name="forum:create_answer")
async def create_answer_route(
    question_id: int,
    body: str = Body(..., embed=True),
    user: User = Depends(get_current_user_authorize()),
    db: AsyncSession = Depends(get_db_session)
) -> AnswerRead:
    answer = await publication_crud.create_answer(db, AnswerCreate(body=body, user_id=user.id, question_id=question_id))
    return AnswerRead(
        id=answer.id,
        user=user.username,
        body=answer.body,
        question_id=answer.question_id,
        user_id=answer.user_id,
        created_at=answer.created_at,
        updated_at=answer.updated_at
    )

@router.get("/{question_id}/answers",response_model=List[AnswerRead], name="forum:get_answers")
async def get_answers_route(
    question_id: int,
    db: AsyncSession = Depends(get_db_session)
) -> List[AnswerRead]:
    try:
        answers = await forum_service.get_answers_by_question(db, question_id)
        return answers
    except Exception as e:
        raise HTTPException(status_code=404, detail="Answers not found")

@router.post("/vote/{question_id}", response_model=QuestionReadSingle, name="forum:vote_question")
async def vote_question_route(
    question_id: int,
    vote : VoteType = Body(..., embed=True),
    user: User = Depends(get_current_user_authorize()),
    db: AsyncSession = Depends(get_db_session)
) -> QuestionReadSingle:
    try:
        return await forum_service.vote_publication(db, VoteCreate(user_id=user.id, question_id=question_id, vote=vote))
    except Exception as e:
        raise HTTPException(status_code=403, detail="Question not found")

@router.post("/vote/answer/{answer_id}", response_model=AnswerRead, name="forum:vote_answer")
async def vote_answer_route(
    answer_id: int,
    vote : VoteType = Body(..., embed=True),
    user: User = Depends(get_current_user_authorize()),
    db: AsyncSession = Depends(get_db_session)
) -> AnswerRead:
    try:
        return await forum_service.vote_publication(db, VoteCreate(user_id=user.id, answer_id=answer_id, vote=vote))
    except Exception as e:
        raise HTTPException(status_code=403, detail="Answer not found")

@router.get("/vote/{question_id}", response_model=List[VoteRead], name="forum:get_votes_for_question")
async def get_votes_for_question_route(
    question_id: int,
    user: User = Depends(get_current_user_authorize()),
    db: AsyncSession = Depends(get_db_session)
) -> List[VoteRead]:
    try:
        votes = await publication_crud.get_all_votes_in_question(db, question_id, user.id)
        return [VoteRead.model_validate(vote) for vote in votes]
    except Exception as e:
        raise HTTPException(status_code=403, detail="Votes not found")
