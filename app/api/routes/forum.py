from typing import Callable, List

from fastapi import APIRouter, Depends, HTTPException, Body
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies.auth import get_current_user_authorize
from app.api.dependencies.database import get_db_session

from app.models.domain.publication import Question, Answer
from app.models.domain.vote import VoteType
from app.models.domain.user import User
from app.models.schemas.publication import QuestionCreate, QuestionRead, AnswerCreate, AnswerRead, QuestionCreateNoID
from app.models.schemas.vote import VoteCreate

from app.db.crud import publication as publication_crud
from app.services import forum as forum_service

router = APIRouter()

@router.get("/questions",response_class=List[QuestionRead], name="publication:get_questions")
async def get_questions_route(
    user: User = Depends(get_current_user_authorize()),
    db: AsyncSession = Depends(get_db_session)
) -> List[QuestionRead]:
    questions = await publication_crud.get_last_questions(db)
    return [QuestionRead.model_validate(question) for question in questions]

@router.post("", response_model=QuestionRead, name="publication:create_question")
async def create_question_route(
    question: QuestionCreateNoID = Body(..., embed=True),
    user: User = Depends(get_current_user_authorize()),
    db: AsyncSession = Depends(get_db_session)
) -> QuestionRead:
    question = await publication_crud.create_question(db, QuestionCreate(**question.model_dump(), user_id=user.id))
    return QuestionRead.model_validate(question)

@router.post("/{question_id}",response_class=QuestionRead, name="publication:create_answer")
async def create_answer_route(
    question_id: int,
    body: str = Body(..., embed=True),
    user: User = Depends(get_current_user_authorize()),
    db: AsyncSession = Depends(get_db_session)
) -> AnswerRead:
    answer = await publication_crud.create_answer(db, AnswerCreate(body=body, user_id=user.id, question_id=question_id))
    return AnswerRead.model_validate(answer)

@router.get("/{question_id}/answers",response_class=List[AnswerRead], name="publication:get_answers")
async def get_answers_route(
    question_id: int,
    db: AsyncSession = Depends(get_db_session)
) -> List[AnswerRead]:
    answers = await publication_crud.get_answers_for_question(db, question_id)
    return [AnswerRead.model_validate(answer) for answer in answers]

@router.post("/vote/{question_id}", response_model=QuestionRead, name="publication:vote_question")
async def vote_question_route(
    question_id: int,
    vote : VoteType = Body(..., embed=True),
    user: User = Depends(get_current_user_authorize()),
    db: AsyncSession = Depends(get_db_session)
) -> QuestionRead:
    question = await forum_service.vote_publication(db, VoteCreate(user_id=user.id, question_id=question_id, vote=vote))
    return QuestionRead.model_validate(question)

@router.post("/vote/answer/{answer_id}", response_model=AnswerRead, name="publication:vote_answer")
async def vote_answer_route(
    answer_id: int,
    vote : VoteType = Body(..., embed=True),
    user: User = Depends(get_current_user_authorize()),
    db: AsyncSession = Depends(get_db_session)
) -> AnswerRead:
    answer = await forum_service.vote_publication(db, VoteCreate(user_id=user.id, answer_id=answer_id, vote=vote))
    return AnswerRead.model_validate(answer)
