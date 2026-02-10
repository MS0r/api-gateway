from typing import List

from fastapi import APIRouter, Depends, HTTPException, Body
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies.auth import get_current_user_authorize
from app.api.dependencies.database import get_db_session

from app.models.domain.user import User
from app.models.domain.vote import VoteType
from app.models.schemas.publication import (
    QuestionRead,
    AnswerRead,
    QuestionCreateNoID,
    QuestionReadSingle,
)
from app.models.schemas.vote import VoteCreate, VoteRead

from app.services.forum import ForumService
from app.services.base import NotFoundError, ValidationError

router = APIRouter()


def get_forum_service(db: AsyncSession = Depends(get_db_session)) -> ForumService:
    return ForumService(db)

@router.get(
    "/questions", response_model=List[QuestionRead], name="forum:search_questions"
)
async def search_questions_route(
    s: str | None = None, service: ForumService = Depends(get_forum_service)
) -> List[QuestionRead]:
    if s:
        result = await service.search_questions(s)
    else:
        result = await service.get_questions_list()

    if result.is_fail():
        raise HTTPException(status_code=500, detail=result.error.message)

    return result.data

@router.post("/questions", response_model=QuestionRead, name="forum:create_question")
async def create_question_route(
    question: QuestionCreateNoID,
    user: User = Depends(get_current_user_authorize()),
    service: ForumService = Depends(get_forum_service),
) -> QuestionRead:
    result = await service.create_question(question, user.id)

    if result.is_fail():
        if isinstance(result.error, ValidationError):
            raise HTTPException(status_code=400, detail=result.error.message)
        raise HTTPException(status_code=500, detail=result.error.message)

    return result.data

@router.get(
    "/questions/{question_id}",
    response_model=QuestionReadSingle,
    name="forum:get_question",
)
async def get_question_route(
    question_id: int, service: ForumService = Depends(get_forum_service)
) -> QuestionReadSingle:
    result = await service.get_question(question_id, increment_view=False)

    if result.is_fail():
        if isinstance(result.error, NotFoundError):
            raise HTTPException(status_code=404, detail=result.error.message)
        raise HTTPException(status_code=500, detail=result.error.message)

    return result.data

@router.get(
    "/questions/view/{question_id}",
    response_model=QuestionReadSingle,
    name="forum:view_question",
)
async def view_question_route(
    question_id: int, service: ForumService = Depends(get_forum_service)
) -> QuestionReadSingle:
    result = await service.get_question(question_id, increment_view=True)

    if result.is_fail():
        if isinstance(result.error, NotFoundError):
            raise HTTPException(status_code=404, detail=result.error.message)
        raise HTTPException(status_code=500, detail=result.error.message)

    return result.data

@router.post(
    "/questions/{question_id}", response_model=AnswerRead, name="forum:create_answer"
)
async def create_answer_route(
    question_id: int,
    body: str = Body(..., embed=True),
    user: User = Depends(get_current_user_authorize()),
    service: ForumService = Depends(get_forum_service),
) -> AnswerRead:
    from app.models.schemas.publication import AnswerCreate

    answer_data = AnswerCreate(body=body, user_id=user.id, question_id=question_id)

    result = await service.create_answer(answer_data, user.id)

    if result.is_fail():
        if isinstance(result.error, NotFoundError):
            raise HTTPException(status_code=404, detail=result.error.message)
        raise HTTPException(status_code=500, detail=result.error.message)

    return result.data

@router.get(
    "/{question_id}/answers", response_model=List[AnswerRead], name="forum:get_answers"
)
async def get_answers_route(
    question_id: int, service: ForumService = Depends(get_forum_service)
) -> List[AnswerRead]:
    # First verify question exists
    question_result = await service.get_question(question_id, increment_view=False)
    if question_result.is_fail():
        if isinstance(question_result.error, NotFoundError):
            raise HTTPException(status_code=404, detail=question_result.error.message)
        raise HTTPException(status_code=500, detail=question_result.error.message)

    answers = await service._get_answers_with_votes(question_id)
    return answers

@router.post(
    "/vote/{question_id}", response_model=QuestionReadSingle, name="forum:vote_question"
)
async def vote_question_route(
    question_id: int,
    vote: VoteType = Body(..., embed=True),
    user: User = Depends(get_current_user_authorize()),
    service: ForumService = Depends(get_forum_service),
) -> QuestionReadSingle:
    vote_data = VoteCreate(user_id=user.id, question_id=question_id, vote=vote)

    result = await service.vote(vote_data)

    if result.is_fail():
        if isinstance(result.error, NotFoundError):
            raise HTTPException(status_code=404, detail=result.error.message)
        elif isinstance(result.error, ValidationError):
            raise HTTPException(status_code=400, detail=result.error.message)
        raise HTTPException(status_code=500, detail=result.error.message)

    # Result should be QuestionReadSingle for question votes
    return result.data

@router.post(
    "/vote/answer/{answer_id}", response_model=AnswerRead, name="forum:vote_answer"
)
async def vote_answer_route(
    answer_id: int,
    vote: VoteType = Body(..., embed=True),
    user: User = Depends(get_current_user_authorize()),
    service: ForumService = Depends(get_forum_service),
) -> AnswerRead:
    vote_data = VoteCreate(user_id=user.id, answer_id=answer_id, vote=vote)

    result = await service.vote(vote_data)

    if result.is_fail():
        if isinstance(result.error, NotFoundError):
            raise HTTPException(status_code=404, detail=result.error.message)
        elif isinstance(result.error, ValidationError):
            raise HTTPException(status_code=400, detail=result.error.message)
        raise HTTPException(status_code=500, detail=result.error.message)

    return result.data

@router.get(
    "/vote/{question_id}",
    response_model=List[VoteRead],
    name="forum:get_votes_for_question",
)
async def get_votes_for_question_route(
    question_id: int,
    user: User = Depends(get_current_user_authorize()),
    db: AsyncSession = Depends(get_db_session),
) -> List[VoteRead]:
    from app.db.crud import publication as publication_crud

    try:
        votes = await publication_crud.get_all_votes_in_question(
            db, question_id, user.id
        )
        return [VoteRead.model_validate(vote) for vote in votes]
    except Exception as e:
        raise HTTPException(
            status_code=500, detail=f"Failed to retrieve votes: {str(e)}"
        )
