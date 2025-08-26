from fastapi import APIRouter, Body, Depends, HTTPException
from sqlalchemy.ext.asyncio import AsyncSession
from app.api.dependencies.auth import get_current_user_authorize
from app.api.dependencies.database import get_db_session

from app.models.domain.user import User
from app.models.schemas.exercise import ExerciseCreate, ExerciseRead, ExerciseUpdate
from app.models.schemas.submission import SubmissionCreate, SubmissionRead, SubmissionCreateNoID
from app.models.schemas.erlang import ErlangTestResponse


from app.db.crud import exercise as exercise_crud
from app.db.crud import submission as submission_crud
from app.services import erlang as erlang_service

router = APIRouter()

@router.post("",response_model=ExerciseRead, name="exercise:create_exercise")
async def create_exercise_route(
    exercise: ExerciseCreate = Body(..., embed=True),
    db: AsyncSession = Depends(get_db_session)
) -> ExerciseRead:
    exercise_created = await exercise_crud.create_exercise(db,exercise)
    if not exercise_created:
        raise HTTPException(status_code=400, detail="Failed to create exercise")
    return ExerciseRead.model_validate(exercise_created)

@router.get("/{exercise_id}", response_model=ExerciseRead, name="exercise:get_exercise")
async def get_exercise_route(
    exercise_id: int,
    db: AsyncSession = Depends(get_db_session)
) -> ExerciseRead:
    exercise = await exercise_crud.get_exercise(db, exercise_id)
    if not exercise:
        raise HTTPException(status_code=404, detail="Exercise not found")
    return ExerciseRead.model_validate(exercise)


@router.get("/{exercise_id}/submissions", response_model=SubmissionRead, name="exercise:get_submissions")
async def get_submissions_route(
    exercise_id: int,
    db: AsyncSession = Depends(get_db_session)
) -> SubmissionRead:
    submissions = await submission_crud.get_submissions_by_exercise_id(db, exercise_id)
    if not submissions:
        raise HTTPException(status_code=404, detail="No submissions found for this exercise")
    return [SubmissionRead.model_validate(submission) for submission in submissions]

@router.post("/{exercise_id}/submit", response_model=ErlangTestResponse, name="exercise:submit_exercise")
async def submit_exercise_route(
    exercise_id: int,
    submission: SubmissionCreateNoID = Body(..., embed=True),
    user: User = Depends(get_current_user_authorize()),
    db: AsyncSession = Depends(get_db_session)
) -> ErlangTestResponse:
    try:
        sub = SubmissionCreate(code_snippet=submission.code_snippet, user_id=user.id, exercise_id=exercise_id)
        test_response = await erlang_service.submit_code_erlang(db, sub)
        return test_response
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

@router.delete("/submission/{submission_id}",response_model=bool, name="exercise:delete_submission")
async def delete_submission_route(
    submission_id: int,
    db: AsyncSession = Depends(get_db_session)
):
    return await submission_crud.delete_submission(db, submission_id)

@router.put("/{exercise_id}", response_model=ExerciseRead, name="exercise:update_exercise")
async def update_exercise_route(
    exercise_id : int,
    db: AsyncSession = Depends(get_db_session),
    update : ExerciseUpdate =  Body(..., embed=True),
):
    exercise = await exercise_crud.update_exercise(db, exercise_id, update)
    if not exercise:
        raise HTTPException(status_code=404, detail="Exercise not found")
    return ExerciseRead.model_validate(exercise)