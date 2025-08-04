# Create user progress, user course enrolled, user code submission, and user quiz answers web responses
from typing import List

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies.auth import get_current_user_authorize
from app.api.dependencies.database import get_db_session
from app.core.config import get_app_settings
from app.core.settings.app import AppSettings
from app.models.domain.user import User

from app.models.schemas.user import UserWithToken
from app.models.schemas.progress import ProgressSchema
from app.models.schemas.course import EnrollmentRead
from app.models.schemas.submission import SubmissionRead

from app.db.crud import course as course_crud
from app.services import jwt
from app.services import user as user_service

router = APIRouter()

@router.get("", response_model=UserWithToken,name="user:get_current_user")
async def get_current_user(
    current_user: User = Depends(get_current_user_authorize()),
    settings: AppSettings = Depends(get_app_settings)
) -> UserWithToken:
    token = jwt.create_access_token_for_user(current_user, settings.secret_key.get_secret_value())
    
    return UserWithToken(
        id=current_user.id,
        username=current_user.username,
        email=current_user.email,
        role=current_user.role,
        status=current_user.status,
        token=token
    )

@router.get("/progress/{course_id}", response_model=ProgressSchema, name="course:course_progress")
async def get_course_progress_route(
    course_id: int,
    db: AsyncSession = Depends(get_db_session),
    current_user: User = Depends(get_current_user_authorize())
) -> ProgressSchema:
    progress : ProgressSchema = await user_service.get_user_progress(current_user.id, course_id, db)
    if not progress:
        raise HTTPException(status_code=404, detail="No progress found for this course")
    return progress

@router.get("/enrollments", response_model=List[EnrollmentRead], name="course:enrollments")
async def get_user_enrollments_route(
    user : User = Depends(get_current_user_authorize()),
    db: AsyncSession = Depends(get_db_session)
) -> List[EnrollmentRead]:
    enrollments = await course_crud.get_user_enrollments(db, user.id)
    return [EnrollmentRead.model_validate(enrollment) for enrollment in enrollments]

@router.get("/submissions", response_model=List[SubmissionRead], name="submission:get_user_submissions")
async def get_user_submissions_route(
    user: User = Depends(get_current_user_authorize()),
) -> List[SubmissionRead]:
    if not user.submissions:
        raise HTTPException(status_code=404, detail="No submissions found for this user")
    # Convert domain model to schema model
    return [SubmissionRead.model_validate(submission) for submission in user.submissions]