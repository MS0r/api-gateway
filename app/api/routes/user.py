from typing import List

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies.auth import get_current_user_authorize
from app.api.dependencies.database import get_db_session
from app.core.config import get_app_settings
from app.core.settings.app import AppSettings
from app.models.domain.user import User

from app.models.schemas.user import UserWithToken, UserRead
from app.models.schemas.progress import ProgressSchema
from app.models.schemas.course import EnrollmentRead
from app.models.schemas.submission import SubmissionRead

from app.db.crud import course as course_crud
from app.services import jwt
from app.services.user import UserService
from app.services.base import NotFoundError

router = APIRouter()


def get_user_service(db: AsyncSession = Depends(get_db_session)) -> UserService:
    return UserService(db)


@router.get("", response_model=UserRead, name="user:get_current_user")
async def get_current_user_route(
    current_user: User = Depends(get_current_user_authorize()),
    settings: AppSettings = Depends(get_app_settings),
) -> UserRead:
    token = jwt.create_access_token_for_user(
        current_user, settings.secret_key.get_secret_value()
    )

    # Create a dict to include the token
    user_data = UserRead.model_validate(current_user).model_dump(by_alias=True)
    user_data["token"] = token

    return UserRead.model_validate(user_data)


@router.get(
    "/progress/{course_id}", response_model=ProgressSchema, name="user:course_progress"
)
async def get_course_progress_route(
    course_id: int,
    current_user: User = Depends(get_current_user_authorize()),
    service: UserService = Depends(get_user_service),
) -> ProgressSchema:
    result = await service.get_user_progress(current_user.id, course_id)

    if result.is_fail():
        if isinstance(result.error, NotFoundError):
            raise HTTPException(status_code=404, detail=result.error.message)
        raise HTTPException(status_code=500, detail=result.error.message)

    return result.data


@router.get(
    "/enrollments", response_model=List[EnrollmentRead], name="user:enrollments"
)
async def get_user_enrollments_route(
    user: User = Depends(get_current_user_authorize()),
    db: AsyncSession = Depends(get_db_session),
) -> List[EnrollmentRead]:
    enrollments = await course_crud.get_user_enrollments(db, user.id)
    return [EnrollmentRead.model_validate(enrollment) for enrollment in enrollments]


@router.get(
    "/submissions",
    response_model=List[SubmissionRead],
    name="user:get_user_submissions",
)
async def get_user_submissions_route(
    user: User = Depends(get_current_user_authorize()),
) -> List[SubmissionRead]:
    if not user.submissions:
        raise HTTPException(
            status_code=404, detail="No submissions found for this user"
        )

    return [
        SubmissionRead.model_validate(submission) for submission in user.submissions
    ]
