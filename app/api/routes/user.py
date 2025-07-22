# Create user progress, user course enrolled, user code submission, and user quiz answers web responses
from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.ext.asyncio import AsyncSession

from app.models.domain.user import User
from app.services import progress as user_service
from app.db.events import get_db_session
from app.api.dependencies.auth import get_current_user_authorize
from app.core.config import get_app_settings
from app.core.settings.app import AppSettings
from app.models.schemas.user import UserWithToken
from app.models.schemas.progress import ProgressSchema
from app.services import jwt

router = APIRouter()

@router.get("", response_model=UserWithToken,name="user:get_current_user")
async def get_current_user(
    current_user: User = Depends(get_current_user_authorize()),
    settings: AppSettings = Depends(get_app_settings)
) -> UserWithToken:
    token = jwt.create_access_token_for_user(current_user, settings.secret_key.get_secret_value())
    
    return UserWithToken(
        _id=current_user.id,
        username=current_user.username,
        email=current_user.email,
        role=current_user.role,
        status=current_user.status,
        token=token
    )

