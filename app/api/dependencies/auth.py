from typing import Callable

from fastapi import Depends, HTTPException, Security
from fastapi.security import APIKeyHeader
from sqlalchemy.ext.asyncio import AsyncSession

from starlette import requests, status
from starlette.exceptions import HTTPException as StarletteHTTPException

from app.core.settings.app import AppSettings
from app.core.config import get_app_settings
from app.services import jwt
from app.db.crud import user as user_service
from app.db.events import get_db_session
from app.db.errors import EntityDoesNotExist
from app.models.domain.user import User

HEADER_KEY = "Authorization"

class RWAPIKeyHeader(APIKeyHeader):
    async def __call__(
        self,
        request: requests.Request,
    ) -> str | None:
        try:
            return await super().__call__(request)
        except StarletteHTTPException as original_auth_exc:
            raise HTTPException(
                status_code=original_auth_exc.status_code,
                detail="Could not validate API key",
            )
        
def get_current_user_authorize(*,required: bool = True) -> Callable:
    return _get_current_user if required else _get_current_user_optional

def _get_authorization_header_retriever(
        *, required: bool
) -> Callable:
   return _get_authorization_header if required else _get_authorization_header_optional

def _get_authorization_header(
        api_key : str = Security(
            RWAPIKeyHeader(name=HEADER_KEY)
        ),
        settings: AppSettings = Depends(get_app_settings)
        ) -> str:
    try:
        token_prefix, token = api_key.split(" ")
    except ValueError:
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Wrong token"
        )
    if token_prefix != settings.jwt_token_prefix:
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="wrong token"
        )
    
    return token

def _get_authorization_header_optional(
    authorization: str | None = Security(
        RWAPIKeyHeader(name=HEADER_KEY, auto_error=False),
    ),
    settings: AppSettings = Depends(get_app_settings),
) -> str:
    if authorization:
        return _get_authorization_header(authorization, settings)

    return ""


async def _get_current_user(
    db: AsyncSession = Depends(get_db_session),
    token: str = Depends(_get_authorization_header_retriever()),
    settings: AppSettings = Depends(get_app_settings),
) -> User:
    try:
        username = jwt.get_username_from_token(
            token,
            str(settings.secret_key.get_secret_value()),
        )
    except ValueError:
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="payload is malformed",
        )

    try:
        return await user_service.get_user_by_username(db, username)
    except EntityDoesNotExist:
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="payload is malformed",
        )
    
async def _get_current_user_optional(
    db: AsyncSession = Depends(get_db_session),
    token: str = Depends(_get_authorization_header_retriever(required=False)),
    settings: AppSettings = Depends(get_app_settings),
) -> User | None:
    if token:
        return await _get_current_user(db, token, settings)

    return None