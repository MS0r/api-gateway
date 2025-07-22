from fastapi import APIRouter, Body, Depends, HTTPException 
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.exc import IntegrityError

from app.models.domain.user import User
from app.models.schemas.user import UserCreate, UserRead, UserLogin, UserWithToken
from app.db.events import get_db_session
from app.db.crud import user as user_service
from app.services.jwt import create_access_token_for_user
from app.core.settings.app import AppSettings
from app.core.config import get_app_settings

router = APIRouter()

@router.post("/register", response_model=UserWithToken, name="user:register")
async def create_user_route(
    user_create: UserCreate = Body(..., embed=True, alias="user"), 
    db: AsyncSession = Depends(get_db_session),
    settings : AppSettings =  Depends(get_app_settings)
) -> UserWithToken:
    try:
        user = await user_service.create_user(db, user_create)
        if not user:
            raise HTTPException(status_code=400, detail="Failed to create user")
        
        token = create_access_token_for_user(user, secret_key=settings.secret_key.get_secret_value())

        return UserWithToken(
            **user.model_dump(by_alias=True),
            token=token
        )
    
    except IntegrityError as e:
        raise HTTPException(status_code=400, detail="Username or email already exists") from e
    

@router.post("/login", response_model=UserWithToken, name="user:login")
async def login_user_route(
    user_login: UserLogin = Body(..., embed=True, alias="user"), 
    db: AsyncSession = Depends(get_db_session),
    settings: AppSettings = Depends(get_app_settings)
) -> UserWithToken:
    user = await user_service.get_user_by_username(db, user_login.username) | await user_service.get_user_by_email(db, user_login.username)
    
    if not user or not user.verify_password(user_login.password):
        raise HTTPException(status_code=401, detail="Invalid username or password")
    
    jwt = create_access_token_for_user(user, secret_key=settings.secret_key.get_secret_value())

    return UserWithToken(
        **user.model_dump(by_alias=True),
        token=jwt
    )
