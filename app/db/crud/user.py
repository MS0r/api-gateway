from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.exc import IntegrityError
from app.models.domain.user import User
from app.models.schemas.user import (UserCreate, UserUpdate, UserRead, UserLogin)
from app.db.errors import EntityDoesNotExist
import bcrypt

def hash_password(password: str) -> bytes:
    return bcrypt.hashpw(password.encode('utf-8'), bcrypt.gensalt())

async def create_user(db: AsyncSession, user_create: UserCreate) -> UserRead:
    user_create.password = hash_password(user_create.password)
    user = User(**user_create.model_dump(by_alias=True))
    try:
        db.add(user)
        await db.commit()
        await db.refresh(user)
        return UserRead.model_validate(user)
    except IntegrityError as e:
        await db.rollback()
        raise e

async def get_user(db: AsyncSession, user_id: int) -> UserRead | None:
    user = await db.get(User, user_id)
    if user:
        return UserRead.model_validate(user)
    
    raise EntityDoesNotExist(f"User with id {user_id} does not exist")

async def update_user(db: AsyncSession, user_id: int, user_update: UserUpdate) -> UserRead | None:
    user = await db.get(User, user_id)
    if not user:
        raise EntityDoesNotExist(f"User with id {user_id} does not exist")
    for key, value in user_update.model_dump(exclude_unset=True).items():
        setattr(user, key, value)
    await db.commit()
    await db.refresh(user)
    return UserRead.model_validate(user)

async def get_user_by_username(db: AsyncSession, username: str) -> User | None:
    user = await db.execute(
        select(User).where(User.username == username)
    )
    return user.scalar_one_or_none() if user else None  

async def get_user_by_email(db: AsyncSession, email: str) -> User | None:
    user = await db.execute(
        select(User).where(User.email == email)
    )
    return user.scalar_one_or_none() if user else None  