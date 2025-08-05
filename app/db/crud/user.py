from typing import List, Optional

from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.exc import IntegrityError

from app.models.domain.user import User
from app.models.schemas.user import (UserCreate, UserUpdate)
from app.db.errors import EntityDoesNotExist


async def create_user(db: AsyncSession, user_create: UserCreate) -> User:
    user_data = user_create.model_dump(by_alias=True)
    user = User(**user_data)
    try:
        db.add(user)
        await db.commit()
        await db.refresh(user)
        return user
    except IntegrityError as e:
        await db.rollback()
        raise e

async def get_user(db: AsyncSession, user_id: int) -> User | None:
    user = await db.get(User, user_id)
    if user:
        return user
    raise EntityDoesNotExist(f"User with id {user_id} does not exist")

async def update_user(db: AsyncSession, user_id: int, user_update: UserUpdate) -> User | None:
    user = await db.get(User, user_id)
    if not user:
        raise EntityDoesNotExist(f"User with id {user_id} does not exist")
    for key, value in user_update.model_dump(exclude_unset=True).items():
        setattr(user, key, value)
    await db.commit()
    await db.refresh(user)
    return user

async def get_user_by_username(db: AsyncSession, username: str) -> User | None:
    user = await db.execute(
        select(User).where(User.username == username)
    )
    return user.scalar_one_or_none()

async def get_user_by_email(db: AsyncSession, email: str) -> User | None:
    user = await db.execute(
        select(User).where(User.email == email)
    )
    return user.scalar_one_or_none() 
