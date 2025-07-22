from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel
from app.models.domain.user import (UserRole, UserStatus)

class UserCreate(BaseModel):
    username: str
    email: str
    password: str
    role: UserRole  = UserRole.USER
    status: UserStatus = UserStatus.ACTIVE

class UserUpdate(BaseModel):
    username: str | None = None
    email: str | None = None
    password: str | None = None
    role: UserRole | None = None
    status: UserStatus | None = None

class UserLogin(BaseModel):
    username: str
    password: str

class UserRead(RWModel):
    _id: int
    username: str
    email: str
    role: UserRole
    status: UserStatus

class UserWithToken(UserRead):
    token: str

