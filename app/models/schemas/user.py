from enum import Enum
from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel

class UserRole(Enum):
    USER : str = "user"
    ADMIN : str = "admin"

class UserStatus(Enum):
    ACTIVE : str = "active"
    INACTIVE : str = "inactive"
    SUSPENDED : str = "suspended"

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
    id: int
    username: str
    email: str
    role: UserRole
    status: UserStatus

class UserWithToken(UserRead):
    token: str
