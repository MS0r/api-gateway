from pydantic import BaseModel
from typing import List
from app.models.schemas.rwmodel import RWModel
from app.models.domain.user import (UserRole, UserStatus)
from app.models.schemas.submission import SubmissionRead
from app.models.schemas.course import EnrollmentRead
from app.models.schemas.quiz_pass import QuizPassRead

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

class UserAuthRead(RWModel):
    username: str
    email: str
    status: UserStatus
    role : UserRole

class UserWithToken(UserAuthRead):
    token: str = None

class UserRead(UserWithToken):
    quiz_passes: List[QuizPassRead] | None = None
    enrollments: List[EnrollmentRead] | None = None
    submissions: List[SubmissionRead] | None = None


