from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel
from app.models.domain.course import EnrollmentStatus

class CourseCreate(BaseModel):
    title: str
    description: str

class CourseUpdate(BaseModel):
    title: str | None = None
    description: str | None = None

class CourseRead(RWModel):
    title: str
    description: str

class EnrollmentCreate(BaseModel):
    user_id: int
    course_id: int
    status: EnrollmentStatus = EnrollmentStatus.ENROLLED

class EnrollmentRead(RWModel):
    user_id: int
    course_id: int
    status: EnrollmentStatus


