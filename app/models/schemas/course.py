from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel
from typing_extensions import Self
from app.models.domain.course import EnrollmentStatus

class CourseCreate(BaseModel):
    title: str
    description: str

class CourseUpdate(BaseModel):
    title: str | None = None
    description: str | None = None

class CourseRead(RWModel):
    _id: int
    title: str
    description: str
    units: list[Self] | None = None

class CourseEnrollment(BaseModel):
    user_id: int
    course_id: int
    status: EnrollmentStatus = EnrollmentStatus.ENROLLED

class CourseEnrollmentRead(RWModel):
    user_id: int
    course_id: int
    status: EnrollmentStatus


