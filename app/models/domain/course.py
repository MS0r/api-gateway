import enum

from app.models.common import DateTimeMixin, IDMixin, Base
from sqlalchemy import Column, String, Enum, Integer, ForeignKey
from sqlalchemy.orm import relationship
from app.models.domain.user import User

class EnrollmentStatus(enum.Enum):
    ENROLLED = "enrolled"
    COMPLETED = "completed"
    DROPPED = "dropped"

class Course(DateTimeMixin, IDMixin, Base):
    __tablename__ = "courses"
    title = Column(String, nullable=False)
    description = Column(String, nullable=False)

    units = relationship("Unit", back_populates="course", cascade="all, delete-orphan")
    enrollments = relationship("Enrollment", back_populates="course", cascade="all, delete-orphan")

class Enrollment(DateTimeMixin, IDMixin, Base):
    __tablename__ = "enrollments"
    
    user_id = Column(Integer, ForeignKey("users.id"), nullable=False)
    course_id = Column(Integer, ForeignKey("courses.id"), nullable=False)
    
    user = relationship("User", back_populates="enrollment")
    course = relationship("Course", back_populates="enrollment")

    status = Column(Enum(EnrollmentStatus), nullable=False)