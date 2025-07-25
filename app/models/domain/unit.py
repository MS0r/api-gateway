from app.models.domain.course import Course
from sqlalchemy import Column, String, Enum, Integer, ForeignKey
from sqlalchemy.orm import relationship
from sqlalchemy.dialects.postgresql import ARRAY
from app.models.common import DateTimeMixin, IDMixin, Base

class Unit(DateTimeMixin, IDMixin, Base):
    __tablename__ = "units"
    title = Column(String, nullable=False)
    description = Column(String, nullable=False)
    course_id = Column(Integer, ForeignKey("courses.id"), nullable=False)

    course = relationship("Course", back_populates="units")
    exercise = relationship("Exercise",uselist=False, back_populates="unit")
    subunits = relationship("Subunit", back_populates="unit", cascade="all, delete-orphan")

    order = Column(Integer, nullable=True)