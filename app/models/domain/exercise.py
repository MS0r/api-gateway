from app.models.common import DateTimeMixin, IDMixin, Base
from sqlalchemy import Column, String, Enum, Integer, ForeignKey
from sqlalchemy.orm import relationship

class Exercise(DateTimeMixin, IDMixin, Base):
    __tablename__ = "exercises"
    title = Column(String, nullable=False)
    description = Column(String, nullable=True)
    exercise_schema = Column(String, nullable=False)
    test_cases = Column(String, nullable=False)

    unit_id = Column(Integer, ForeignKey("units.id"), nullable=False)

    unit = relationship("Unit", back_populates="exercise")
    submissions = relationship("Submission", back_populates="exercise", cascade="all, delete-orphan")
