import enum
from app.models.common import DateTimeMixin, IDMixin, Base
from sqlalchemy import Column, String, Enum, Integer, ForeignKey
from sqlalchemy.orm import relationship

class SubmissionStatus(enum.Enum):
    PENDING = "pending"
    COMPLETED = "completed"
    FAILED = "failed"

class Submission(DateTimeMixin, IDMixin, Base):
    __tablename__ = "submissions"
    submission_date = Column(String, nullable=False)
    result = Column(Enum(SubmissionStatus), nullable=False)
    code_snippet = Column(String, nullable=True)

    exercise_id = Column(Integer, ForeignKey("exercises.id"), nullable=False)
    user_id = Column(Integer, ForeignKey("users.id"), nullable=False)
    unit_id = Column(Integer, ForeignKey("units.id"), nullable=False)

    user = relationship("User", back_populates="submissions")
    unit = relationship("Unit", back_populates="submissions")
    exercise = relationship("Exercise", back_populates="submissions")