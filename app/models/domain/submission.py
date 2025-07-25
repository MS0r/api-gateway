import enum
from app.models.common import DateTimeMixin, IDMixin, Base
from sqlalchemy import Column, String, Enum, Integer, ForeignKey
from sqlalchemy.orm import relationship

class Submission(DateTimeMixin, IDMixin, Base):
    __tablename__ = "submissions"
    code_snippet = Column(String, nullable=True)

    exercise_id = Column(Integer, ForeignKey("exercises.id"), nullable=False)
    user_id = Column(Integer, ForeignKey("users.id"), nullable=False)

    user = relationship("User", back_populates="submissions")
    exercise = relationship("Exercise", back_populates="submissions")