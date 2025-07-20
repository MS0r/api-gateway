from app.models.common import DateTimeMixin, IDMixin, Base
from sqlalchemy import Column, String, Enum, Integer, ForeignKey
from sqlalchemy.orm import relationship

class Quiz(DateTimeMixin, IDMixin, Base):
    __tablename__ = "quizzes"
    title = Column(String, nullable=False)
    description = Column(String, nullable=True)
    correct_answers = Column(String, nullable=True)

    subunit_id = Column(Integer, ForeignKey("subunits.id"), nullable=False)
    subunit = relationship("Subunit", back_populates="quiz", cascade="all, delete-orphan")
    quiz_answers = relationship("Quiz_Answers", back_populates="quiz", cascade="all, delete-orphan")