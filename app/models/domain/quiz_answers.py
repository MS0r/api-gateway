from app.models.common import DateTimeMixin, IDMixin, Base
from sqlalchemy import Column, String, Enum, Integer, ForeignKey, Boolean
from sqlalchemy.orm import relationship

class Quiz_Answers(DateTimeMixin, IDMixin, Base):
    __tablename__ = "quiz_user"
    answers = Column(String, nullable=False)
    is_correct = Column(Boolean, nullable=False)

    user_id = Column(Integer, ForeignKey("users.id"), nullable=False)
    quiz_id = Column(Integer, ForeignKey("quizzes.id"), nullable=False)
    subunit_id = Column(Integer, ForeignKey("subunits.id"), nullable=False)

    user = relationship("User", back_populates="quiz_answers")
    quiz = relationship("Quiz", back_populates="quiz_answers")
    subunit = relationship("Subunit", back_populates="quiz_answers")