from app.models.common import DateTimeMixin, IDMixin, Base
from sqlalchemy import Column, Integer, ForeignKey, UniqueConstraint
from sqlalchemy.orm import relationship

class QuizPass(DateTimeMixin, IDMixin, Base):
    __tablename__ = "quiz_passes"
    user_id = Column(Integer, ForeignKey("users.id"), nullable=False)
    quiz_id = Column(Integer, ForeignKey("quizzes.id"), nullable=False)
    
    user = relationship("User", back_populates="quiz_passes")
    quiz = relationship("Quiz", back_populates="quiz_passes")

    __table_args__ = (
        UniqueConstraint("user_id", "quiz_id", name="uix_user_quiz"),
    )