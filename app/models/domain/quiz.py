from app.models.common import DateTimeMixin, IDMixin, Base
from sqlalchemy import Boolean, Column, String, Integer, ForeignKey
from sqlalchemy.orm import relationship

class Quiz(DateTimeMixin, IDMixin, Base):
    __tablename__ = "quizzes"
    title = Column(String, nullable=False)
    description = Column(String, nullable=True)

    subunit_id = Column(Integer, ForeignKey("subunits.id"), nullable=False)

    subunit = relationship("Subunit", back_populates="quiz")
    questions = relationship("QuizQuestion", back_populates="quiz", cascade="all, delete-orphan")
    quiz_passes = relationship("QuizPass", back_populates="quiz", cascade="all, delete-orphan")

class QuizQuestion(IDMixin, Base):
    __tablename__ = "quiz_questions"
    question_text = Column(String, nullable=False)
    quiz_id = Column(Integer, ForeignKey("quizzes.id"), nullable=False)

    quiz = relationship("Quiz", back_populates="questions")
    options = relationship("Option", back_populates="quiz_question", cascade="all, delete-orphan")

class Option(IDMixin, Base):
    __tablename__ = "options"
    text = Column(String, nullable=False)
    is_correct = Column(Boolean, default=False)

    quiz_question_id = Column(Integer, ForeignKey("quiz_questions.id"), nullable=False)
    quiz_question = relationship("QuizQuestion", back_populates="options")