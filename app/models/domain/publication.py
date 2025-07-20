from app.models.common import DateTimeMixin, IDMixin, Base
from sqlalchemy import Column, String, ForeignKey, Integer
from sqlalchemy.dialects.postgresql import ARRAY
from sqlalchemy.orm import relationship


class Publication(DateTimeMixin, IDMixin, Base):
    __abstract__ = True
    body = Column(String, nullable=False)
    user_id = Column(Integer, ForeignKey("users.id"), nullable=False)

class Question(Publication, Base):
    __tablename__ = "questions"
    title = Column(String, nullable=False)
    tags = Column(ARRAY(String), nullable=True)

    user = relationship("User", back_populates="question")
    answers = relationship("Answer", back_populates="question", cascade="all, delete-orphan")

class Answer(Publication, Base):
    __tablename__ = "answers"
    question_id = Column(Integer, ForeignKey("questions.id"), nullable=False)

    user = relationship("User", back_populates="answer")
    question = relationship("Question", back_populates="answer", cascade="all, delete-orphan")