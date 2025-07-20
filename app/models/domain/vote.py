import enum

from app.models.common import DateTimeMixin, IDMixin, Base
from sqlalchemy import Column, Enum, Integer, ForeignKey
from sqlalchemy.orm import relationship

class VoteType(enum.Enum):
    UPVOTE = "upvote"
    DOWNVOTE = "downvote"

class Vote(DateTimeMixin, IDMixin, Base):
    __tablename__ = "votes"
    user_id = Column(Integer, ForeignKey("users.id"), nullable=False)
    question_id = Column(Integer, ForeignKey("questions.id"), nullable=True)
    answer_id = Column(Integer, ForeignKey("answers.id"), nullable=True)

    user = relationship("User", back_populates="vote")
    question = relationship("Question", back_populates="vote")
    answer = relationship("Answer", back_populates="vote")

    vote = Column(Enum(VoteType), nullable=False)
    