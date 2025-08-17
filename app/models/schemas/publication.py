from pydantic import BaseModel, field_validator
from app.models.domain.user import User
from app.models.schemas.rwmodel import RWModel
from app.models.domain.publication import Answer
from typing import List


class QuestionCreateNoID(BaseModel):
    title: str
    body: str
    tags: List[str] | None = None

class QuestionCreate(QuestionCreateNoID):
    user_id: int

class QuestionUpdate(BaseModel):
    title: str | None = None
    body: str | None = None
    tags: List[str] | None = None

class AnswerCreate(BaseModel):
    body: str
    question_id: int
    user_id: int

class AnswerUpdate(BaseModel):
    body: str | None = None

class AnswerRead(RWModel):
    body: str
    question_id: int
    user_id: int
    user : str

    @field_validator("user", mode="before")
    def validate_user(cls, v: User | None) -> str:
        if v is None:
            return "Unknown User"
        return v.username

class QuestionRead(RWModel):
    title: str
    body: str
    tags: List[str] | None = []
    user_id: int
    views : int
    answer_count: int | None = None
    upvote_count: int | None = None
    downvote_count: int | None = None

class QuestionReadSingle(QuestionRead):
    answers: List[AnswerRead] = []
    user : str

    @field_validator("answers", mode="before")
    def validate_answer(cls, v : List[Answer]) -> List[AnswerRead] | None:
        try:
            if not isinstance(v, list):
                return []
            return [AnswerRead.model_validate(answer) for answer in v]
        except Exception:
            return []

    @field_validator("user", mode="before")
    def get_username(cls, v: User | None) -> str:
        if v is None:
            return "Unknown User"
        return v.username