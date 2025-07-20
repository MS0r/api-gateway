from enum import Enum
from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel


class QuizAnswersCreate(BaseModel):
    answers: str
    is_correct: bool
    user_id: int
    quiz_id: int
    subunit_id: int

class QuizAnswersUpdate(BaseModel):
    answers: str | None = None
    is_correct: bool | None = None

class QuizAnswersRead(RWModel):
    answers: str
    is_correct: bool
    user_id: int
    quiz_id: int
    subunit_id: int