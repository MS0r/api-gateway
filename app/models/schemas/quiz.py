from enum import Enum
from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel


class QuizCreate(BaseModel):
    title: str
    description: str | None = None
    correct_answers: str | None = None
    subunit_id: int

class QuizUpdate(BaseModel):
    title: str | None = None
    description: str | None = None
    correct_answers: str | None = None

class QuizRead(RWModel):
    title: str
    description: str | None
    correct_answers: str | None
    subunit_id: int
