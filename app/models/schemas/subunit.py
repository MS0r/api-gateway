from typing import List, Literal
from pydantic import BaseModel, field_validator
from app.models.schemas.quiz import QuizRead
from app.models.domain.quiz import Quiz
from app.models.schemas.rwmodel import RWModel

class Block(BaseModel):
    type: Literal['text','html','code']
    value: str

class SubunitCreateNoID(BaseModel):
    title: str
    description: str
    order: int | None = None
    blocks: List[Block]

class SubunitCreate(SubunitCreateNoID):
    unit_id: int

class SubunitUpdate(BaseModel):
    title: str | None = None
    description: str | None = None
    order: int | None = None
    blocks: List[Block] | None = None

class SubunitCreated(RWModel):
    title: str
    description: str
    order: int | None = None
    blocks: List[Block]
    unit_id: int

class SubunitRead(SubunitCreated):
    quiz : QuizRead | None = None

    @field_validator('quiz',mode='before')
    def validate_quiz(cls, v : Quiz | None) -> QuizRead:
        if v is None:
            return None
        return QuizRead.model_validate(v)
