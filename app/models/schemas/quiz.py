from enum import Enum
from typing import List
from pydantic import BaseModel, field_validator
from app.models.domain.quiz import Option, QuizQuestion
from app.models.schemas.rwmodel import RWModel

class OptionCreate(BaseModel):
    text: str
    is_correct: bool
    quiz_question_id : int

class OptionRead(RWModel):
    text: str
    is_correct: bool
    quiz_question_id: int

class QuizQuestionCreate(BaseModel):
    question_text: str
    quiz_id: int

class QuizQuestionRead(RWModel):
    question_text: str
    quiz_id: int
    options : List[OptionRead] = []

    @field_validator('options',mode='before')
    def validate_options(cls,v : List[Option] | None) -> List[OptionRead]:
        if v is None:
            return []
        if isinstance(v, list):
            return [OptionRead.model_validate(option) for option in v]
        raise ValueError("Options must be a list of Option objects")

class QuizCreate(BaseModel):
    title: str
    description: str | None = None
    subunit_id: int

class QuizUpdate(BaseModel):
    title: str | None = None
    description: str | None = None

class QuizRead(RWModel):
    title: str
    description: str | None
    subunit_id: int
    questions: List[QuizQuestionRead] = []

    @field_validator('questions', mode='before')
    def validate_questions(cls, v: List[QuizQuestion] | None) -> List[QuizQuestionRead]:
        if v is None:
            return []
        if isinstance(v, list):
            return [QuizQuestionRead.model_validate(question) for question in v]
        raise ValueError("Questions must be a list of QuizQuestionRead objects")
