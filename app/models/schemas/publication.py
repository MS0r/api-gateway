from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel
from typing import List


class QuestionCreate(BaseModel):
    title: str
    tags: List[str] | None = None
    user_id: int

class QuestionUpdate(BaseModel):
    title: str | None = None
    body: str | None = None
    tags: List[str] | None = None

class QuestionRead(RWModel):
    _id: int
    title: str
    body: str
    tags: List[str] | None
    user_id: int

class AnswerCreate(BaseModel):
    body: str
    question_id: int

class AnswerUpdate(BaseModel):
    body: str | None = None
    
class AnswerRead(RWModel):
    _id: int
    body: str
    question_id: int
    user_id: int