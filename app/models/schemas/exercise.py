from enum import Enum
from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel


class ExerciseCreate(BaseModel):
    title: str
    description: str
    exercise_schema: str
    test_cases: str
    unit_id: int

class ExerciseUpdate(BaseModel):
    title: str | None = None
    description: str | None = None
    exercise_schema: str | None = None
    test_cases: str | None = None

class ExerciseRead(RWModel):
    title: str
    description: str
    exercise_schema: str
    test_cases: str
    unit_id: int
