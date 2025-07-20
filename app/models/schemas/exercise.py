from enum import Enum
from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel


class ExerciseCreate(BaseModel):
    title: str
    description: str | None = None
    expected_output: str
    test_cases: str | None = None
    unit_id: int

class ExerciseUpdate(BaseModel):
    title: str | None = None
    description: str | None = None
    expected_output: str | None = None
    test_cases: str | None = None

class ExerciseRead(RWModel):
    title: str
    description: str | None
    expected_output: str
    test_cases: str | None
    unit_id: int
