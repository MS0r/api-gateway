from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel

class SubmissionCreate(BaseModel):
    submission_date: str
    code_snippet: str
    exercise_id: int
    user_id: int

class SubmissionUpdate(BaseModel):
    submission_date: str | None = None
    code_snippet: str | None = None

class SubmissionRead(RWModel):
    submission_date: str
    code_snippet: str | None
    exercise_id: int
    user_id: int