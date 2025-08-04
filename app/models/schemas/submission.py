from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel

class SubmissionCreateNoID(BaseModel):
    code_snippet: str

class SubmissionCreate(SubmissionCreateNoID):
    exercise_id: int
    user_id: int

class SubmissionUpdate(BaseModel):
    code_snippet: str | None = None

class SubmissionRead(RWModel):
    code_snippet: str | None
    exercise_id: int
    user_id: int