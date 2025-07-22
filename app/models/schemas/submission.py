from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel
from app.models.domain.submission import SubmissionStatus

class SubmissionCreate(BaseModel):
    submission_date: str
    result: SubmissionStatus = SubmissionStatus.PENDING
    code_snippet: str | None = None
    exercise_id: int
    user_id: int
    unit_id: int

class SubmissionUpdate(BaseModel):
    submission_date: str | None = None
    result: SubmissionStatus | None = None
    code_snippet: str | None = None

class SubmissionRead(RWModel):
    submission_date: str
    result: SubmissionStatus
    code_snippet: str | None
    exercise_id: int
    user_id: int
    unit_id: int