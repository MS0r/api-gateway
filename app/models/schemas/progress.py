from pydantic import BaseModel

class ProgressSchema(BaseModel):
    progress : float
    quiz_ended : int
    exercise_ended : int
    