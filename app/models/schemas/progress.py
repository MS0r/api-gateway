from pydantic import BaseModel

class ProgressSchema(BaseModel):
    progress : float
    total_quizzes : int
    completed_quizzes : int
    total_exercises : int
    completed_exercises : int
    