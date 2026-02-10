"""User service for managing user progress and course enrollment."""

from sqlalchemy.ext.asyncio import AsyncSession

from app.services.base import BaseService, Result, NotFoundError
from app.db.crud import quiz_pass as qa_crud
from app.db.crud import submission as sub_crud
from app.db.crud import quiz as quiz_crud
from app.db.crud import exercise as exercise_crud
from app.models.schemas.progress import ProgressSchema


class UserService(BaseService):

    def __init__(self, db: AsyncSession):
        self.db = db

    async def health_check(self) -> dict:
        """Return service health status."""
        return {"status": "ok", "service": "user"}

    async def get_user_progress(
        self, user_id: int, course_id: int
    ) -> Result[ProgressSchema]:
        quiz_passes = await qa_crud.get_quiz_passes_from_user_course(
            self.db, user_id, course_id
        )
        submissions = await sub_crud.get_submissions_from_user_course(
            self.db, user_id, course_id
        )

        quizzes_course = await quiz_crud.get_course_quizzes(self.db, course_id)
        exercises_course = await exercise_crud.get_course_exercises(self.db, course_id)

        completed_quizzes = len(quiz_passes)
        completed_exercises = len(submissions)
        total_quizzes = len(quizzes_course)
        total_exercises = len(exercises_course)

        quiz_progress = completed_quizzes / total_quizzes if total_quizzes > 0 else 0.0
        exercise_progress = (
            completed_exercises / total_exercises if total_exercises > 0 else 0.0
        )

        overall_progress = ((quiz_progress + exercise_progress) / 2) * 100

        progress = ProgressSchema(
            progress=overall_progress,
            total_quizzes=total_quizzes,
            completed_quizzes=completed_quizzes,
            total_exercises=total_exercises,
            completed_exercises=completed_exercises,
        )

        return Result.ok(progress)
