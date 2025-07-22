# Create user progress, user course enrolled, user code submission, and user quiz answers web responses
from sqlalchemy.ext.asyncio import AsyncSession
from app.models.domain.user import User

from app.core.settings.app import AppSettings
from app.models.domain.user import User
from app.models.schemas.progress import ProgressSchema
from app.db.crud import quiz_answers as qa_crud
from app.db.crud import submission as sub_crud
from app.models.domain.submission import SubmissionStatus

async def get_user_progress(user_id : int, course_id : int, db : AsyncSession, settings : AppSettings) -> ProgressSchema:
    # Placeholder for user progress logic
    quiz_answers = await qa_crud.get_quiz_answers_from_user_course(db, user_id, course_id)
    submissions = await sub_crud.get_submissions_from_user_course(db, user_id, course_id)

    completed_quizzes = len([q for q in quiz_answers if q.is_correct])
    completed_exercises = len([s for s in submissions if s.result == SubmissionStatus.COMPLETED])

    total_quizzes = len(quiz_answers)
    total_exercises = len(submissions)

    if total_quizzes > 0:
        quiz_progress = completed_quizzes / total_quizzes
    else:
        quiz_progress = 0.0
    if total_exercises > 0:
        exercise_progress = completed_exercises / total_exercises
    else:
        exercise_progress = 0.0

    progress = (quiz_progress + exercise_progress) / 2

    return ProgressSchema(
        progress=progress,
        total_quizzes=total_quizzes,
        completed_quizzes=completed_quizzes,
        total_exercises=total_exercises,
        completed_exercises=completed_exercises,
    )