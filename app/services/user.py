# Create user progress, user course enrolled, user code submission, and user quiz answers web responses
from sqlalchemy.ext.asyncio import AsyncSession

from app.models.schemas.progress import ProgressSchema

from app.db.crud import quiz_pass as qa_crud
from app.db.crud import submission as sub_crud
from app.db.crud import quiz as quiz_crud
from app.db.crud import exercise as exercise_crud


async def get_user_progress(user_id : int, course_id : int, db : AsyncSession) -> ProgressSchema:

    quiz_passes = await qa_crud.get_quiz_passes_from_user_course(db, user_id, course_id)
    submissions = await sub_crud.get_submissions_from_user_course(db, user_id, course_id)
    
    quizzes_course = await quiz_crud.get_course_quizzes(db, course_id)
    exercises_course = await exercise_crud.get_course_exercises(db, course_id)

    completed_quizzes = len(quiz_passes)
    completed_exercises = len(submissions)

    total_quizzes = len(quizzes_course)
    total_exercises = len(exercises_course)

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