from typing import List
from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession
from app.models.domain.submission import Submission
from app.models.domain.exercise import Exercise
from app.models.domain.unit import Unit
from app.models.schemas.submission import (SubmissionCreate, SubmissionUpdate)

async def create_submission(db: AsyncSession, submission_create: SubmissionCreate) -> Submission:
    submission = Submission(**submission_create.model_dump(by_alias=True))
    db.add(submission)
    await db.commit()
    await db.refresh(submission)
    return submission

async def get_submission(db: AsyncSession, submission_id: int) -> Submission | None:
    submission = await db.get(Submission, submission_id)
    return submission if submission else None

async def get_submissions_from_user_course(db: AsyncSession, user_id: int, course_id: int) -> List[Submission]:
    submissions = await db.execute(
        select(Submission).join(Submission.exercise).join(Exercise.unit).where(Unit.course_id == course_id, Submission.user_id == user_id)
    )
    return submissions.scalars().all()

async def update_submission(db: AsyncSession, submission_id: int, submission_update: SubmissionUpdate) -> Submission | None:
    submission = await db.get(Submission, submission_id)
    if not submission:
        return None
    for key, value in submission_update.model_dump(exclude_unset=True).items():
        setattr(submission, key, value)
    await db.commit()
    await db.refresh(submission)
    return submission

async def delete_submission(db: AsyncSession, submission_id: int) -> bool:
    submission = await db.get(Submission, submission_id)
    if not submission:
        return False
    await db.delete(submission)
    await db.commit()
    return True

async def get_submissions_by_exercise_id(db: AsyncSession, exercise_id: int) -> List[Submission]:
    submissions = await db.execute(
        select(Submission).where(Submission.exercise_id == exercise_id)
    )
    return submissions.scalars().all()

async def get_submissions_by_user_exercise(db : AsyncSession, exercise_id : int, user_id : int) -> Submission | None:
    submission = await db.execute(
        select(Submission).where(Submission.exercise_id == exercise_id, Submission.user_id == user_id)
    )
    return submission.scalar_one_or_none()
