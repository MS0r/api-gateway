from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession
from app.models.domain.submission import Submission
from app.models.schemas.submission import (SubmissionCreate, SubmissionUpdate, SubmissionRead)

async def create_submission(db: AsyncSession, submission_create: SubmissionCreate) -> Submission:
    submission = Submission(**submission_create.model_dump(by_alias=True))
    db.add(submission)
    await db.commit()
    await db.refresh(submission)
    return submission

async def get_submission(db: AsyncSession, submission_id: int) -> SubmissionRead | None:
    submission = await db.get(Submission, submission_id)
    if submission:
        return SubmissionRead.model_validate(submission)
    return None

async def get_submissions_from_user_course(db: AsyncSession, user_id: int, course_id: int) -> list[SubmissionRead]:
    submissions = await db.execute(
        select(Submission).where(Submission.user_id == user_id, Submission.course_id == course_id)
    )
    return [SubmissionRead.model_validate(s) for s in submissions.scalars().all()]

async def update_submission(db: AsyncSession, submission_id: int, submission_update: SubmissionUpdate) -> SubmissionRead | None:
    submission = await db.get(Submission, submission_id)
    if not submission:
        return None
    for key, value in submission_update.model_dump(exclude_unset=True).items():
        setattr(submission, key, value)
    await db.commit()
    await db.refresh(submission)
    return SubmissionRead.model_validate(submission)