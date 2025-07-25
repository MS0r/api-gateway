from typing import List, Optional

from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select
from sqlalchemy.orm import selectinload

from app.models.domain.user import User
from app.models.domain.unit import Unit
from app.models.domain.course import Course, Enrollment
from app.models.schemas.course import CourseCreate, CourseUpdate

async def create_course(db: AsyncSession, course_create: CourseCreate) -> Course:
    course = Course(**course_create.model_dump(by_alias=True))
    db.add(course)
    await db.commit()
    await db.refresh(course)
    return course

async def get_course(db: AsyncSession, course_id: int) -> Course | None:
    course = await db.get(Course, course_id)
    return course if course else None

async def update_course(db: AsyncSession, course_id: int, course_update: CourseUpdate) -> Course | None:
    course = await db.get(Course, course_id)
    if not course:
        return None
    for key, value in course_update.model_dump(exclude_unset=True).items():
        setattr(course, key, value)
    await db.commit()
    await db.refresh(course)
    return course

async def enroll_user_in_course(db: AsyncSession, user: User, course_id: int) -> Enrollment:
    enrollment = Enrollment(user_id=user.id, course_id=course_id)
    db.add(enrollment)
    await db.commit()
    await db.refresh(enrollment)
    return enrollment

async def get_user_enrollments(db: AsyncSession, user_id: int) -> List[Enrollment]:
    result = await db.execute(select(Enrollment).where(Enrollment.user_id == user_id))
    return result.scalars().all()

async def get_course_full(db: AsyncSession, course_id: int) -> Course:
    result = await db.execute(
        select(Course)
        .where(Course.id == course_id)
        .options(
            selectinload(Course.units).selectinload(Unit.subunits)
        )
    )
    return result.scalar_one()