from sqlalchemy.ext.asyncio import AsyncSession
from app.models.domain.course import Course
from app.models.schemas.course import (CourseCreate, CourseUpdate, CourseRead, CourseEnrollment, CourseEnrollmentRead)

async def create_course(db: AsyncSession, course_create: CourseCreate) -> Course:
    course = Course(**course_create.model_dump(by_alias=True))
    db.add(course)
    await db.commit()
    await db.refresh(course)
    return course

async def get_course(db: AsyncSession, course_id: int) -> CourseRead | None:
    course = await db.get(Course, course_id)
    if course:
        return CourseRead.model_validate(course)
    return None

async def update_course(db: AsyncSession, course_id: int, course_update: CourseUpdate) -> CourseRead | None:
    course = await db.get(Course, course_id)
    if not course:
        return None
    for key, value in course_update.model_dump(exclude_unset=True).items():
        setattr(course, key, value)
    await db.commit()
    await db.refresh(course)
    return CourseRead.model_validate(course)

async def create_course_enrollment(db: AsyncSession, user_id: int, course_id: int) -> CourseEnrollmentRead:
    enrollment = CourseEnrollment(user_id=user_id, course_id=course_id)
    db.add(enrollment)
    await db.commit()
    await db.refresh(enrollment)
    return CourseEnrollmentRead.model_validate(enrollment)

async def get_course_enrollment(db: AsyncSession, user_id: int, course_id: int) -> CourseEnrollmentRead | None:
    enrollment = await db.get(CourseEnrollment, (user_id, course_id))
    if enrollment:
        return CourseEnrollmentRead.model_validate(enrollment)
    return None