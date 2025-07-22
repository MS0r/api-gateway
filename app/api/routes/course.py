from fastapi import APIRouter, Body, Depends, HTTPException
from sqlalchemy.ext.asyncio import AsyncSession
from app.db.crud import course as course_crud
from app.models.schemas.course import CourseCreate, CourseUpdate, CourseRead, CourseEnrollment, CourseEnrollmentRead
from app.db.events import get_db_session
from app.services import progress as progress_service
from app.api.dependencies.auth import get_current_user_authorize
from app.models.domain.user import User
from app.models.schemas.progress import ProgressSchema


router = APIRouter()

@router.post("", response_model=CourseRead, name="course:create")
async def create_course_route(
    course_create: CourseCreate = Body(..., embed=True, alias="course"), 
    db: AsyncSession = Depends(get_db_session)
) -> CourseRead:
    course = await course_crud.create_course(db, course_create)
    if not course:
        raise HTTPException(status_code=400, detail="Failed to create course")
    return CourseRead.model_validate(course)

@router.get("/{course_id}", response_model=CourseRead, name="course:get")
async def get_course_route(
    course_id: int,
    db: AsyncSession = Depends(get_db_session)
) -> CourseRead:
    course = await course_crud.get_course(db, course_id)
    if not course:
        raise HTTPException(status_code=404, detail="Course not found")
    return course

@router.put("/{course_id}", response_model=CourseRead, name="course:update")
async def update_course_route(
    course_id: int,
    course_update: CourseUpdate = Body(..., embed=True, alias="course"),
    db: AsyncSession = Depends(get_db_session)
) -> CourseRead:
    course = await course_crud.update_course(db, course_id, course_update)
    if not course:
        raise HTTPException(status_code=404, detail="Course not found")
    return course

@router.post("/enroll", response_model=CourseEnrollmentRead, name="course:enroll")
async def enroll_in_course_route(
    enrollment: CourseEnrollment = Body(..., embed=True, alias="enrollment"),
    db: AsyncSession = Depends(get_db_session)
) -> CourseEnrollmentRead:
    enrolled_course = await course_crud.enroll_in_course(db, enrollment)
    if not enrolled_course:
        raise HTTPException(status_code=400, detail="Failed to enroll in course")
    return CourseEnrollmentRead.model_validate(enrolled_course)

@router.get("/enrollments/{user_id}", response_model=list[CourseEnrollmentRead], name="course:enrollments")
async def get_user_enrollments_route(
    user_id: int,
    db: AsyncSession = Depends(get_db_session)
) -> list[CourseEnrollmentRead]:
    enrollments = await course_crud.get_user_enrollments(db, user_id)
    if not enrollments:
        raise HTTPException(status_code=404, detail="No enrollments found for this user")
    return [CourseEnrollmentRead.model_validate(enrollment) for enrollment in enrollments]

@router.get("/{course_id}/progress", response_model=list[CourseEnrollmentRead], name="course:course_progress")
async def get_course_progress_route(
    course_id: int,
    db: AsyncSession = Depends(get_db_session),
    current_user: User = Depends(get_current_user_authorize())
) -> ProgressSchema:
    progress : ProgressSchema = await progress_service.get_user_progress(current_user.id, course_id, db)
    if not progress:
        raise HTTPException(status_code=404, detail="No progress found for this course")
    return progress