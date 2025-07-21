from fastapi import APIRouter, Body, Depends, HTTPException
from sqlalchemy.ext.asyncio import AsyncSession
from app.services import course as course_service
from app.models.schemas.course import CourseCreate, CourseUpdate, CourseRead, CourseEnrollment, CourseEnrollmentRead
from app.db.events import get_db_session

router = APIRouter()

@router.post("", response_model=CourseRead, name="course:create")
async def create_course_route(
    course_create: CourseCreate = Body(..., embed=True, alias="course"), 
    db: AsyncSession = Depends(get_db_session)
) -> CourseRead:
    course = await course_service.create_course(db, course_create)
    if not course:
        raise HTTPException(status_code=400, detail="Failed to create course")
    return CourseRead.model_validate(course)

@router.get("/{course_id}", response_model=CourseRead, name="course:get")
async def get_course_route(
    course_id: int,
    db: AsyncSession = Depends(get_db_session)
) -> CourseRead:
    course = await course_service.get_course(db, course_id)
    if not course:
        raise HTTPException(status_code=404, detail="Course not found")
    return course

@router.put("/{course_id}", response_model=CourseRead, name="course:update")
async def update_course_route(
    course_id: int,
    course_update: CourseUpdate = Body(..., embed=True, alias="course"),
    db: AsyncSession = Depends(get_db_session)
) -> CourseRead:
    course = await course_service.update_course(db, course_id, course_update)
    if not course:
        raise HTTPException(status_code=404, detail="Course not found")
    return course

@router.post("/enroll", response_model=CourseEnrollmentRead, name="course:enroll")
async def enroll_in_course_route(
    enrollment: CourseEnrollment = Body(..., embed=True, alias="enrollment"),
    db: AsyncSession = Depends(get_db_session)
) -> CourseEnrollmentRead:
    enrolled_course = await course_service.enroll_in_course(db, enrollment)
    if not enrolled_course:
        raise HTTPException(status_code=400, detail="Failed to enroll in course")
    return CourseEnrollmentRead.model_validate(enrolled_course)

@router.get("/enrollments/{user_id}", response_model=list[CourseEnrollmentRead], name="course:enrollments")
async def get_user_enrollments_route(
    user_id: int,
    db: AsyncSession = Depends(get_db_session)
) -> list[CourseEnrollmentRead]:
    enrollments = await course_service.get_user_enrollments(db, user_id)
    if not enrollments:
        raise HTTPException(status_code=404, detail="No enrollments found for this user")
    return [CourseEnrollmentRead.model_validate(enrollment) for enrollment in enrollments]