from fastapi import APIRouter, Body, Depends, HTTPException
from sqlalchemy.ext.asyncio import AsyncSession

from app.api.dependencies.auth import get_current_user_authorize
from app.models.domain.user import User
from app.db.crud import course as course_crud
from app.models.schemas.course import CourseCreate, CourseUpdate, CourseRead, EnrollmentRead
from app.api.dependencies.database import get_db_session


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

@router.post("/{course_id}/enroll", response_model=EnrollmentRead, name="course:enroll")
async def enroll_user_in_course(
    course_id: int,
    user: User = Depends(get_current_user_authorize),  # Assuming you have a dependency to get the current user
    db: AsyncSession = Depends(get_db_session)
) -> EnrollmentRead:
    enrolled_course = await course_crud.enroll_user_in_course(db, user, course_id)
    if not enrolled_course:
        raise HTTPException(status_code=400, detail="Failed to enroll in course")
    return EnrollmentRead.model_validate(enrolled_course)