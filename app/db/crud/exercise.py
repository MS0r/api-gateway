from typing import List, Optional

from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select

from app.models.domain.exercise import Exercise
from app.models.domain.unit import Unit
from app.models.schemas.exercise import ExerciseCreate, ExerciseUpdate

async def create_exercise(db: AsyncSession, exercise_create: ExerciseCreate) -> Exercise:
    exercise = Exercise(**exercise_create.model_dump(by_alias=True))
    db.add(exercise)
    await db.commit()
    await db.refresh(exercise)
    return exercise

async def get_exercise(db: AsyncSession, exercise_id: int) -> Exercise | None:
    exercise = await db.get(Exercise, exercise_id)
    return exercise if exercise else None

async def update_exercise(db: AsyncSession, exercise_id: int, exercise_update: ExerciseUpdate) -> Exercise | None:
    exercise = await db.get(Exercise, exercise_id)
    if not exercise:
        return None
    for key, value in exercise_update.model_dump(exclude_unset=True).items():
        setattr(exercise, key, value)
    await db.commit()
    await db.refresh(exercise)
    return exercise

async def get_course_exercises(db: AsyncSession, course_id: int) -> List[Exercise]:
    exercises = await db.execute(
        select(Exercise).join(Exercise.unit).where(Unit.course_id == course_id)
    )
    return [exercise for exercise in exercises.scalars().all()]