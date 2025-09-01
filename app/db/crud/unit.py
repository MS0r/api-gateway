from typing import List, Union
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select
from sqlalchemy.orm import selectinload

from app.models.domain.unit import Unit
from app.models.domain.subunit import Subunit
from app.models.schemas.unit import (UnitCreate, UnitUpdate)

async def create_unit(db: AsyncSession, unit_create: UnitCreate) -> Unit:
    unit = Unit(**unit_create.model_dump(by_alias=True))
    db.add(unit)
    await db.commit()
    await db.refresh(unit)
    return unit

async def get_unit(db: AsyncSession, unit_id: int) -> Unit | None:
    unit = await db.get(Unit, unit_id)
    return unit if unit else None

async def update_unit(db: AsyncSession, unit_id: int, unit_update: UnitUpdate) -> Unit | None:
    unit = await db.get(Unit, unit_id)
    if not unit:
        return None
    for key, value in unit_update.model_dump(exclude_unset=True).items():
        setattr(unit, key, value)
    await db.commit()
    await db.refresh(unit)
    return unit

async def get_subunits_by_unit_id(db: AsyncSession, unit_id: int) -> List[Subunit | None]:
    subunits = await db.execute(
        select(Subunit).where(Subunit.unit_id == unit_id).options(selectinload(Subunit.quiz))
    )
    return [subunit for subunit in subunits.scalars().all()]