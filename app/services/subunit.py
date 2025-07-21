from sqlalchemy.ext.asyncio import AsyncSession
from app.models.domain.subunit import SubUnit
from app.models.schemas.subunit import (SubUnitCreate, SubUnitUpdate, SubUnitRead)

async def create_subunit(db: AsyncSession, subunit_create: SubUnitCreate) -> SubUnit:
    subunit = SubUnit(**subunit_create.model_dump(by_alias=True))
    db.add(subunit)
    await db.commit()
    await db.refresh(subunit)
    return subunit

async def get_subunit(db: AsyncSession, subunit_id: int) -> SubUnitRead | None:
    subunit = await db.get(SubUnit, subunit_id)
    if subunit:
        return SubUnitRead.model_validate(subunit)
    return None

async def update_subunit(db: AsyncSession, subunit_id: int, subunit_update: SubUnitUpdate) -> SubUnitRead | None:
    subunit = await db.get(SubUnit, subunit_id)
    if not subunit:
        return None
    for key, value in subunit_update.model_dump(exclude_unset=True).items():
        setattr(subunit, key, value)
    await db.commit()
    await db.refresh(subunit)
    return SubUnitRead.model_validate(subunit)