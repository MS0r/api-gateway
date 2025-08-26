from typing import List

from fastapi import APIRouter, Body, Depends, HTTPException

from sqlalchemy.ext.asyncio import AsyncSession

from app.db.crud import unit as unit_crud
from app.db.crud import subunit as subunit_crud

from app.models.schemas.unit import UnitCreate, UnitUpdate, UnitRead, SingleUnit
from app.models.schemas.subunit import SubunitRead, SubunitCreate, SubunitCreateNoID, SubunitCreated
from app.api.dependencies.database import get_db_session

router = APIRouter()

@router.post("",response_model=SingleUnit,name="unit:create")
async def create_unit_route(
    unit_create: UnitCreate = Body(..., embed=True,alias="unit"), 
    db: AsyncSession = Depends(get_db_session)
) -> SingleUnit:
    unit = await unit_crud.create_unit(db, unit_create)
    if not unit:
        raise HTTPException(status_code=400, detail="Failed to create unit")
    return SingleUnit.model_validate(unit)

@router.get("/{unit_id}", response_model=SingleUnit, name="unit:get")
async def get_unit_route(
    unit_id: int,
    db: AsyncSession = Depends(get_db_session)
) -> SingleUnit:
    unit = await unit_crud.get_unit(db, unit_id)
    if not unit:
        raise HTTPException(status_code=404, detail="Unit not found")
    return SingleUnit.model_validate(unit)

@router.put("/{unit_id}", response_model=SingleUnit, name="unit:update")
async def update_unit_route(
    unit_id: int,
    unit_update: UnitUpdate = Body(..., embed=True, alias="unit"),
    db: AsyncSession = Depends(get_db_session)
) -> SingleUnit:
    unit = await unit_crud.update_unit(db, unit_id, unit_update)
    if not unit:
        raise HTTPException(status_code=404, detail="Unit not found")
    return SingleUnit.model_validate(unit)

@router.post("/{unit_id}", response_model=SubunitCreated, name="unit:create_subunit")
async def create_subunit_route(
    unit_id: int,
    subunit_create: SubunitCreateNoID = Body(..., embed=True, alias="subunit"),
    db: AsyncSession = Depends(get_db_session)
) -> SubunitCreated:
    subunit = await subunit_crud.create_subunit(db, SubunitCreate(**subunit_create.model_dump(by_alias=True), unit_id=unit_id))
    if not subunit:
        raise HTTPException(status_code=400, detail="Failed to create subunit")
    return SubunitCreated.model_validate(subunit)

@router.get("/{unit_id}/subunits", response_model=list[SubunitRead], name="unit:get_subunits")
async def get_subunits_route(
    unit_id: int,
    db: AsyncSession = Depends(get_db_session)
) -> list[SubunitRead]:
    subunits = await unit_crud.get_subunits_by_unit_id(db, unit_id)
    if not subunits:
        raise HTTPException(status_code=404, detail="No subunits found for this unit")
    return [SubunitRead.model_validate(subunit) for subunit in subunits]