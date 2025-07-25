from sqlalchemy.ext.asyncio import AsyncSession
from app.models.domain.vote import Vote
from app.models.schemas.vote import (VoteCreate, VoteUpdate)

async def create_vote(db: AsyncSession, vote_create: VoteCreate) -> Vote:
    vote = Vote(**vote_create.model_dump(by_alias=True))
    db.add(vote)
    await db.commit()
    await db.refresh(vote)
    return vote

async def get_vote(db: AsyncSession, vote_id: int) -> Vote | None:
    vote = await db.get(Vote, vote_id)
    if vote:
        return vote
    return None

async def update_vote(db: AsyncSession, vote_id: int, vote_update: VoteUpdate) -> Vote | None:
    vote = await db.get(Vote, vote_id)
    if not vote:
        return None
    for key, value in vote_update.model_dump(exclude_unset=True).items():
        setattr(vote, key, value)
    await db.commit()
    await db.refresh(vote)
    return vote