from sqlalchemy.ext.asyncio import AsyncSession

from app.db.crud import publication as publication_crud
from app.models.domain.publication import Question, Answer
from app.models.domain.vote import Vote, VoteType
from app.models.schemas.publication import QuestionRead, QuestionCreateNoID, AnswerRead
from app.models.schemas.vote import VoteCreate, VoteUpdate

async def vote_publication(
    db: AsyncSession,
    vote : VoteCreate
) -> Vote | None:
    existing_vote = None
    if vote.question_id:
        question = await publication_crud.get_question(db, vote.question_id)
        if not question:
            return None
        # Check if the user has already voted
        existing_vote = await publication_crud.get_vote_question(db, vote.user_id, vote.question_id)
    elif vote.answer_id:
        answer = await publication_crud.get_answer(db, vote.answer_id)
        if not answer:
            return None
        # Check if the user has already voted
        existing_vote = await publication_crud.get_vote_answer(db, vote.user_id, vote.answer_id)
        
    if existing_vote:
        if existing_vote.vote == vote.vote:
            # Remove the existing vote
            publication_crud.remove_vote(db, existing_vote)
        # Update the existing vote
        publication_crud.update_vote(db, existing_vote.id, VoteUpdate(vote=vote.vote))
    else:
        # Create a new vote
        publication_crud.vote(db, vote)
    