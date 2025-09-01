from fastapi import HTTPException
from sqlalchemy.ext.asyncio import AsyncSession

from app.db.crud import publication as publication_crud
from app.models.domain.publication import Question, Answer
from app.models.domain.vote import Vote, VoteType
from app.models.schemas.publication import QuestionRead, QuestionCreateNoID, AnswerRead, QuestionReadSingle
from app.models.schemas.vote import VoteCreate, VoteUpdate

async def get_question(
    db: AsyncSession,
    question_id : int,
    view : bool
) -> QuestionReadSingle:
    question, answers = await publication_crud.get_question(db, question_id,view)
    if not question:
        raise HTTPException(status_code=404, detail="Question not found")
    question, u, d = question
    q = QuestionRead.model_validate(question)
    setattr(q, "upvote_count", u)
    setattr(q, "downvote_count", d)
    complete_question = QuestionReadSingle(**q.model_dump(by_alias=True), answers=answers, user=question.user)
    return complete_question

async def vote_publication(
    db: AsyncSession,
    vote : VoteCreate
) -> QuestionReadSingle | AnswerRead | None:
    existing_vote = None
    if vote.question_id:
        # Check if the user has already voted
        existing_vote = await publication_crud.get_vote_question(db, vote.user_id, vote.question_id)
    elif vote.answer_id:
        # Check if the user has already voted
        existing_vote = await publication_crud.get_vote_answer(db, vote.user_id, vote.answer_id)
    if existing_vote:
        if existing_vote.vote == vote.vote:
            # Remove the existing vote
            await publication_crud.remove_vote(db, existing_vote)
        # Update the existing vote
        await publication_crud.update_vote(db, existing_vote.id, VoteUpdate(vote=vote.vote))
    else:
        # Create a new vote
        await publication_crud.vote(db, vote)

    if vote.question_id:
        return await get_question(db, vote.question_id,False)
    elif vote.answer_id:
        answer, u, d = await publication_crud.get_answer(db, vote.answer_id)
        a = AnswerRead.model_validate(answer)
        setattr(a, "upvote_count", u)
        setattr(a, "downvote_count", d)
        return a

