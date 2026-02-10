"""Forum service for managing questions, answers, and votes."""

from typing import List, Optional
from sqlalchemy.ext.asyncio import AsyncSession

from app.services.base import BaseService, Result, NotFoundError, ValidationError
from app.db.crud import publication as publication_crud
from app.models.domain.publication import Question, Answer
from app.models.domain.vote import Vote
from app.models.schemas.publication import (
    QuestionRead,
    QuestionReadSingle,
    AnswerRead,
    QuestionCreateNoID,
    AnswerCreate,
)
from app.models.schemas.vote import VoteCreate, VoteUpdate


class ForumService(BaseService):
    def __init__(self, db: AsyncSession):
        self.db = db

    async def health_check(self) -> dict:
        """Return service health status."""
        return {"status": "ok", "service": "forum"}

    # Question operations

    async def get_question(
        self, question_id: int, increment_view: bool = False
    ) -> Result[QuestionReadSingle]:
        result = await publication_crud.get_question(
            self.db, question_id, increment_view
        )

        if not result:
            return Result.fail(NotFoundError("Question", str(question_id)))

        question, upvotes, downvotes = result

        answers_result = await self._get_answers_with_votes(question_id)

        question_read = QuestionRead.model_validate(question)

        response = QuestionReadSingle(
            **question_read.model_dump(by_alias=True, exclude_none=True),
            upvote_count=upvotes,
            downvote_count=downvotes,
            answers=answers_result,
            user=question.user,
        )

        return Result.ok(response)

    async def get_questions_list(self, limit: int = 5) -> Result[List[QuestionRead]]:
        questions = await publication_crud.get_last_questions(self.db, limit)

        result = []
        for question, answer_count, upvotes, downvotes in questions:
            q = QuestionRead.model_validate(question)
            # Create a new instance with vote counts instead of setattr
            q_dict = q.model_dump(by_alias=True)
            q_dict["upvote_count"] = upvotes
            q_dict["downvote_count"] = downvotes
            q_dict["answer_count"] = answer_count
            result.append(QuestionRead.model_validate(q_dict))

        return Result.ok(result)

    async def search_questions(self, query: str) -> Result[List[QuestionRead]]:
        questions = await publication_crud.search_questions(self.db, query)

        result = []
        for question, answer_count, upvotes, downvotes in questions:
            q = QuestionRead.model_validate(question)
            q_dict = q.model_dump(by_alias=True)
            q_dict["upvote_count"] = upvotes
            q_dict["downvote_count"] = downvotes
            q_dict["answer_count"] = answer_count
            result.append(QuestionRead.model_validate(q_dict))

        return Result.ok(result)

    async def create_question(
        self, question_data: QuestionCreateNoID, user_id: int
    ) -> Result[QuestionRead]:
        from app.models.schemas.publication import QuestionCreate

        create_data = QuestionCreate(
            **question_data.model_dump(by_alias=True), user_id=user_id
        )

        question = await publication_crud.create_question(self.db, create_data)
        return Result.ok(QuestionRead.model_validate(question))

    async def update_question(
        self, question_id: int, question_data, user_id: int
    ) -> Result[QuestionRead]:
        existing = await publication_crud.get_question(self.db, question_id, False)
        if not existing:
            return Result.fail(NotFoundError("Question", str(question_id)))

        question = await publication_crud.update_question(
            self.db, question_id, question_data
        )

        if not question:
            return Result.fail(NotFoundError("Question", str(question_id)))

        return Result.ok(QuestionRead.model_validate(question))


    async def _get_answers_with_votes(self, question_id: int) -> List[AnswerRead]:
        answers = await publication_crud.get_answers_for_question(self.db, question_id)

        result = []
        for answer, upvotes, downvotes in answers:
            a = AnswerRead.model_validate(answer)
            a_dict = a.model_dump(by_alias=True)
            a_dict["upvote_count"] = upvotes
            a_dict["downvote_count"] = downvotes
            result.append(AnswerRead.model_validate(a_dict))

        return result

    async def create_answer(
        self, answer_data: AnswerCreate, user_id: int
    ) -> Result[AnswerRead]:
        question = await publication_crud.get_question(
            self.db, answer_data.question_id, False
        )
        if not question:
            return Result.fail(NotFoundError("Question", str(answer_data.question_id)))

        answer = await publication_crud.create_answer(self.db, answer_data)

        answer_with_votes = await publication_crud.get_answer(self.db, answer.id)
        if answer_with_votes:
            answer, upvotes, downvotes = answer_with_votes
            a = AnswerRead.model_validate(answer)
            a_dict = a.model_dump(by_alias=True)
            a_dict["upvote_count"] = upvotes
            a_dict["downvote_count"] = downvotes
            return Result.ok(AnswerRead.model_validate(a_dict))

        return Result.ok(AnswerRead.model_validate(answer))

    async def get_answer(self, answer_id: int) -> Result[AnswerRead]:
        result = await publication_crud.get_answer(self.db, answer_id)

        if not result:
            return Result.fail(NotFoundError("Answer", str(answer_id)))

        answer, upvotes, downvotes = result
        a = AnswerRead.model_validate(answer)
        a_dict = a.model_dump(by_alias=True)
        a_dict["upvote_count"] = upvotes
        a_dict["downvote_count"] = downvotes

        return Result.ok(AnswerRead.model_validate(a_dict))

    # Vote operations

    async def vote(
        self, vote_data: VoteCreate
    ) -> Result[QuestionReadSingle | AnswerRead]:
        existing_vote = None

        if vote_data.question_id:
            existing_vote = await publication_crud.get_vote_question(
                self.db, vote_data.question_id, vote_data.user_id
            )
        elif vote_data.answer_id:
            existing_vote = await publication_crud.get_vote_answer(
                self.db, vote_data.answer_id, vote_data.user_id
            )

        if existing_vote:
            if existing_vote.vote == vote_data.vote:
                await publication_crud.remove_vote(self.db, existing_vote)
            else:
                await publication_crud.update_vote(
                    self.db, existing_vote.id, VoteUpdate(vote=vote_data.vote)
                )
        else:
            await publication_crud.vote(self.db, vote_data)

        if vote_data.question_id:
            return await self.get_question(vote_data.question_id, False)
        elif vote_data.answer_id:
            return await self.get_answer(vote_data.answer_id)

        return Result.fail(
            ValidationError("Either question_id or answer_id must be provided")
        )
