import asyncio
import json
import uuid
import aio_pika

from sqlalchemy.ext.asyncio import AsyncSession
from app.db.crud.exercise import get_exercise
from app.db.crud.submission import (
    get_submissions_by_user_exercise,
    create_submission,
)
from app.models.schemas.submission import SubmissionCreate
from app.models.schemas.erlang import (
    ErlangPayload,
    ErlangCompileResponse,
    ErlangTestResponse,
    ErlangTestPayload,
)


class ErlangService:
    def __init__(self, channel : aio_pika.Channel):
        self.channel = channel
        self._futures : dict[str, asyncio.Future] = {}

    @classmethod
    async def create(cls, channel : aio_pika.Channel):
        self = cls(channel)

        self.callback_queue = await channel.declare_queue(
            exclusive=True
        )

        await self.callback_queue.consume(self._on_response)
        return self

    async def _on_response(self, message: aio_pika.IncomingMessage):
        async with message.process():
            corr_id = message.correlation_id
            future = self._futures.pop(corr_id, None)
            if future:
                future.set_result(
                    json.loads(message.body.decode())
                )

    async def _call(self, payload: dict) -> dict:
        corr_id = str(uuid.uuid4())
        loop = asyncio.get_running_loop()
        future = loop.create_future()
        self._futures[corr_id] = future

        await self.channel.default_exchange.publish(
            aio_pika.Message(
                body=json.dumps(payload).encode(),
                correlation_id=corr_id,
                reply_to=self.callback_queue.name,
            ),
            routing_key="rpc_queue",
        )

        return await future

    async def _test_code_erlang(
        self,
        db: AsyncSession,
        source_code: str,
        exercise_id: int,
    ) -> ErlangTestResponse:

        exercise = await get_exercise(db, exercise_id)
        if not exercise:
            raise ValueError(f"Exercise with ID {exercise_id} not found")

        payload = ErlangTestPayload(
            code=source_code,
            cases=exercise.test_cases,
        )

        res = await self._call(
            payload.model_dump(by_alias=True)
        )

        return ErlangTestResponse.model_validate(res)

    async def health_check(self):
        return {"status": "ok", "message": "Erlang service is running"}
    
    async def compile_erlang_code(
        self,
        payload: ErlangPayload,
    ) -> ErlangCompileResponse:

        res = await self._call(
            payload.model_dump(by_alias=True)
        )
        return ErlangCompileResponse.model_validate(res)

    async def submit_code_erlang(
        self,
        db: AsyncSession,
        submission: SubmissionCreate,
    ) -> ErlangTestResponse:

        submitted = await get_submissions_by_user_exercise(
            db,
            submission.exercise_id,
            submission.user_id,
        )

        results = await self._test_code_erlang(
            db,
            submission.code_snippet,
            submission.exercise_id,
        )

        if (
            not submitted
            and results.status == "ok"
            and results.test_results.failures == 0
        ):
            created = await create_submission(db, submission)
            if not created:
                raise ValueError("Failed to create submission")

        return results