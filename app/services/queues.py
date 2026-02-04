import pika
import time
import uuid
import json

from loguru import logger
from typing import Dict

from sqlalchemy.ext.asyncio import AsyncSession
from app.db.crud.exercise import get_exercise
from app.db.crud.submission import get_submissions_by_user_exercise,create_submission
from app.models.schemas.submission import SubmissionCreate
from app.models.schemas.erlang import (
    ErlangPayload, 
    ErlangCompileResponse, 
    ErlangTestResponse,
    ErlangTestPayload
    )

class ErlangService:
    def __init__(self, host : str, user : str, password : str, retries = 10):
        for i in range(retries):
            try:
                self.conn = pika.BlockingConnection(
                    pika.ConnectionParameters(
                        host=host,
                        credentials=pika.PlainCredentials(username=user,password=password)
                    )
                )
                break
            except Exception as e:
                logger.warning(f"Rabbitmq not connected, trying again... {e}")
                time.sleep(3)
        
        self.channel = self.conn.channel()
        result = self.channel.queue_declare(queue="",exclusive=True)
        self.callback_queue = result.method.queue

        self.channel.basic_consume(
            queue=self.callback_queue,
            on_message_callback=self._on_response,
            auto_ack=True
        )

        self.response = None
        self.corr_id = None

    def _on_response(self, ch, method, props, body):
        if self.corr_id == props.correlation_id:
            self.response = json.loads(body.decode())

    def _call(self, payload : dict):
        self.response = None
        self.corr_id = str(uuid.uuid4())
        self.channel.basic_publish(
            exchange='',
            routing_key='rpc_queue',
            properties=pika.BasicProperties(
                reply_to=self.callback_queue,
                correlation_id=self.corr_id,
            ),
            body=json.dumps(payload).encode())
        while self.response is None:
            self.conn.process_data_events(time_limit=None)
        return self.response

    def close(self):
        self.channel.queue_delete(queue=self.callback_queue)
        self.channel.close()
        self.conn.close()

    async def _test_code_erlang(
        self, 
        db: AsyncSession, 
        source_code: str, 
        exercise_id: int
        ) -> ErlangTestResponse:

        exercise = await get_exercise(db, exercise_id)
        if not exercise:
            raise ValueError(f"Exercise with ID {exercise_id} not found")
        
        payload = ErlangTestPayload(code=source_code, cases=exercise.test_cases)
        res = self._call(payload.model_dump(by_alias=True))
        return ErlangTestResponse.model_validate(res)

    async def health_check(self):
        return {"status": "ok", "message": "Erlang service is running"}

    def compile_erlang_code(self, payload: ErlangPayload) -> ErlangCompileResponse:
        res = self._call(payload.model_dump(by_alias=True))
        return ErlangCompileResponse.model_validate(res)

    async def submit_code_erlang(
        self, 
        db: AsyncSession, 
        submission: SubmissionCreate
        ) -> ErlangTestResponse:

        submitted = await get_submissions_by_user_exercise(
            db, 
            submission.exercise_id, 
            submission.user_id
        )
        results = await self._test_code_erlang(
            db, 
            submission.code_snippet, 
            submission.exercise_id
        )
        if not submitted and results.test_results.failures == 0 and results.status == "ok":
            created = await create_submission(db, submission)
            if not created:
                raise ValueError("Failed to create submission")
        return results

class ErlangRegistry:

    def __init__(self, host : str, user : str, password : str):
        self._host = host
        self._user = user
        self._password = password
        self._services = Dict[str, ErlangService] = {}
    
    def get(self, client_id : str) -> ErlangService:
        if client_id not in self._services:
            self._services[client_id] = ErlangService(
                host=self._host,
                user=self._user,
                password=self._password
            )
        return self._services[client_id]
    
    def close_queues(self):
        while len(self._services) > 0:
            self._services.popitem().close()
            