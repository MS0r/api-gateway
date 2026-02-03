import pika
import time
import uuid
import json

from loguru import logger
from sqlalchemy.ext.asyncio import AsyncSession
from app.db.crud import exercise as exercise_crud
from app.models.schemas.erlang import ErlangPayload, ErlangCompileResponse, ErlangTestResponse,ErlangTestPayload

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
            self.connection.process_data_events(time_limit=None)
        return self.response

    async def health_check():
        return {"status": "ok", "message": "Erlang service is running"}

    def compile_erlang_code(self, payload: ErlangPayload) -> ErlangCompileResponse:
        res = self._call(payload.model_dump(by_alias=True))
        return ErlangCompileResponse.model_validate(res)
    
    async def test_code_erlang_v2(self, db: AsyncSession, source_code: str, exercise_id: int) -> ErlangTestResponse:
        exercise = await exercise_crud.get_exercise(db, exercise_id)
        if not exercise:
            raise ValueError(f"Exercise with ID {exercise_id} not found")
        payload = ErlangTestPayload(code=source_code, cases=exercise.test_cases)
        res = self._call(payload.model_dump(by_alias=True))
        return ErlangTestResponse.model_validate(res)

