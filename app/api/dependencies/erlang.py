import uuid
from fastapi import Depends
from datetime import datetime, UTC, timedelta
from starlette import responses, requests
from app.services.queues import ErlangRegistry, ErlangService


def _get_client_id(request: requests.Request, response: responses.Response) -> str:
    client_id = request.cookies.get("_client_id")
    if not client_id:
        client_id = str(uuid.uuid4())
        expiry = datetime.now(UTC)
        expiry += timedelta(days=1)

        response.set_cookie(
            key="_client_id",
            value=client_id,
            httponly=True,
            samesite="lax",
            secure=True,
            expires=expiry.strftime('%a, %d-%b-%Y %T GMT'),
            max_age=expiry.strftime('%a, %d-%b-%Y %T GMT')
        )
    return client_id

def _get_registry(request :  requests.Request) -> ErlangRegistry:
    return request.app.state.erlang_registry

def get_erlang_service(
    client_id : str = Depends(_get_client_id),
    registry : ErlangRegistry = Depends(_get_registry) 
    ) -> ErlangService :
    return registry.get(client_id)