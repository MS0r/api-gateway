from starlette import requests
from app.services.erlang import ErlangService

def get_erlang_service(request :  requests.Request):
    return request.app.state.erlang_service
