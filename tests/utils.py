from app.services.base import Result, ServiceError

def mockServiceRaise(msg, cls = ServiceError):
    async def raize(*a,**kw):
        return Result.fail(cls(msg))
    return raize