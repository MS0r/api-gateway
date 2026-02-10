def mockraise(msg):
    def raize(*a,**kw):
        raise Exception(msg)
    return raize