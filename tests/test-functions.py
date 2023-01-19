from abc import ABCMeta, abstractmethod
from zope.interface import Interface

frametype_class_dict = {}


class ID3v2FrameClassFactory(type):
    def __new__(cls, class_name, parents, attributes):
        print('Creating class', class_name)
        # Here we could add some helper methods or attributes to c
        c = type(class_name, parents, attributes)
        if attributes['frame_identifier']:
            frametype_class_dict[attributes['frame_identifier']] = c
        return c

    @staticmethod
    def get_class_from_frame_identifier(frame_identifier):
        return frametype_class_dict.get(frame_identifier)


class ID3v2Frame(metaclass=ID3v2FrameClassFactory):
    frame_identifier = None


class ID3v2TitleFrame(ID3v2Frame,
                      metaclass=ID3v2FrameClassFactory) :
    frame_identifier = 'TIT2'


class ID3v2CommentFrame(ID3v2Frame, metaclass=ID3v2FrameClassFactory)   :
    frame_identifier = 'COMM'


def decorator(func):
    el = "string"

    print("Thisis decorator")

    def __wrapper__()  :
        print(el)
        return func()

    return __wrapper__


class MyClass:
    """This is myclass description"""
    y: float
    j: str = "String"
    k: dict[str, str] = {"str": "str"}

    def __init__(self, a="str"):
        self.a = a

    def myMethod(self,
                 i: int,  # This test description in arguments
                 s: float = 3.33
                 ):
        """My method description"""
        print('asd')
        print('asf')
        pass

    def function(x, y, j: float, k: int = 3):
        """Method Description"""
        pass

    @staticmethod
    def myStaticMethod(i: int, s: float = 3.33):
        """Static Method Description"""
        pass

    @decorator
    def myDecoratedMethod(self, i: int, la: int = 3):
        pass


class MyClass2:
    """This is my description of class"""
    y: float
    j: str = "String"
    k: dict[str, str] = {"str": "str"}

    def __init__(self):
        pass


class MyAbstractClass():
    __metaclass__ = ABCMeta

    @abstractmethod
    def foo(x, y, c):
        pass


class MyInterface(Interface):
    def myintermethod(self, a, b, c):
        print("asd")


def function(x,
             y: float,
             j: str = "Default",
             k: dict[str, float] = {"str": 3.33}) -> list[list]:
    """This is the description of function"""
    def entryfunction(x, y, j):
        """This is the description of entry function"""
        def entryentryfunction(x, y, s):
            """This is the description of entry-entry function"""
            def entryentryfunction(x, y, n):
                pass

    return [[]]


def function2(x, y: float, j: str = "Default", k: dict[str, float] = {"str": 3.33}) -> list[list]:
    pass


@decorator
def decorated_function(x, y, j, la, o):
    pass

async def async_function(x: int, y, j, la):
    pass

@decorator
class MyAsyncClass():
    __metaclass__ = ABCMeta

    async def __init__(params):
        pass

    async def method(params):
        pass
