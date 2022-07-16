from abc import ABCMeta, abstractmethod


def decorator(func):
    el = "string"

    print("Thisis decorator")

    def __wrapper__():
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

    def myMethod(self, i: int, s: float = 3.33):
        """Method Description"""
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


def function(x, y: float, j: str = "Default", k: dict[str, float] = {"str": 3.33}) -> list[list]:
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
