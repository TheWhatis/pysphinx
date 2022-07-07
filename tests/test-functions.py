class MyClass:
    """This is my description of class"""
    y: float
    j: str = "String"
    k: dict[str, str] = {"str": "str"}

    def __init__(self, a="str"):
        """Initalize init"""
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


class MyClass2:
    """This is my description of class"""
    y: float
    j: str = "String"
    k: dict[str, str] = {"str": "str"}

    def __init__(self):
        pass


def function(x, y: float, j: str = "Default", k: dict[str, float] = {"str": 3.33}) -> list[list]:
    """This is the description of function"""
    pass


def function2(x, y: float, j: str = "Default", k: dict[str, float] = {"str": 3.33}) -> list[list]:
    pass
