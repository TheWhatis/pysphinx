class MyClass:
    """This is my description of class"""
    y: float
    j: str = "String"
    k: dict[str, str] = {"str": "str"}
    
    def __init__(self, a = "str"):
        """Initalize init"""
        self.a = a

    def myMethod(self, i: int, s: float = 3.33):
        """Method Description"""
        pass
        
