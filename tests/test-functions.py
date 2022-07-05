def func():
    pass



def arguments_function(f, s, t):
    pass


def expand_arguments_function(
        f,
        s,
        t
):
    pass


def expand_arguments_defaults(
        f= "string",
        s=1.2,
        t={"str": 1.2}
):
    pass


def typing_function() -> str:
    pass

def typing_arguments_function(f: str, s: float, t: dict):
    pass


def typing_arguments_function2(f: str, s: float, t: dict) -> dict[str, float]:
    pass


def expand_typing_arguments_function(
        f: str,
        s: float,
        t: dict
):
    pass


def expand_typing_arguments_function2(
        f: str,
        s: float,
        t
) -> str:
    pass


def expand_typing_arguments_function_defaults(
        f: str = "default", #This is the description
        s: dict = dict[str, float],
        t: dict = str
) -> str:
    pass

