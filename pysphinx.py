"""
Вспомогательный модуль для скрипта;
==================================
.. Содержит функции для парсинга конструкций языка python
"""

import os
import ast
import json

from typing import Union
from typing import Any

_path = str


def _parse_construct(
        code: Union[str, _path],
        name: str = "",
        line_start: int = 0,
        line_end: int = 0
) -> list:
    """
    Распарсить переданную конструкцию python;
    -----------------------------------------
/home/whatis/projects/programming/elisp/python-sphinx-doc/pysphinx.py
    .. Получает на вход текст кода (или путь до файла с кодом) и
       возвращает разобранную конструкцию в виде ``["тип returns" [аргументы]]``

       Но если была передана конструкция, в которой допущена ошибка, то
       вернет текст ошибки;

    :param code: ``Union[str, path]``
        .. Текст конструкции или путь до файла с ней;

    :param name: ``str``
        .. Название конструкции;

    :param line_start: ``int``
        .. Номер линии, с которой начинается конструкция

    :param line_end: ``int``
        .. Номер линии, на которой заканчивается конструкция

    :returns: ``list[Union[str, list]]``
        .. Возвращает список [
                "Тип returns"/None,
                "Description"/None
                [
                    ["name", "type", "default_value"],
                    ["name", False, "default_value"],
                    ["name", False, False],
                ]
            ]
    """

    # Обработка аргументов
    code = str(code)
    name = str(name)
    line_start = int(line_start)
    line_end = int(line_end)

    # Обработка исключений
    # Если код с ошибкой, то возвращаем её
    try:
        # Если передан файл, то получаем содержимое и
        # передаем в ast.parse
        if os.path.exists(code):
            with open(code) as f:
                module = ast.parse(f.read())
        else:
            module = ast.parse(code)
    except Exception as error:
        return [str(error), [str(error)]]

    # Если код имеет больше одной конструкции и
    # не был передан аргумент "name", то возвращаем ошибку
    construct = None
    len_constructs = len(module.body)
    if not len_constructs:
        message = "Вы передали неверный \"текст кода/путь\" : \"{code}\""
        return [None, [message]]
    elif len_constructs > 1 and not name:
        message = "Код имеет больше одной функции - необходимо передать аргумент \"name\""
        return [None, [message]]
    elif len_constructs == 1:
        construct = module.body[0]
        construct_type = type(construct)

        # Если некорректный тип конструкции
        construct_types = (ast.FunctionDef, ast.ClassDef)
        if type(construct) not in construct_types:
            message = f"Construct имеет тип \"{construct_type}\", а должен быть " + str(construct_types)
            return [None, [message]]
    else:
        message = f"В коде конструкций с названием \"{name}\" больше одного - нужно передать аргументы \"line_start\" или \"line_end\""

        # Перебираем все конструкции, в
        # поисках нужной
        for el in module.body:
            if isinstance(el, ast.ClassDef):
                if el.name == name:
                    if line_start:  # Если передан line_start
                        if line_start == el.lineno:
                            construct = el
                    else:
                        if construct:
                            return [None, [message]]  # Error (если найдены дубли)
                        construct = el
                else:
                    for method in el.body:
                        if isinstance(method, ast.FunctionDef):
                            if method.name == name:
                                if line_start:  # Если передан line_start
                                    if line_start == method.lineno:
                                        construct = method
                                else:
                                    if construct:
                                        return [None, [message]]  # Error (если найдены дубли)
                                    construct = method

            elif isinstance(el, ast.FunctionDef):
                if el.name == name:
                    if line_start:  # Если передан line_start
                        if line_start == el.lineno:
                            construct = el
                    else:
                        if construct:
                            return [None, [message]]  # Error (если найдены дубли)
                        construct = el

    if not construct:
        message = f"В коде не найдено конструкции с названием \"{name}\""
        return [None, [message]]

    returns = None
    description = None
    arguments = []

    # Получаем аргументы/аттрибуты. Их:
    # Название
    # Типизацию
    # Значение по умолчанию
    argument: Any  # Написал, чтобы не вылезала ошибка incopitable assigment...
    default_value: Any  # Написал, чтобы не вылезала ошибка incopitable assigment...
    if isinstance(construct, ast.FunctionDef):

        # Типизация возвращения (если не None, то str название, иначе None)
        returns = ast.unparse(construct.returns) if construct.returns else construct.returns

        # Описание конструкции (если она есть)
        description = ast.unparse(construct.body[0]) if isinstance(construct.body[0], ast.Expr) else None

        len_args = len(construct.args.args)
        len_defaults = len(construct.args.defaults)

        for i in range(len_args):
            argument = construct.args.args[i]

            # Типизация аргумента (если не None, то str название, иначе None)
            annotation = ast.unparse(argument.annotation) if argument.annotation else argument.annotation
            j = (len_args - i)
            default_value = ast.unparse(construct.args.defaults[-j]) if j <= len_defaults else None

            arguments.append([
                argument.arg,
                annotation,
                default_value
            ])
    elif isinstance(construct, ast.ClassDef):

        # Типизация возвращения (если не None, то str название, иначе None)
        returns = construct.name

        # Описание конструкции (если она есть)
        description = ast.unparse(construct.body[0]) if isinstance(construct.body[0], ast.Expr) else None

        len_args = len(construct.body)

        for i in range(len_args):
            argument = construct.body[i]
            if isinstance(argument, ast.AnnAssign):
                name = ast.unparse(argument.target)

                # Типизация аргумента (если не None, то str название, иначе None)
                annotation = ast.unparse(argument.annotation) if argument.annotation else argument.annotation
                default_value = ast.unparse(argument.value) if argument.value else argument.value

                arguments.append([
                    name,
                    annotation,
                    default_value
                ])

    return [
        returns, description, arguments
    ]


def print_construct(code: Union[str, _path],
                    name: str = "",
                    line_start: int = 0,
                    line_end: int = 0
                    ):
    """
    Вывести в stdout результат работы parse_construct;
    ---------------------------------------------------
    """
    try:
        printmessage = _parse_construct(code, name, line_start, line_end)
    except Exception as error:
        raise error
        err = str(error)
        printmessage = [err, [err]]

    # print(code)

    print(
        json.dumps(printmessage, ensure_ascii=False)
    )


# if __name__ == "__main__":
    # print_construct("test-functions.py", "function", 15)
