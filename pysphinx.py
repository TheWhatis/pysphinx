"""
Вспомогательный модуль для скрипта;
===================================
.. Содержит функции для парсинга конструкций языка python
"""

import os
import ast
import json

from typing import Union
from typing import Any
from typing import Optional

_path = str
_ast_body = list


def _parse_entry(body: _ast_body,
                 name: str,
                 line_start: int = 0,
                 level: int = 1,
                 module: Optional[Union[ast.FunctionDef, ast.ClassDef]] = None):
    """
    Функция для получения конструкции (рекурсивная);
    ------------------------------------------------
    .. Получает на вход тело файла/текст, где находиться конструкция и
       возращает её вложенность и её саму;


    :param body: ``Object Ast Body``
        .. Тело, где находиться конструкция;

    :param name: ``str``
        .. Название конструцкии;

    :param line_start: ``int`` - default: 0
        .. Номер строки где начинается конструкция;

    :param level: ``int`` - default: 0
        .. Уровень её вложенности;

    :param module: ``Optional[Union[ast.FunctionDef, ast.ClassDef]]`` - default: None
       .. Родительский элемент (не обязательный параметр);


    :returns; ``list/False``
        .. Возвращает список [
            Уровень вложенности/False,
            Конструкцию/"текст ошибки"
        ]/False

    """

    construction_types = (ast.FunctionDef, ast.ClassDef)
    result = None
    type_construction = None

    message = f"В коде конструкций с названием \"{name}\" больше одного - нужно передать аргументы \"line_start\" или \"line_end\""

    for construction in body:
        # Проверка всех конструкций внетри конструкции
        if "body" in construction.__dict__:
            if len(construction.body) > 1:
                entry = _parse_entry(construction.body, name, line_start, (level + 1), construction)
                if isinstance(entry, list):
                    if entry[0] and result:
                        return [None, [message]]
                    else:
                        result = entry

                    # Если было по линии, то возвращаем
                    if line_start and result[0]:
                        return result

                    # Если отработало исключение
                    if not result[0]:
                        return result

        # Перебираем конструкции
        if type(construction) in construction_types:
            if construction.name == name:
                if line_start:
                    if line_start == construction.lineno:
                        if module:
                            if isinstance(module, ast.ClassDef):
                                type_construction = "method"
                        result = construction
                    elif result:
                        return [None, [message]]
                elif result:
                    return [None, [message]]
                else:
                    if module:
                        if isinstance(module, ast.ClassDef):
                            type_construction = "method"                        
                    result = construction

    if result:
        if isinstance(result, list):
            return result
        else:
            return [level, result, type_construction]
    else:
        return result


def _parse_construct(
        code: Union[str, _path],
        name: str = "",
        line_start: int = 0,
        line_end: int = 0
) -> list:
    """
    Распарсить переданную конструкцию python;
    -----------------------------------------
    .. Получает на вход текст кода (или путь до файла с кодом) и
       возвращает разобранную конструкцию в виде ``[Уровень вложенности, "Тип конструкции", ["тип returns", "описание", [аргументы...]]]``

       Но если была передана конструкция, в которой допущена ошибка, то
       вернет текст ошибки;


    :param code: ``Union[str, path]``
        .. Текст конструкции или путь до файла с ней;

    :param name: ``str`` - default: ""
        .. Название конструкции;

    :param line_start: ``int`` - default: 0
        .. Номер линии, с которой начинается конструкция

    :param line_end: ``int`` - default: 0
        .. Номер линии, на которой заканчивается конструкция


    :returns: ``list[Union[str, list]]``
        .. Возвращает список [
                Уровень вложенности,
                "Тип конструкции"/None
                [
                    "Тип returns"/None,
                    "Description"/None
                    [
                        ["name", "type", "default_value"],
                        ["name", False, "default_value"],
                        ["name", False, False],
                    ]
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

        if os.path.exists(os.path.abspath(os.path.expanduser(code))):
            code = os.path.abspath(os.path.expanduser(code))
            with open(code) as f:
                module = ast.parse(f.read())
        else:
            module = ast.parse(code)
    except Exception as error:
        return [None, [str(error)]]

    # Если код имеет больше одной конструкции и
    # не был передан аргумент "name", то возвращаем ошибку
    len_constructs = len(module.body)
    if not len_constructs:
        message = "Вы передали неверный \"текст кода/путь\" : \"{code}\""
        return [None, [message]]
    elif len_constructs > 1 and not name:
        message = "Код имеет больше одной функции - необходимо передать аргумент \"name\""
        return [None, [message]]

    # Рекурсивно перебирая все элементы получаем нужный
    construct = _parse_entry(module.body, name, line_start)

    # Проверяем что получили
    if isinstance(construct, list):
        if construct[0]:
            level = construct[0]
            type_construction = construct[2]  # Тип конструкции (функция, класс, метод)
            construct = construct[1]
        else:
            return construct

    # return construct
    if not construct:
        message = f"В коде не найдено конструкции с названием \"{name}\""
        return [None, [message]]

    returns = "None"  # Типизация функции (что она возвращает)
    description = None  # Описание (если оно есть)
    arguments = []  # Аргументы

    # Получаем аргументы/аттрибуты. Их:
    # Название
    # Типизацию
    # Значение по умолчанию
    argument: Any  # Написал, чтобы не вылезала ошибка incopitable assigment...
    default_value: Any  # Написал, чтобы не вылезала ошибка incopitable assigment...
    if isinstance(construct, ast.FunctionDef):
        # Тип конструкции (функция, класс, метод...)
        if construct.decorator_list:
            declist = construct.decorator_list
            if isinstance(declist[0], ast.Name):
                if declist[0].id == "staticmethod":
                    type_construction = "static-method"
                elif declist[0].id == "classmethod":
                    type_construction = "class-method"
                else:
                    type_construction = "decorated"
        else:
            if not type_construction:
                type_construction = "asd"

        # Типизация возвращения (если не None, то str название, иначе None)
        returns = ast.unparse(construct.returns) if construct.returns else "None"

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
        # Тип конструкции (функция, класс, метод...)
        type_construction = "class"

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

    # Убираем ковычки
    if description:
        description = description[:-1][1:]

    return [
        level,
        type_construction,
        [
            returns,
            description,
            arguments
        ]
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

    print(
        json.dumps(printmessage, ensure_ascii=False)
    )
