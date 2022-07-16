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
# from typing import Optional

_path = str
_ast_body = list


def _parse_entry(module,
                 line: int,
                 near_line: int = 0,
                 level: int = 1
                 ) -> list:
    """
    Функция для получения конструкции (рекурсивная);
    ------------------------------------------------
    .. Получает на вход тело файла/текст, где находиться конструкция и
       возращает её и её вложенность;


    :param module: ``module ast from parse`` - default: None
       .. Модуль (Python код), полученный из ast.parse('filepath');

    :param line: ``int``
       .. Номер строки, на которой сейчас находится курсор;

    :param near_line: ``int`` - default: 0
      .. Номер строки ближайшей конструкции - Не обязательный параметр;

    :param level: ``int`` - default: 1
      .. Уровень вложенности конструкции - Не обязательный параметр;


    :returns; ``list/False``
        .. Возвращает список [
            Уровень вложенности/False,
            Конструкцию/"текст ошибки",
            Тип конструкции/None
        ]/False

    """
    # Обработка аргументов
    line = int(line)
    near_line = int(near_line)
    level = int(level)

    construction_types = (ast.FunctionDef, ast.ClassDef)
    message = "Код пуст или не найдено ближайших конструкций \"_parse_entry\""

    type_construction = None
    construction = None

    for el in module.body:
        if type(el) in construction_types:
            if el.lineno < line:
                if near_line < el.lineno:
                    near_line = el.lineno
                    construction = el

    if not construction:
        return [None, [message]]

    if len(construction.body) > 1:
        result_entry = _parse_entry(construction, line, near_line, (level + 1))
        if result_entry[0]:
            level = result_entry[0]
            construction = result_entry[1]
            type_construction = result_entry[2]

    if isinstance(module, ast.ClassDef):
        type_construction = "method"

    return [level, construction, type_construction]


def _parse_construct(
        code: _path,
        line: int
) -> list:
    """
    Распарсить переданную конструкцию python;
    -----------------------------------------
    .. Получает на вход текст кода (или путь до файла с кодом) и
       возвращает разобранную конструкцию в виде ``[Уровень вложенности, "Тип конструкции", ["тип returns", "описание", [аргументы...]]]``

       Но если была передана конструкция, в которой допущена ошибка, то
       вернет текст ошибки;


    :param code: ``Union[str, path]``
        .. Путь до файла с конструкциями;

    :param line: ``int`` - default: 0
        .. Номер строки, где находиться курсор;


    :returns: ``list[Union[str, list]]``
        .. Возвращает список [
                Уровень вложенности,
                "Тип конструкции"/None,
                Номер строки конструкции,
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
    line = int(line)

    # Обработка исключений
    # Если код с ошибкой, то возвращаем её
    try:
        # Если передан файл, то получаем содержимое и
        # передаем в ast.parse
        code = os.path.abspath(os.path.expanduser(code))
        with open(code) as f:
            module = ast.parse(f.read())
    except Exception as error:
        return [None, [str(error)]]

    # Если код имеет больше одной конструкции и
    # не был передан аргумент "name", то возвращаем ошибку
    len_constructs = len(module.body)
    if not len_constructs:
        message = "Файл с кодом пустой: \"{code}\""
        return [None, [message]]

    # Рекурсивно перебирая все элементы получаем нужный
    result_entry = _parse_entry(module, line)

    # Проверяем что получили
    if result_entry[0]:
        level = result_entry[0]
        type_construction = result_entry[2]  # Тип конструкции (функция, класс, метод)
        construct = result_entry[1]
    else:
        return construct

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
                elif declist[0].id == "abstractmethod":
                    type_construction = "abstract-method"
                elif type_construction == "method":
                    type_construction = "decorated-method"
                else:
                    type_construction = "decorated-function"
        else:
            if not type_construction:
                type_construction = "function"

        # Типизация возвращения (если не None, то str название, иначе None)
        returns = ast.unparse(construct.returns) if construct.returns else "None"

        # Описание конструкции (если она есть)
        if isinstance(construct.body[0], ast.Expr):
            if "targets" not in construct.body[0].__dict__:
                if isinstance(construct.body[0].value, ast.Constant):
                    description = ast.unparse(construct.body[0])

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
        if construct.bases:
            baseslist = construct.bases
            if isinstance(baseslist[0], ast.Name):
                if baseslist[0].id == "ABC":
                    type_construction = "abstract-class"

            if not type_construction:
                type_construction = "inheritance-class"
        elif construct.keywords:
            keywords = construct.keywords
            if isinstance(keywords[0].value, ast.Name):
                if keywords[0].value.id == "ABCMeta":
                    type_construction = "abstract-class"

            if not type_construction:
                type_construction = "inheritance-class"
        elif len(construct.body) > 1:
            for arg in construct.body:
                if isinstance(arg, ast.Assign) and isinstance(arg.targets[0], ast.Name):
                    if arg.targets[0].id == "__metaclass__":
                        type_construction = "abstract-class"

        if not type_construction:
            type_construction = "class"

        # Типизация возвращения (если не None, то str название, иначе None)
        returns = construct.name

        # Описание конструкции (если она есть)
        if isinstance(construct.body[0], ast.Expr):
            if "targets" not in construct.body[0].__dict__:
                if isinstance(construct.body[0].value, ast.Constant):
                    description = ast.unparse(construct.body[0])

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
        construct.lineno,
        [
            returns,
            description,
            arguments
        ]
    ]


def print_construct(code: Union[str, _path],
                    line: int
                    ):
    """
    Вывести в stdout результат работы parse_construct;
    ---------------------------------------------------
    """
    try:
        printmessage = _parse_construct(code, line)
    except Exception as error:
        err = str(error)
        printmessage = [err, [err]]

    print(
        json.dumps(printmessage, ensure_ascii=False)
    )
