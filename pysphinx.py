"""
Вспомогательный модуль для скрипта;
===================================
.. Содержит функции для парсинга конструкций языка python
"""

import os
import ast
import json
import tokenize

from typing import Union
from typing import Any

from io import StringIO

_path = str
_ast_body = list


def _get_docstring_line(filepath: _path,
                        construction: Union[ast.ClassDef, ast.FunctionDef]
                        ) -> int:
    """
    Получить номер строки, куда вставить docstring;
    -----------------------------------------------
    .. Получает на вход объект конструкции ast и путь до файла .py где она находиться, и
       возвращает номер строки, где нужно вставлять docstring


    :param filepath: ``_path``
      .. Путь до файла с конструкцией;

    :param construction: ``Union[ast.ClassDef, ast.Functiondef, ast.AsyncFunctionDef]``
      .. Ast объект конструкции;


    :returns: ``int``
    .. Возвращает номер строки;
    """
    # Получаем сегмент кода для парсинга
    with open(filepath) as f:
        segment = StringIO(ast.get_source_segment(f.read(), construction, padded=False))

    unparsed_construction = ast.unparse(construction).split("\n")[0].replace(" ", "")

    prev_results = []
    results_line = ""

    for token in tokenize.generate_tokens(segment.readline):
        if token.start[0] > 0:
            trimmed_line = token.line.strip().replace(" ", "")
            if trimmed_line in unparsed_construction:
                if trimmed_line not in prev_results:
                    results_line += trimmed_line
                    prev_results.append(trimmed_line)

            if results_line == unparsed_construction.replace(" ", ""):
                return construction.lineno + len(prev_results)

    return construction.lineno


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

    construction_types = (ast.FunctionDef, ast.ClassDef, ast.AsyncFunctionDef)
    message = "Код пуст или не найдено ближайших конструкций \"_parse_entry\""

    type_construction = None
    construction = None

    for el in module.body:
        if type(el) in construction_types:
            if el.lineno <= line:
                if near_line < el.lineno:
                    near_line = el.lineno
                    construction = el

    if not construction:
        return [None, [message]]

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
                Номер строки, где вставить docstring,
                [
                    номер строки где начинается docstring,
                    номер строки где кончается,
                ]/None,
                [
                    "Тип returns"/None,
                    "Description"/None
                    [
                        ["name", "type", "default_value"],
                        ["name", False, "default_value"],
                        ["name", False, False],
                    ]
                ],
                Номер строки, где находиться конструкция
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

    start_docstring_line = float('inf')  # Номер строки, где начинается docstring
    end_docstring_line = 0  # Номер строки, где заканчивается docstring

    # Описание конструкции (если она есть)
    # А также получение номера строк, где
    # начинается и заканчивается уже созданный docstring
    if isinstance(construct.body[0], ast.Expr):
        if "targets" not in construct.body[0].__dict__:
            for el in construct.body:
                if not isinstance(el, ast.Expr):
                    break
                elif isinstance(el.value, ast.Constant):
                    if el.lineno < start_docstring_line:
                        start_docstring_line = el.lineno

                    if el.end_lineno:
                        if el.end_lineno > end_docstring_line:
                            end_docstring_line = el.end_lineno

                    unparsed = ast.unparse(el)
                    if description:
                        if len(description) > description:
                            description = unparsed
                    else:
                        description = unparsed

    def detect_function(type_construction, async_function = False):
        async_function = "async-" if async_function else ""
        if construct.decorator_list:
            declist = construct.decorator_list
            if isinstance(declist[0], ast.Name):
                if declist[0].id == "staticmethod":
                    type_construction = f"{async_function}static-method"
                elif declist[0].id == "classmethod":
                    type_construction = f"{async_function}class-method"
                elif declist[0].id == "abstractmethod":
                    type_construction = f"{async_function}abstract-method"
                elif declist[0].id == "abstractproperty":
                    type_construction = f"{async_function}abstract-property-method"
                elif type_construction == "method":
                    type_construction = f"{async_function}decorated-method"
                else:
                    type_construction = f"{async_function}decorated-function"
        elif type_construction == "method":
            type_construction = f"{async_function}{type_construction}"
        else:
            if not type_construction:
                type_construction = f"{async_function}function"

        # Типизация возвращения (если не None, то str название, иначе None)
        returns = ast.unparse(construct.returns) if construct.returns else "None"

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

        return type_construction

    if isinstance(construct, ast.FunctionDef):
        type_construction = detect_function(type_construction)
        # Тип конструкции (функция, класс, метод...)
    elif isinstance(construct, ast.AsyncFunctionDef):
        type_construction = detect_function(type_construction, True)
    elif isinstance(construct, ast.ClassDef):
        # Тип конструкции (функция, класс, метод...)
        names_abstract = ("ABC", "ABCMeta", "abc.ABC", "abc.ABCMeta")
        names_interface = ("zope.interface.Interface", "Interface", "interface.Interface")

        async_class = ""
        if len(construct.body) > 0:
            for arg in construct.body:
                if (isinstance(arg, ast.AsyncFunctionDef) and arg.name == "__init__"):
                    async_class = "async-"
                    break


        decorated_class = ""
        if construct.decorator_list:
            decorated_class = "decorated-"

        if construct.bases:
            baseslist = construct.bases
            if isinstance(baseslist[0], ast.Name):
                if baseslist[0].id in names_abstract:
                    type_construction = f"{async_class}{decorated_class}abstract-class"
                elif baseslist[0].id in names_interface:
                    type_construction = f"{async_class}{decorated_class}interface"

            if not type_construction:
                type_construction = "inheritance-class"
        elif construct.keywords:
            keywords = construct.keywords
            if isinstance(keywords[0].value, ast.Name):
                if keywords[0].value.id in names_abstract:
                    type_construction = f"{async_class}{decorated_class}abstract-class"
                elif keywords[0].value.id in names_interface:
                    type_construction = f"{async_class}{decorated_class}interface"

            if not type_construction:
                type_construction = f"{async_class}{decorated_class}inheritance-class"
        elif len(construct.body) > 1:
            for arg in construct.body:
                if isinstance(arg, ast.Assign) and isinstance(arg.targets[0], ast.Name) and isinstance(arg.value, ast.Name):
                    if arg.targets[0].id == "__metaclass__":
                        if arg.value.id in names_abstract:
                            type_construction = f"{async_class}{decorated_class}abstract-class"

        if not type_construction:
            type_construction = f"{async_class}{decorated_class}class"

        # Типизация возвращения
        returns = construct.name

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

    # Если конструкция не найдена, то возвращаем None
    if isinstance(start_docstring_line, float):
        lines_docstring = None
    else:
        lines_docstring = [start_docstring_line, end_docstring_line]

    return [
        level,
        type_construction,
        _get_docstring_line(code, construct),
        lines_docstring,
        [
            returns,
            description,
            arguments
        ],
    ]


def print_construct(code: Union[str, _path],
                    line: int
                    ):
    """
    Вывести в stdout результат работы parse_construct;
    --------------------------------------------------
    """
    try:
        printmessage = _parse_construct(code, line)
    except Exception as error:
        err = str(error)
        printmessage = [err, [err]]

    print(
        json.dumps(printmessage, ensure_ascii=False)
    )
