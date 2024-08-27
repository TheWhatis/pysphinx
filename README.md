
# &#1057;&#1086;&#1076;&#1077;&#1088;&#1078;&#1072;&#1085;&#1080;&#1077;

1.  [Что это такое?](#org16377fe)
2.  [Установка](#orgd602c07)
    1.  [Конфигурация](#org76ac661)
3.  [Использование](#orgc946d2d)
    1.  [Как выглядит работа Pysphinx](#org3f2ae74)



<a id="org16377fe"></a>

# Что это такое?

**Пока этот проект находится в бета-тестировании (многое не добавлено и не исправлено)**

**(см docs/technical<sub>requirements.org</sub>)**

`pysphinx` это Emacs minor mode для вставки документации `docstring` в
Python функции, методы, классы, данные и т.п. в формате **reStructuredText (ReST), javadoc, Google doc, numpydoc**


<a id="orgd602c07"></a>

# Установка

**Установка пока доступна только с github (в дальнейшем добавлю в melpa)**

Устанавливаем и добавляем директорию по следующему пути:

-   `git clone https://github.com/TheWhatis/pysphinx.git`
-   `cp -r pysphinx ~/.emacs.d/elpa/`


<a id="org76ac661"></a>

## Конфигурация

Добавить следующие строки в свой **.emacs** конфиг

    (add-to-list "/abspath/to/home/.emacs.d/elpa/pysphinx/") ; Абсолютный путь до пакета
    (require 'pysphinx) ; Подключаем `pysphinx'
    (add-hook 'python-mode-hook 'pysphinx-minor-mode) ; Примантируем к `python-mode-hook' `pysphinx-minor-mode'

Когда вы это сделаете, у вас появится комбинация клавиш `C-c h` - к ней привязана интерактивная функция `pysphinx-put-or-delete-docstring`.

С помощью неё вы сможете добавлять к вашим конструкциям шаблоны `docstring`


<a id="orgc946d2d"></a>

# Использование

Откройте Python код, переместите курсор под функцию/метод/класс и нажмите комбинацию клавиш `C-c h` (см. видео ниже)


<a id="org3f2ae74"></a>

## Как выглядит работа Pysphinx

![img](https://raw.githubusercontent.com/TheWhatis/pysphinx/master/support/demo.gif)

