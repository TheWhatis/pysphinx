;;; pysphinx.el --- Sphinx friendly docstrings for Python functions -*- lexical-binding: t; -*-
;; Copyright (c) 2022 <anton-gogo@mail.ru>

;; Author: Whatis <anton-gogo@gmail.com>
;; URL: https://github.com/Whatis/python-sphinx-doc.el
;; Version: 0.0.1
;; Keywords: Sphinx, Python, pysphinx
;; Package-Requires: python, rx, python-mode, python3

;; This program is *not* a part of emacs and is provided under the MIT
;; License (MIT) <http://opensource.org/licenses/MIT>
;;
;; Copyright (c) 2022 <anton-gogo@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;;; ...

;;; Code:
(require 'rx)
(require 'python)

;; Шаблоны
(defvar pysphinx-template-header-levels)

(let ((header-1 "=")
      (header-2 "-")
      (header-3 "~")
      (header-4 "^"))
  (setq pysphinx-template-header-levels (list))
  (push header-4 pysphinx-template-header-levels)
  (push header-3 pysphinx-template-header-levels)
  (push header-2 pysphinx-template-header-levels)
  (push header-1 pysphinx-template-header-levels))

(defvar pysphinx-template-header "{header};")

(defvar pysphinx-template-description (concat ".. This is multistring" "\n"
					      "   Description"))

(defvar pysphinx-template-argument-name ":param {argument_name}:")
(defvar pysphinx-template-argument-type " ``{argument_type}``")
(defvar pysphinx-template-argument-value " - {argument_value}")
(defvar pysphinx-template-argument-description "\n{indent}.. This is argument description")

(defvar pysphinx-template-returns-name ":param returns:")
(defvar pysphinx-template-returns-type "``{returns_type}``")
(defvar pysphinx-template-returns-description "\n{indent}.. This is returns description")

(defvar pysphinx-template-examples (concat "*Examples:*" "\n"
					   "::" "\n"
					   "{indent}.. first example:: function(\"asd\", 33) => asd33"))

(defvar pysphinx-template-function (concat "{header}" "\n" "\n"
					   "{description}" "\n" "\n" "\n"
					   "{arguments}" "\n" "\n" "\n"
					   "{returns}" "\n" "\n" "\n"
					   "{examples}"))

(defvar pysphinx-template-class (concat "{header}" "\n" "\n"
					"{description}" "\n" "\n" "\n"
					"{arguments}" "\n" "\n" "\n"
					"{returns}" "\n" "\n" "\n"
					"{examples}"))

(defvar pysphinx-template-method (concat "{header}" "\n" "\n"
					 "{description}" "\n" "\n" "\n"
					 "{arguments}" "\n" "\n" "\n"
					 "{returns}" "\n" "\n" "\n"
					 "{examples}"))

(defvar pysphinx-template-static-method (concat "{header}" "\n" "\n"
						"{description}" "\n" "\n" "\n"
						"{arguments}" "\n" "\n" "\n"
						"{returns}" "\n" "\n" "\n"
						"{examples}"))

(defun pysphinx-generate-template-header->str (header level)
  "Создание заголовка для шаблона.
HEADER - текст заголовка
LEVEL - уровень вложенности"
  (unless (<= 0 level 3)
    (setq level 3))
  (let ((char (nth level pysphinx-template-header-levels)))
    (when (stringp char)
      (setq char (string-to-char char)))
    (concat
     (replace-regexp-in-string "{header}" header pysphinx-template-header) "\n"
     (make-string (length header) char)))
  )

(defun pysphinx-generate-template-description->str ()
  "Создание описания для шаблона."
  (let ((result))
    (setq result pysphinx-template-description)
    result))

(defun pysphinx-generate-template-arguments->str (arguments)
  "Создание описания аргументов для шаблона.
ARGUMENTS - аргументы конструкций"
  (mapconcat #'pysphinx-generate-template-argument->str arguments "\n\n"))

(defun pysphinx-generate-template-argument->str (arg)
    "Создание описания аргумента для шаблона.
ARG - аргумент конструкции"
  (pcase-let (((seq name type default-value) arg))
    (concat
     (replace-regexp-in-string
      "{argument_name}" name pysphinx-template-argument-name)
     (when type
       (replace-regexp-in-string
	"{argument_type}" type pysphinx-template-argument-type))
     (when default-value
       (replace-regexp-in-string
	"{argument_value}" default-value pysphinx-template-argument-value))
     pysphinx-template-argument-description)))


(defun pysphinx-generate-template-returns->str (returns)
  "Создание описания аргумента который будет возвращать конструкция для шаблона.
RETURNS - что возвращает конструкция (полученный тип)"
  (let ((result))
    (setq result (concat
		  pysphinx-template-returns-name " "
		  (replace-regexp-in-string "{returns_type}" returns pysphinx-template-returns-type)
		  pysphinx-template-returns-description))
    result))

(defun pysphinx-create-directory-if-not-exists (path)
  "Создать директорию если она не существует.
PATH - путь до нее"
  (when (not (file-exists-p path))
    (make-directory path)))

;; Создает нужные директории для работы пакета
(pysphinx-create-directory-if-not-exists "~/.emacs.d/pysphinx")
(pysphinx-create-directory-if-not-exists "~/.emacs.d/pysphinx/temp")

;; Основные функции
(defvar pysphinx--python-boilerplate-file-path ; Путь до модуля python
  (concat
   (file-name-directory (or load-file-name buffer-file-name))
   "pysphinx.py"))

(defun pysphinx--run-python-internal ()
  "Запустить скрытно оболочку python."
  (python-shell-make-comint
   (python-shell-calculate-command)
   (python-shell-get-process-name nil) nil)
  (python-shell-send-string-no-output
   (with-temp-buffer
     (insert-file-contents pysphinx--python-boilerplate-file-path)
     (buffer-string)))
  )

(defun pysphinx-get-line-expression-numbers-from-buffer->list (start end &optional type-search)
  "Получить диапазон между первой регуляркой и последней.
START - начало для поиска
END - конец для поиска
TYPE-SEARCH - тип поиска ('default', 'only-forward', 'only-backward', 'reverse')
  Где:
    'default' - искать по стандарту (сначала сверху курсора, потом снизу)
    'only-forward' - искать только снизу курсора (2 раза)
    'only-backward' - искать только сверху курсора (2 раза)
    'reverse' - искать сначала снизу курсора, потом сверху"

  (save-excursion
    (let ((result)) ; Результат работы функции
      ;; Default
      (when (or (not type-search) (string-match "default" type-search))
	(when (search-backward-regexp start nil t)
	  (push (line-number-at-pos) result) ; Добавляем номер строки
	  ;; Ищем второе совпадение после курсора (когда курсор был переведен функцией search-backward...)
	  (when (search-forward-regexp end nil t)
	    (push (line-number-at-pos) result)))) ; Добавляем номер строки

      ;; Если был передан type-search
      (when type-search
	;; Only Forward
	(when (string-match "only-forward" type-search)
	  (when (search-forward-regexp start nil t)
	    (push (line-number-at-pos) result) ; Добавляем номер строки
	    ;; Ищем второе совпадение после курсора (когда курсор был переведен функцией search-forward...)
	    (when (search-forward-regexp end nil t)
	      (push (line-number-at-pos) result)))) ; Добавляем номер строки

	;; Only Backward
	(when (string-match "only-backward" type-search)
	  (when (search-backward-regexp start nil t)
	    (push (line-number-at-pos) result) ; Добавляем номер строки
	    ;; Ищем второе совпадение после курсора (когда курсор был переведен функцией search-backward...)
	    (when (search-backward-regexp end nil t)
	      (push (line-number-at-pos) result)))) ; Добавляем номер строки

	;; Reverse
	(when (string-match "reverse" type-search)
	  (when (search-forward-regexp start nil t)
	    (push (line-number-at-pos) result) ; Добавляем номер строки
	    ;; Ищем второе совпадение после курсора (когда курсор был переведен функцией search-backward...)
	    (when (search-backward-regexp end nil t)
	      (push (line-number-at-pos) result))))) ; Добавляем номер строки

      ;; Если создан диапазон, то переворачиваем его
      (when result
	(setq result (nreverse result)))
      result))) ; Возвращаем результат

(defun pysphinx-get-construction-data-from-buffer->list (start end match &optional match-number)
  "Получить данные конструкции (имя, на какой линии находится).
START - начало для поиска
END - конец для поиска
MATCH - регулярка для получения названия функции      ↓↓↓↓↓↓↓↓↓↓↓↓
MATCH-NUMBER - номер регулярки (функция (match-string match-number string)) (По Умолчанию 1)"
  (save-excursion
    (let ((name) ; Название кострукции
	  (line-start) ; На какой линии начинается
	  (line-last) ; На какой линии заканчивается объявление
	  (filepath (format "~/.emacs.d/pysphinx/temp/%s" (buffer-name))) ; Путь до файла с конструкцией
	  (line) ; Строка
	  (result)) ; Эта перменная будет возвращаться

      ;; Если не был передан match-number - то по умолчанию 1
      (when (not match-number)
	(setq match-number 1))

      (let ((list-numbers (pysphinx-get-line-expression-numbers-from-buffer->list start end))) ; Диапазон
	(when list-numbers
	  (setq line-start (nth 0 list-numbers)) ; Получаем line-start
	  (setq line-last (nth 1 list-numbers)) ; Получаем line-end

	  ;; Сохраняем весь текст буфера в отдельный файл
	  (when (file-exists-p filepath)
	    (delete-file filepath))

	  (write-region nil nil filepath t)

	  (with-no-warnings ; Перемещаемся к строке, где находится функция
	    (goto-line line-start))

	  (setq line (thing-at-point 'line line-start)) ; Получаем эту строку

	  (when (string-match match line)
	    (setq name (match-string match-number line))) ; Получаем name

	  (when name ; Если name не null, то возвращаем массив
	    (push line-last result)
	    (push line-start result)
	    (push name result)
	    (push filepath result))
	  result)) ; Иначе возвращаем null
      result)))

(defun pysphinx-get-construction-correct-data-from-buffer->list ()
  "Получить список с данными ближайшей конструкции из буфера.
Получает список всех конструкций и возвращает ближайшею из них
Ближайшую от курсора сверху"
  (let ((result)
	(constructions
	 (list
	  ;; DEF - обычная функция
	  (pysphinx-get-construction-data-from-buffer->list "def "
							    (rx
							    ")" (* any) ":")
							    (rx
							     "def "
							     (group (* any))
							     "("))
	  ;; CLASS - обычный класс
	  (pysphinx-get-construction-data-from-buffer->list "class "
							    (rx
							     (group (+ (in "a-zA-Z0-9_")))
							     (or ":" "("))
							    (rx
							     "class "
							     (group (+ (in "a-zA-Z0-9_")))
							     (or ":" "("))))))

    (let ((max-in-list 0) ; Максимальный элемент в списке (номер строки)
	  (line-number)) ; Номер строки

      ;; Получаем ближайшую конструкцию к курсору (ближайшую сверху)
      (dolist (construction constructions)
	(when construction
	  (setq line-number (nth 2 construction))
	  (when (> line-number max-in-list)
	    (setq max-in-list line-number))))

      ;; И собственно получаем её
      (dolist (construction constructions)
	(when construction
	  (setq line-number (nth 2 construction))
	  (when (= max-in-list line-number)
	    (setq result construction))))

      result)))

(defun pysphinx-get-construction-correct-data->list ()
  "Получить данные правильной (ближайшей) конструкции."
  (save-excursion
    (let ((result) ; Результат работы функции здесь
	  (json-result) ; Ответ json
	  ;; Данные конструкции, полученные в буфере
	  (construction (pysphinx-get-construction-correct-data-from-buffer->list)))

      (when construction
	(let ((json-array-type 'list)
	      (filepath (nth 0 construction)) ; Путь до файла с кодом
	      (name (nth 1 construction)) ; Имя конструкции
	      (line-start (nth 2 construction))) ; Номер строки где она начинается

	  (setq json-result
		(json-read-from-string ; Форматируем json в список
		 ;; Вызываем функцию, которая фозвращает данные конструкции
		 ;; предварительно запускаем python через функцию "pysphinx--run-python-internal"
		 (python-shell-send-string-no-output
		  ;; Передаем параметры
		  (format "print_construct(\"%s\", \"%s\", %d)"
			  filepath
			  name
			  line-start))))

	  ;; Если была передана конструкция, то возвращаем её
	  (when json-result
	    (setq result json-result))))
      result)))

;; Генерация шаблонов для конструкций
(defun pysphinx-generate-template-function->str (level header arguments returns)
  "Генерация шаблона функции.
LEVEL - уровень вложенности
HEADER - текст заголовка либо nil
ARGUMENTS - список аргументов либо nil
RETURNS - текст типа возвращенных данных"
  (let ((result pysphinx-template-function)
	(description pysphinx-template-description)
	(examples pysphinx-template-examples))
    ;; Обрабатываем заголовок
    (when (not header)
      (setq header "This is function header"))
    (setq header (pysphinx-generate-template-header->str header level))

    ;; Обрабатываем аргументы
    (if arguments
	(setq arguments (pysphinx-generate-template-arguments->str arguments))

      (setq arguments ""))
    ;; Обрабатываем returns
    (when (not returns)
      (setq returns "None"))
    (setq returns (pysphinx-generate-template-returns->str returns))

    ;; Собстенно создание шаблона
    (setq result (replace-regexp-in-string "{header}" header result))
    (setq result (replace-regexp-in-string "{description}" description result))
    (setq result (replace-regexp-in-string "{arguments}" arguments result))
    (setq result (replace-regexp-in-string "{returns}" returns result))
    (setq result (replace-regexp-in-string "{examples}" examples result))
    result))

(defun pysphinx-generate-template-class->str (level header arguments returns)
  "Генерация шаблона класса.
LEVEL - уровень вложенности
HEADER - текст заголовка либо nil
ARGUMENTS - список аргументов либо nil
RETURNS - текст типа возвращенных данных"
  (let ((result pysphinx-template-function)
	(description pysphinx-template-description)
	(examples pysphinx-template-examples))
    ;; Обрабатываем заголовок
    (when (not header)
      (setq header "This is function header"))
    (setq header (pysphinx-generate-template-header->str header level))

    ;; Обрабатываем аргументы
    (if arguments
	(setq arguments (pysphinx-generate-template-arguments->str arguments))

      (setq arguments ""))
    ;; Обрабатываем returns
    (when (not returns)
      (setq returns "None"))
    (setq returns (pysphinx-generate-template-returns->str returns))

    ;; Собстенно создание шаблона
    (setq result (replace-regexp-in-string "{header}" header result))
    (setq result (replace-regexp-in-string "{description}" description result))
    (setq result (replace-regexp-in-string "{arguments}" arguments result))
    (setq result (replace-regexp-in-string "{returns}" returns result))
    (setq result (replace-regexp-in-string "{examples}" examples result))
    result))

(defun pysphinx-generate-template-method->str (level header arguments returns)
  "Генерация шаблона метода класса.
LEVEL - уровень вложенности
HEADER - текст заголовка либо nil
ARGUMENTS - список аргументов либо nil
RETURNS - текст типа возвращенных данных"
  (let ((result pysphinx-template-function)
	(description pysphinx-template-description)
	(examples pysphinx-template-examples))
    ;; Обрабатываем заголовок
    (when (not header)
      (setq header "This is function header"))
    (setq header (pysphinx-generate-template-header->str header level))

    ;; Обрабатываем аргументы
    (if arguments
	(setq arguments (pysphinx-generate-template-arguments->str arguments))

      (setq arguments ""))
    ;; Обрабатываем returns
    (when (not returns)
      (setq returns "None"))
    (setq returns (pysphinx-generate-template-returns->str returns))

    ;; Собстенно создание шаблона
    (setq result (replace-regexp-in-string "{header}" header result))
    (setq result (replace-regexp-in-string "{description}" description result))
    (setq result (replace-regexp-in-string "{arguments}" arguments result))
    (setq result (replace-regexp-in-string "{returns}" returns result))
    (setq result (replace-regexp-in-string "{examples}" examples result))
    result))

(defun pysphinx-generate-template-static-method->str (level header arguments returns)
  "Генерация шаблона статического метода (@staticmethod).
LEVEL - уровень вложенности
HEADER - текст заголовка либо nil
ARGUMENTS - список аргументов либо nil
RETURNS - текст типа возвращенных данных"
  (let ((result pysphinx-template-function)
	(description pysphinx-template-description)
	(examples pysphinx-template-examples))
    ;; Обрабатываем заголовок
    (when (not header)
      (setq header "This is function header"))
    (setq header (pysphinx-generate-template-header->str header level))

    ;; Обрабатываем аргументы
    (if arguments
	(setq arguments (pysphinx-generate-template-arguments->str arguments))

      (setq arguments ""))
    ;; Обрабатываем returns
    (when (not returns)
      (setq returns "None"))
    (setq returns (pysphinx-generate-template-returns->str returns))

    ;; Собстенно создание шаблона
    (setq result (replace-regexp-in-string "{header}" header result))
    (setq result (replace-regexp-in-string "{description}" description result))
    (setq result (replace-regexp-in-string "{arguments}" arguments result))
    (setq result (replace-regexp-in-string "{returns}" returns result))
    (setq result (replace-regexp-in-string "{examples}" examples result))
    result))

(defun pysphinx-generate-template-class-method->str (level header arguments returns)
  "Генерация шаблона метода класса (@classmethod).
LEVEL - уровень вложенности
HEADER - текст заголовка либо nil
ARGUMENTS - список аргументов либо nil
RETURNS - текст типа возвращенных данных"
  (let ((result pysphinx-template-function)
	(description pysphinx-template-description)
	(examples pysphinx-template-examples))
    ;; Обрабатываем заголовок
    (when (not header)
      (setq header "This is function header"))
    (setq header (pysphinx-generate-template-header->str header level))

    ;; Обрабатываем аргументы
    (if arguments
	(setq arguments (pysphinx-generate-template-arguments->str arguments))

      (setq arguments ""))
    ;; Обрабатываем returns
    (when (not returns)
      (setq returns "None"))
    (setq returns (pysphinx-generate-template-returns->str returns))

    ;; Собстенно создание шаблона
    (setq result (replace-regexp-in-string "{header}" header result))
    (setq result (replace-regexp-in-string "{description}" description result))
    (setq result (replace-regexp-in-string "{arguments}" arguments result))
    (setq result (replace-regexp-in-string "{returns}" returns result))
    (setq result (replace-regexp-in-string "{examples}" examples result))
    result))

(defun pysphinx-generate-template-construction->str (data)
  "Генерировать шаблон для конструкции.
DATA - данные конструкции"
  (let ((result)
	(indent)
	(level (nth 0 data))
	(type (nth 1 data))
	(returns (nth 0 (nth 2 data)))
	(description (nth 1 (nth 2 data)))
	(twodes "")
	(arguments (nth 2 (nth 2 data))))

    ;; Если есть описание, то удаляем ненужные пробелы
    (when description
      (setq description (string-trim description))
      (setq twodes description))

    ;; Если описаие уже существует, то писать его не недо
    (when (or (not description) (<= (length (split-string twodes "\\\\n")) 2))
      ;; Обрабатываем аргументы
      (dolist (idex (number-sequence 1 python-indent-offset)) ; Табуляция
	(setq indent (concat indent " ")))

      ;; Для обычных функций
      (when (string-match "function" type)
	(setq result (pysphinx-generate-template-function->str level description arguments returns)))

      ;; Для обычных методов
      (when (string-match "method" type)
	(setq result (pysphinx-generate-template-method->str level description arguments returns)))

      ;; Для статичных методов (@staticmethod)
      (when (string-match "static-method" type)
	(setq result (pysphinx-generate-template-static-method->str level description arguments returns)))

      ;; Для методов класса (@classmethod)
      (when (string-match "class-method" type)
	(setq result (pysphinx-generate-template-class-method->str level description arguments returns)))

      ;; Для обычный классов
      (when (string-match "class" type)
	(setq result (pysphinx-generate-template-class->str level description arguments returns)))

      ;; Добавляем табуляцию
      (when result
	(setq result (replace-regexp-in-string "{indent}" indent result))))

    result))

(defun pysphinx-prepare-template-before-put->str (level template)
  "Обработать шаблон перед вставкой.
LEVEL - Уровень вложенности
TEMPLATE - Текст шаблона"
  (let ((result)
	(indent) ; Табуляция
	(spl-template (split-string template "\n"))) ; Разделяем шаблон по строкам

    ;; Получаем правильную табуляцию
    (dolist (idex (number-sequence 1 (* python-indent-offset level)))
      (setq indent (concat indent " ")))

    ;; Добавляем табуляцию
    (dolist (string spl-template)
      (when (> (length string) 0)
	(setq string (concat indent string)))
      (setq result (concat result string "\n")))
    result))

(defun pysphinx-put-template-construction->str (data)
  "Вставить шаблон конструкции.
DATA - данные конструкции"
  (let ((template (pysphinx-generate-template-construction->str data))
	(level (nth 0 data))
	(type (nth 1 data)))
    (when template
      (setq template
	    (pysphinx-prepare-template-before-put->str level (concat "\"\"\"" "\n"
								     template
								     "\n" "\"\"\"")))
      (insert template))
    template))

(defun pysphinx-put-docstring ()
  "Вставить docstring для конструкции."
  (interactive)
  (let ((data (pysphinx-get-construction-correct-data->list))
	(line-number (pysphinx-get-construction-correct-data-from-buffer->list))
	(region) ; Регион для удаления существующего описания
	(description)) ; Описание. Нужно для проверки

    ;; Если data == nil
    (if (not data)
	(message (concat "Не найдена ни одна конструкция Python"))
      ;; Если line-number == nil
      (if (not line-number)
	  (message (concat "Не найдена ни одна конструкция Python"))
	(if (not (nth 0 data))
	    (message "Python Error: %s" (nth 1 data))
	  ;; Если все норм
	  (setq line-number (+ (nth 3 line-number) 1))
	  (setq description (nth 1 (nth 2 data)))

	  ;; Перемещаемся к концу конструкции
	  (with-no-warnings
	    (goto-line line-number))

	  ;; Если есть описание
	  (when (stringp description)
	    ;; Если в описании меньше одной строки, то удаляем её
	    (setq description (string-trim description))
	    (if (not (<= (length (split-string description "\\\\n")) 1))
		(message "У этой конструкции уже имеется docstring")
	      (save-excursion
		(setq region
		      (pysphinx-get-line-expression-numbers-from-buffer->list
		       "\"\"\""
		       (rx "\"\"\"" (? "\n")) "only-forward"))

		(when region
		  (setq region (number-sequence (nth 0 region) (nth 0 region)))

		  (dolist (number region)
		    (with-no-warnings
		      (goto-line number))
		    (kill-whole-line))
		  ))))

	  ;; Вставляем docstring
	  (pysphinx-put-template-construction->str data)
	  )))))

(defvar pysphinx-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c h") 'pysphinx-put-docstring)
    map)
  "Комбинации клавиш для 'pysphinx-minor-mode'.")

(define-minor-mode pysphinx-minor-mode
  "Sphinx генератор документаций (docstring) для кода Python"
  :init-value nil
  :lighter " Sphinx"
  :keymap pysphinx-minor-mode-map
  (when pysphinx-minor-mode ; ON
    (pysphinx--run-python-internal)))

(provide 'pysphinx)
;;; pysphinx.el ends here
