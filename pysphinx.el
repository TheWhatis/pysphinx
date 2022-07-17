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

(defvar pysphinx-template-description
  (concat ".. This is multistring" "\n"
	  "   Description"))

(defvar pysphinx-template-argument-name
  ":param {argument_name}:")
(defvar pysphinx-template-argument-type
  " ``{argument_type}``")
(defvar pysphinx-template-argument-value
  " - default: {argument_value}")
(defvar pysphinx-template-argument-description
  "\n{indent}.. This is argument description")

(defvar pysphinx-template-returns-name
  ":param returns:")
(defvar pysphinx-template-returns-type
  "``{returns_type}``")
(defvar pysphinx-template-returns-description
  "\n{indent}.. This is returns description")

(defvar pysphinx-template-examples
  (concat "*Examples:*" "\n"
	  "::" "\n"
	  "{indent}.. first example:: function(\"asd\", 33) => asd33"))

(defvar pysphinx-template-function
  (concat "{header}" "\n"
	  "{description}" "\n" "\n" "\n"
	  "{arguments}" "\n" "\n" "\n"
	  "{returns}" "\n" "\n" "\n"
	  "{examples}"))

(defvar pysphinx-template-decorated-function
  (concat "{header}" "\n"
	  "{description}" "\n" "\n" "\n"
	  "{arguments}" "\n" "\n" "\n"
	  "{returns}" "\n" "\n" "\n"
	  "{examples}"))

(defvar pysphinx-template-class
  (concat "{header}" "\n"
	  "{description}" "\n" "\n" "\n"
	  "{arguments}" "\n" "\n" "\n"
	  "{returns}" "\n" "\n" "\n"
	  "{examples}"))

(defvar pysphinx-template-abstract-class
  (concat "{header}" "\n"
	  "{description}" "\n" "\n" "\n"
	  "{arguments}" "\n" "\n" "\n"
	  "{returns}" "\n" "\n" "\n"
	  "{examples}"))

(defvar pysphinx-template-interface
  (concat "{header}" "\n"
	  "{description}" "\n" "\n" "\n"
	  "{arguments}" "\n" "\n" "\n"
	  "{returns}" "\n" "\n" "\n"
	  "{examples}"))

(defvar pysphinx-template-method
  (concat "{header}" "\n"
	  "{description}" "\n" "\n" "\n"
	  "{arguments}" "\n" "\n" "\n"
	  "{returns}" "\n" "\n" "\n"
	  "{examples}"))

(defvar pysphinx-template-static-method
  (concat "{header}" "\n"
	  "{description}" "\n" "\n" "\n"
	  "{arguments}" "\n" "\n" "\n"
	  "{returns}" "\n" "\n" "\n"
	  "{examples}"))

(defvar pysphinx-template-class-method
  (concat "{header}" "\n"
	  "{description}" "\n" "\n" "\n"
	  "{arguments}" "\n" "\n" "\n"
	  "{returns}" "\n" "\n" "\n"
	  "{examples}"))

(defvar pysphinx-template-abstract-method
  (concat "{header}" "\n"
	  "{description}" "\n" "\n" "\n"
	  "{arguments}" "\n" "\n" "\n"
	  "{returns}" "\n" "\n" "\n"
	  "{examples}"))

(defvar pysphinx-template-abstract-property-method
  (concat "{header}" "\n"
	  "{description}" "\n" "\n" "\n"
	  "{arguments}" "\n" "\n" "\n"
	  "{returns}" "\n" "\n" "\n"
	  "{examples}"))

(defvar pysphinx-template-decorated-method
  (concat "{header}" "\n"
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
  (let ((char (nth level pysphinx-template-header-levels))
	(header (replace-regexp-in-string "{header}" header pysphinx-template-header)))
    (when (stringp char)
      (setq char (string-to-char char)))
    (setq header (concat header "\n"
			 (make-string (length header) char)))
    header))

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

(defun pypshinx-get-correct-construction-data->list ()
  "Получить ближайшую конструкцию сверху от курсора."
  (let ((json-array-type 'list)
	(result)
	(json-result)
	(filepath (concat "~/.emacs.d/pysphinx/temp/%s" (buffer-name))))

    ;; Сохраняем весь текст буфера в отдельный файл
    (when (file-exists-p filepath)
      (delete-file filepath))

    (write-region nil nil filepath t)

    (setq json-result
	  (json-read-from-string ; Форматируем json в список
	   ;; Вызываем функцию, которая фозвращает данные конструкции
	   ;; предварительно запускаем python через функцию "pysphinx--run-python-internal"
	   ;; Обязательно должен быть подключен pysphinx.py к нашему шелу Python
	   (python-shell-send-string-no-output
	    ;; Передаем параметры
	    (format "print_construct(\"%s\", %d)"
		    filepath
		    (line-number-at-pos)))))

    ;; Если не nil или типа того, то возвращаем результат
    (when json-result
      (setq result json-result))
    result))

;; Генерация шаблонов для конструкций
(defun pysphinx-generate-template->str (type level header arguments returns)
  "Генерация шаблона функции.
TYPE - тип конструкции
LEVEL - уровень вложенности
HEADER - текст заголовка либо nil
ARGUMENTS - список аргументов либо nil
RETURNS - текст типа возвращенных данных"
  (let ((result)
	(description pysphinx-template-description)
	(examples pysphinx-template-examples))

    (when (string-match "function" type)
      (setq result pysphinx-template-function))

    (when (string-match "decorated-function" type)
      (setq result pysphinx-template-decorated-function))

    (when (string-match "class" type)
      (setq result pysphinx-template-class))

    (when (string-match "abstract-class" type)
      (setq result pysphinx-template-abstract-class))

    (when (string-match "interface" type)
      (setq result pysphinx-template-interface))

    (when (string-match "method" type)
      (setq result pysphinx-template-method))

    (when (string-match "static-method" type)
      (setq result pysphinx-template-static-method))

    (when (string-match "class-method" type)
      (setq result pysphinx-template-class-method))

    (when (string-match "abstract-method" type)
      (setq result pysphinx-template-abstract-method))

    (when (string-match "abstract-property-method" type)
      (setq result pysphinx-template-abstract-property-method))

    (when (string-match "decorated-method" type)
      (setq result pysphinx-template-decorated-method))

    (when result
      ;; Обрабатываем заголовок
      (when (not header)
	(setq header (concat "This is " type " header")))
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
      (setq result (replace-regexp-in-string "{examples}" examples result)))
    result))

(defun pysphinx-generate-template-construction->str (data)
  "Генерировать шаблон для конструкции.
DATA - данные конструкции"
  (let ((result)
	(indent)
	(level (nth 0 data))
	(type (nth 1 data))
	(returns (nth 0 (nth 3 data)))
	(description (nth 1 (nth 3 data)))
	(twodes "")
	(arguments (nth 2 (nth 3 data))))

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
      (setq result (pysphinx-generate-template->str type level description arguments returns))

      ;; Для когда не была определена конструкция
      (when (not result)
	(setq result (pysphinx-generate-template->str "function" level description arguments returns)))

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
  (let* ((data (pypshinx-get-correct-construction-data->list))
	(line-number (nth 2 data))
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
	  (setq line-number (+ line-number 1))
	  (setq description (nth 1 (nth 3 data)))

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
