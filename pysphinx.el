;;; pysphinx.el --- Sphinx friendly docstrings for Python functions -*- lexical-binding: t; -*-
;; Copyright (c) 2022 <anton-gogo@mail.ru>

;; Author: Whatis <anton-gogo@gmail.com>
;; URL: https://github.com/TheWhatis/pysphinx
;; Version: 0.8
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

(defconst pysphinx-template-header "{header};")

(defconst pysphinx-template-description
  (concat ".. This is multistring" "\n"
	  "   Description"))

(defconst pysphinx-template-argument-name
  ":param {argument_name}:")
(defconst pysphinx-template-argument-type
  " ``{argument_type}``")
(defconst pysphinx-template-argument-value
  " - default: {argument_value}")
(defconst pysphinx-template-argument-description
  "\n{indent}.. This is argument description")
(defconst pysphinx-template-arguments
  (concat "\n" "\n" "\n" "{arguments}"))


(defconst pysphinx-template-returns-name
  ":param returns:")
(defconst pysphinx-template-returns-type
  "``{returns_type}``")
(defconst pysphinx-template-returns-description
  (concat "\n"
	  "{indent}.. This is returns" "\n"
	  "{indent}   Description;"))


(defconst pysphinx-template-examples
  (concat "*Examples:*" "\n"
	  "::" "\n"
	  "{indent}.. first example:: function(\"asd\", 33) => asd33"))

(defconst pysphinx-template-function
  (concat "{header}" "\n"
	  "{description}"
	  "{arguments}"
	  "{returns}"))

(defconst pysphinx-template-decorated-function
  (concat "{header}" "\n"
	  "{description}"
	  "{arguments}"
	  "{returns}"))

(defconst pysphinx-template-class
  (concat "{header}" "\n"
	  "{description}"
	  "{arguments}"
	  "{returns}"))

(defconst pysphinx-template-abstract-class
  (concat "{header}" "\n"
	  "{description}"
	  "{arguments}"
	  "{returns}"))

(defconst pysphinx-template-interface
  (concat "{header}" "\n"
	  "{description}"
	  "{arguments}"
	  "{returns}"))

(defconst pysphinx-template-method
  (concat "{header}" "\n"
	  "{description}"
	  "{arguments}"
	  "{returns}"))

(defconst pysphinx-template-static-method
  (concat "{header}" "\n"
	  "{description}"
	  "{arguments}"
	  "{returns}"))

(defconst pysphinx-template-class-method
  (concat "{header}" "\n"
	  "{description}"
	  "{arguments}"
	  "{returns}"))

(defconst pysphinx-template-abstract-method
  (concat "{header}" "\n"
	  "{description}"
	  "{arguments}"
	  "{returns}"))

(defconst pysphinx-template-abstract-property-method
  (concat "{header}" "\n"
	  "{description}"
	  "{arguments}"
	  "{returns}"))

(defconst pysphinx-template-decorated-method
  (concat "{header}" "\n"
	  "{description}"
	  "{arguments}"
	  "{returns}"))


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
    (setq header
	  (concat header "\n" (make-string (length header) char)))
    header))


(defun pysphinx-generate-template-description->str ()
  "Создание описания для шаблона."
  (let ((result))
    (setq result pysphinx-template-description)
    result))


(defun pysphinx-generate-template-arguments->str (arguments)
  "Создание описания аргументов для шаблона.
ARGUMENTS - аргументы конструкций"
  (let ((arguments (mapconcat #'pysphinx-generate-template-argument->str arguments "\n\n")))
    (if (string-trim arguments)
	(setq arguments (replace-regexp-in-string "{arguments}" arguments pysphinx-template-arguments))
      (setq arguments (string-trim arguments)))
    arguments))


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
    (setq result (concat "\n" "\n" "\n"
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

;; Генерация шаблонов для конструкций
(defun pysphinx-generate-template->str (type level header arguments returns)
  "Генерация шаблона функции.
TYPE - тип конструкции
LEVEL - уровень вложенности
HEADER - текст заголовка либо nil
ARGUMENTS - список аргументов либо nil
RETURNS - текст типа возвращенных данных"
  (let ((result)
	(description pysphinx-template-description))

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
      (setq result (replace-regexp-in-string "{returns}" returns result)))
    result))


(defun pysphinx-generate-template-construction->str (data)
  "Генерировать шаблон для конструкции.
DATA - данные конструкции"
  (let ((result)
	(indent)
	(level (nth 0 data))
	(type (nth 1 data))
	(returns (nth 0 (nth 4 data)))
	(description (nth 1 (nth 4 data)))
	(twodes "")
	(arguments (nth 2 (nth 4 data))))

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


(defun pysphinx-create-temp-file->str ()
  "Создать временный файл для передачи его в python."
  (let ((filepath (concat "~/.emacs.d/pysphinx/temp/%s" (buffer-name))))
    ;; Сохраняем весь текст буфера в отдельный файл
    (when (file-exists-p filepath)
      (delete-file filepath))

    (write-region nil nil filepath t)
    filepath))


(defun pypshinx-get-correct-construction-data->list ()
  "Получить ближайшую конструкцию сверху от курсора."
  (let ((json-array-type 'list)
	(result)
	(json-result)
	(filepath (pysphinx-create-temp-file->str)))

    (setq json-result
	  (json-read-from-string ; Форматируем json в список
	   ;; Вызываем функцию, которая фозвращает данные конструкции
	   ;; предварительно запускаем python через функцию `pysphinx--run-python-internal'
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
	(level (nth 0 data)))
    (when template
      (setq template
	    (pysphinx-prepare-template-before-put->str level (concat "\"\"\"" "\n"
								     template
								     "\n" "\"\"\"")))
      (insert template))
    template))


(defun pysphinx-put-or-delete-docstring ()
  "Вставить docstring для конструкции."
  (interactive)
  (when (not (python-shell-get-process))
    (pysphinx--run-python-internal))
  (let ((data (pypshinx-get-correct-construction-data->list))
	(line-number) ; Номер строки, куда надо вставлять docstring
	(lines-construct) ; Регион для удаления существующего описания
	(description)) ; Описание. Нужно для проверки

    ;; Если data == nil
    (if (not data)
	(message (concat "Не найдена ни одна конструкция Python или была допущена ошибка в коде"))
      ;; Если ошибка в коде
      (if (not (nth 0 data))
	  (message "Python Error: %s" (nth 1 data))
	;; Если line-number == nil
	(setq line-number (nth 2 data))
	(setq lines-construct (nth 3 data))
	(if (not line-number)
	    (message (concat "Не найдена ни одна конструкция Python"))
	  ;; Если все норм
	  (setq description (nth 1 (nth 4 data)))
	  
	  (save-excursion
	    ;; Переходим к началу конструкции
	    (with-no-warnings
	      (goto-line line-number))

	    ;; Если есть описание, то удаляем его
	    (when (stringp description)
	      (setq description (string-trim description))
	      (when lines-construct
		(setq lines-construct (number-sequence (nth 0 lines-construct) (nth 1 lines-construct)))

		(dolist (number lines-construct)
		  (kill-whole-line))
		))

	    ;; Вставляем docstring
	    (pysphinx-put-template-construction->str data)
	    )
	  )))))

(defvar pysphinx-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c h") 'pysphinx-put-or-delete-docstring)
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
