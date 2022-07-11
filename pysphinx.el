;;; python-sphinx-doc.el --- Sphinx friendly docstrings for Python functions
;; Copyright (c) 2013 <naikvin@gmail.com>

;; Author: Whatis <anton-gogo@gmail.com>
;; URL: https://github.com/Whatis/python-sphinx-doc.el
;; Version: 0.0.1
;; Keywords: Sphinx, Python, Python-sphinx-doc
;; Package-Requires: ...

;; This program is *not* a part of emacs and is provided under the MIT
;; License (MIT) <http://opensource.org/licenses/MIT>
;;
;; Copyright (c) 2013 <anton-gogo@gmail.com>
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

;;; Code:
(require 'rx)
(require 'python)

(defvar pyshpinx--python-boilerplate-file-path ; Путь до модуля python
  (concat
   (file-name-directory (or load-file-name buffer-file-name))
   "pysphinx.py"))

(defun pyshpinx--run-python-internal ()
  "Запустить скрытно оболочку python."
  (python-shell-make-comint
   (python-shell-calculate-command)
   (python-shell-get-process-name nil) nil)
  (python-shell-send-string-no-output
   (with-temp-buffer
     (insert-file-contents pyshpinx--python-boilerplate-file-path)
     (buffer-string)))
  )

(defun pysphinx-get-line-expression-numbers-from-buffer->list (start end)
  "Получить диапазон между первой регуляркой и последней.
START - начало для поиска
END - конец для поиска"
  (save-excursion
    (let ((result)) ; Результат работы функции
      ;; Ищем первое совпадение перед курсором
      (when (search-backward-regexp start nil t)
	(push (line-number-at-pos) result) ; Добавляем номер строки
	;; Ищем второе совпадение после курсора (когда курсор был переведен функцией search-backward...)
	(when (search-forward-regexp end nil t)
	  (push (line-number-at-pos) result) ; Добавляем номер строки
	  (when result
	    (nreverse result))))))) ; Возвращаем результат

(defun pysphinx-get-construction-data-from-buffer->list (start end match &optional match-number)
  "Получить данные конструкции (имя, на какой линии находится).
START - начало для поиска
END - конец для поиска
MATCH - регулярка для получения названия функции      ↓↓↓↓↓↓↓↓↓↓↓↓
MATCH-NUMBER - номер регулярки (функция (match-string match-number string)) (По Умолчанию 1)"
  (save-excursion
    (let ((name) ; Название кострукции
	  (line-start) ; На какой линии начинается
	  (pathfile) ; Путь до файла с конструкцией
	  (line) ; Строка
	  (result)) ; Эта перменная будет возвращаться

      ;; Если не был передан match-number - то по умолчанию 1
      (when (not match-number)
	(setq match-number 1))

      (save-match-data
	(let ((list-numbers (pysphinx-get-line-expression-numbers-from-buffer->list start end))) ; Диапазон
	  (when list-numbers
	    (setq line-start (nth 0 list-numbers)) ; Получаем line-start
	    (setq pathfile (buffer-file-name)) ; Получаем pathfile

	    (with-no-warnings ; Перемещаемся к строке, где находится функция
	      (goto-line line-start))

	    (setq line (thing-at-point 'line line-start)) ; Получаем эту строку

	    (when (string-match match line)
	      (setq name (match-string match-number line))) ; Получаем name

	    (when name ; Если name не null, то возвращаем массив
	      (push line-start result)
	      (push name result)
	      (push pathfile result))
	    result))) ; Иначе возвращаем null
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

    (let ((max-elem 0) ; Максимальный элемент сейчас (номер строки)
	  (max-in-list 0) ; Максимальный элемент в списке (номер строки)
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
	  (setq max-elem (max max-elem line-number))
	  (when (= max-in-list max-elem)
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


(defun checking ()
  "Проверка работы функций."
  (interactive)
  (insert (format "%s" (pysphinx-get-construction-correct-data->list))))

(global-set-key (kbd "C-c h") 'checking)

(add-hook 'python-mode-hook 'pyshpinx--run-python-internal)

;;; pysphinx.el ends here
