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

(defun get-line-expression-numbers->list (start end)
  "Получить диапазон между первой регуляркой и последней.
START - начало для поиска
END - конец для поиска"
  (save-excursion
    (let ((result nil))
      (save-match-data
	(when (search-backward-regexp start nil t)
	  (push (line-number-at-pos) result)
	  (when (search-forward-regexp end nil t)
	    (push (line-number-at-pos) result)
	    (when result
	      (nreverse result))))))))

(defun get-expression-string->str (start end)
  "Получить строку по регуляркам.
START - начало для поиска
END - конец для поиска"
  (save-excursion
    (let ((result)
	  (seq-list nil) ;; Перебираемый список
	  (numb nil) ;; Номер строки
	  (line nil) ;; Строка
	  (list-numbers (get-line-expression-numbers->list start end))) ;; Диапазон
      (save-match-data
	(setq seq-list (number-sequence (nth 0 list-numbers) (nth 1 list-numbers))) ;; Создаем по диапазону список номеров строк
	(when seq-list
	  (setq numb (car seq-list))
	  (with-no-warnings
	    (goto-line numb))
	  (setq line (thing-at-point 'line numb))
	  (setq result line)
	  (setq seq-list (cdr seq-list))
	  (while seq-list
	    (setq numb (car seq-list))
	    (with-no-warnings
	      (goto-line numb))
	    (setq line (thing-at-point 'line numb))
	    (setq result (concat result line))
	    (setq seq-list (cdr seq-list)))
	  (setq result (replace-regexp-in-string "\n\\|  " "" result nil t))))))) ;; Убираем перевод каретки и ненужные пробелы

(defun get-construction-data->list (start end match)
  "Получить данные конструкции (имя, на какой линии находится).
START - начало для поиска
END - конец для поиска
MATCH - регулярка для получения названия функции"
  (save-excursion
    (let ((name) ; Название кострукции
	  (line-start) ; На какой линии начинается
	  (pathfile) ; Путь до файла с конструкцией
	  (temp)
	  (result))
      (save-match-data
	(let ((list-numbers (get-line-expression-numbers->list start end))) ; Диапазон
	  (setq line-start (nth 0 list-numbers)) ; Получаем line-start
	  (setq pathfile (buffer-file-name)) ; Получаем pathfile

	  (with-no-warnings
	    (goto-line line-start))

	  (setq temp (thing-at-point 'line line-start)) ; Получаем name
	  (when (string-match match temp)
	    (setq name (match-string 1 temp)))

	  (when name ; Если name не null, то возвращаем массив
	    (push name result)
	    (push line-start result)
	    (push pathfile result))
	  )) ; Иначе возвращаем null
      result)))

(defun get-result-construction-data->list (start end match)
  "Вызвать функцию python и получить данные конструкции.
START - начало для поиска
END - конец для поиска
MATCH - регулярка для получения названия функции"
  (save-excursion
    ))

(defun checking ()
  "Проверка работы функций."
  (interactive)
  (message "%s" (get-construction-data->list "def " (rx
						     "(" (* any)
						     (group ")" (* any) ":"))
					     (rx "def " (group (* any)) "("))))

(global-set-key (kbd "C-c h") 'checking)
;;; pysphinx.el ends here
