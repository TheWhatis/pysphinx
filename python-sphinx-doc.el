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
(defconst func-regex "def .*().*:")

;; (defun checking ()
;;   "Проверка работы регулярки."
;;   (interactive)
;;   (let ((line (thing-at-point 'line 1))
;; 	(regex "\\()?.->?.\\)\\([a-zA-Z0-9\\.]*\\)"))
;;     (message "%s" (re-seq regex line))))

;; (defun lines-with (str &optional ignore-duplicates)
;;   (let (result)
;;     (save-match-data
;;       (while (search-forward str nil t)
;;         (push (line-number-at-pos) result)
;;         (when ignore-duplicates
;;           (end-of-line))))
;;     (nreverse result)))

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
	  (list-numbers (get-line-expression-numbers start end))) ;; Диапазон
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

(defun checking ()
  "Проверка работы чего то."
  (get-expression-string "def " ").*:"))
;;; python-sphinx-doc.el ends here
