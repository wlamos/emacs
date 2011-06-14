;;; company-sawfish.el --- Company Backends for Sawfish mode

;; Copyright (C) 2011  Zhang Wenliang

;; Author: Zhang Wenliang <wlamos@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(defun company-sawfish-complete ()
  ""
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (when sym
      (let* ((sym (symbol-name (read sym)))
	     (sym-re (concat "^" (regexp-quote sym)))
	     (completion (sawfish-eval-read
                          `(complete-string ,sym (mapcar symbol-name (apropos ,sym-re))))))
	(sawfish-eval-read `(mapcar symbol-name (apropos ,sym-re)))))))

(defun company-sawfish-backend (command &optional arg &rest ignored)
  (case command
    ('prefix (company-sawfish-complete))
    ('candidates (company-sawfish-complete))
    ('meta ())))

(provide 'company-sawfish)
;;; company-sawfish.el ends here
