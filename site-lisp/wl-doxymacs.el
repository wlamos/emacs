;;; wl-doxymacs.el --- 

;; Copyright (C) 2009  wlamos

;; Author: wlamos <wlamos@gmail.com>
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

(require 'doxymacs)

(define-key doxymacs-mode-map "\C-cdt"
  'doxymacs-insert-test-case-file-comment)

(defun doxymacs-insert-test-case-file-comment ()
  "Inserts Doxygen documentatoin for the current file at current point."
  (interactive "*")
  (doxymacs-call-template "test-case-file-comment"))

(defconst doxymacs-JavaDoc-test-case-file-comment-template
  '("/**" > n
    " * " (doxymacs-doxygen-command-char) "file   "
    (if (buffer-file-name)
	(file-name-nondirectory (buffer-file-name))
      "") > n
      " * " (doxymacs-doxygen-command-char) "author " (user-full-name)
      (doxymacs-user-mail-address)
      > n
      " * " (doxymacs-doxygen-command-char) "date   " (current-time-string) > n
      " * " > n
      " * " (doxymacs-doxygen-command-char) "brief  " (p "Brief description of this file: ") > n
      " * " > n
      " * " (doxymacs-doxygen-command-char) "expected_behavior  " (p "Expected behavior of this test case: ") > n
      " * " > n
      " * " p > n
      " */" > n)
  "Default JavaDoc-style template for test case file documentation.")

(defconst doxymacs-Qt-test-case-file-comment-template
 '("/*!" > n
   " " (doxymacs-doxygen-command-char) "file   "
   (if (buffer-file-name)
       (file-name-nondirectory (buffer-file-name))
     "") > n
   " " (doxymacs-doxygen-command-char) "author " (user-full-name)
   (doxymacs-user-mail-address)
   > n
   " " (doxymacs-doxygen-command-char) "date   " (current-time-string) > n
   " " > n
   " " (doxymacs-doxygen-command-char) "brief  " (p "Brief description of this file: ") > n
   " " > n
   " " (doxymacs-doxygen-command-char) "expected_behavior  " (p "Expected behavior of this test case: ") > n
   " " > n
   " " p > n
   "*/" > n)
 "Default Qt-style template for test case file documentation.")

(defconst doxymacs-C++-test-case-file-comment-template
 '("///" > n
   "/// " (doxymacs-doxygen-command-char) "file   "
   (if (buffer-file-name)
       (file-name-nondirectory (buffer-file-name))
     "") > n
   "/// " (doxymacs-doxygen-command-char) "author " (user-full-name)
   (doxymacs-user-mail-address)
   > n
   "/// " (doxymacs-doxygen-command-char) "date   " (current-time-string) > n
   "/// " > n
   "/// " (doxymacs-doxygen-command-char) "brief  " (p "Brief description of this file: ") > n
   "/// " > n
   "/// " (doxymacs-doxygen-command-char) "expected_behavior  " (p "Expected behavior of this test case: ") > n
   "/// " > n
   "/// " p > n
   "///" > n)
 "Default C++-style template for test case file documentation.")

(provide 'wl-doxymacs)
;;; wl-doxymacs.el ends here
