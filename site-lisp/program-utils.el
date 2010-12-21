;;; program-utils.el --- Some utils for programming

;; Copyright (C) 2010  Wenliang Zhang

;; Author: Wenliang Zhang <wlamos@gmail.com>
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

;; CVS setting
(setq cvs-dired-use-hook 'always)

(require 'psvn)
(require 'magit)
(require 'one-key)

(defvar one-key-vcs-alist nil
  "`One-Key' menu list for VCS.")

(setq one-key-vcs-alist
      '((("c" . "CVS") . cvs-examine)
	(("s" . "SVN") . svn-examine)
	(("g" . "Git") . magit-status)))

(defun one-key-menu-vcs ()
  "`One-Key' menu for VCS"
  (interactive)
  (one-key-menu "EMMS" one-key-vcs-alist t))

(defun zwl-vcs-examine ()
  "Examine directory using a VCS (Version Control System) tool.
If current directory contains CVS/, using cvs;
If current directory contains .svn/, using svn;
If current directory is in magit directory, using magit;
otherwise pop a one-key and let user choose which command will be used."
  (interactive)
  (if current-prefix-arg
      (call-interactively 'one-key-menu-vcs)
   (cond
    ((file-exists-p "CVS/") (call-interactively 'cvs-examine))
    ((file-exists-p ".svn/") (call-interactively 'svn-examine))
    ((magit-get-top-dir default-directory) (call-interactively 'magit-status))
    (t (call-interactively 'one-key-menu-vcs)))))

(define-key global-map "\C-xve" 'zwl-vcs-examine)

(provide 'program-utils)
;;; program-utils.el ends here
