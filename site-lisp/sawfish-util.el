;;; sawfish-util.el --- Some Sawfish utils

;; Copyright (C) 2011  Wenliang Zhang

;; Author: Wenliang Zhang <wlamos@gmail.com>
;; Keywords: tools, lisp

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

;; Installation

;; 1. Put this el file to your `load-path'

;; 2. Put the following to your .emacs file:
;;
;; (require 'sawfish-util)
;;
;; 3. Configure the directories containing the sawfish lisp files, e.g.
;;    - the directory contains the sawfish lisp files
;;    - the directory contains the rep lisp files
;;    - the directory contains your ownself lisp files
;;
;; The following is an example setting:
;;
;; (setq sawfish-lisp-dir '("~/.sawfish/"
;;			    "/usr/local/share/sawfish/1.8.0/lisp"
;;			    "/usr/local/share/rep/0.91.1/lisp"))

;; List of commands
;;
;; - sawfish-jump-to-function-def, which is bound to M-. f
;;
;; - sawfish-jump-to-variable-def, which is bound to M-. v
;;

;;; Code:
(require 'sawfish)

(defvar sawfish-lisp-dir '("~/.sawfish")
  "A list of paths that contain sawfish lisp codes.")

(defun sawfish-filter-legal-dir (dirs)
  "Filter out the illegal directories in DIR.
A directory is illegal when it's not a directory or it's not readable for
current user."
  (remove-if-not #'(lambda (dir)
		     (when (and (file-directory-p dir)
				(file-readable-p dir))
		       dir))
		 dirs))

;; Stolen from sawfish.el
(defun sawfish-find-ask (default description lookups)
  "Ask the user for a symbol.

The symbol will be described as DESCRIPTION with a completing read using
LOOKUPS for the completion. DEFAULT should be a function that returns a
default value for the read."
  (sawfish-load-symbols)
  (intern (completing-read (format "Find sawfish %s: " description)
                           (symbol-value lookups)
                           nil
                           t
                           (funcall default))))

(defun sawfish-find-ask-function ()
  "Ask for a function name."
  (sawfish-find-ask 'sawfish-funcall-at-point "function" 'sawfish-function-list))

(defun sawfish-find-ask-variable ()
  "Ask for a variable name."
  (sawfish-find-ask 'sawfish-variable-at-point "variable" 'sawfish-variable-list))

(defun sawfish-quote-func (func)
  "Return the regexp to grep FUNC."
  (concat "\"\\([ \t]*(define[ \t]+\\([ \t]*"
	  (symbol-name func)
	  "([ \t]*\\)|[ \t]+)|defmacro[ \t]+"
	  (symbol-name func)
	  "[ \t]+\\()\""))

(defun sawfish-find-function (func dirs)
  "Find FUNC under directory DIRS."
  (grep (concat "egrep " (sawfish-quote-func func) " -nH --exclude=\"*.jlc\" -R " dirs)))

(defun sawfish-quote-var (var)
  "Return the regexp to grep variable VAR."
  (concat "\"\\([ \t]*(define|defvar-setq)[ \t]+" (symbol-name var) "[ \t]*([ \t]|$)\""))

(defun sawfish-find-variable (var dirs)
  "Find VAR under directory DIRS"
  (grep (concat "egrep " (sawfish-quote-var var) " -nH --exclude=\"*.jlc\" -R " dirs)))

;;;###autoload
(defun sawfish-jump-to-function-def (function)
  "Jump to definition of FUNCTION."
  (interactive (list (sawfish-find-ask-function)))
  (sawfish-find-function function (mapconcat #'(lambda (a) a)
					     (sawfish-filter-legal-dir sawfish-lisp-dir)
					     " ")))

;;;###autoload
(defun sawfish-jump-to-variable-def (var)
  "Jump to definition of variable VAR"
  (interactive (list (sawfish-find-ask-variable)))
  (sawfish-find-variable var (mapconcat #'(lambda (a) a)
					(sawfish-filter-legal-dir sawfish-lisp-dir)
					" ")))

;; Keybindings
(define-key sawfish-mode-map [(meta .) ?f] 'sawfish-jump-to-function-def)
(define-key sawfish-mode-map [(meta .) ?v] 'sawfish-jump-to-variable-def)

(provide 'sawfish-util)
;;; sawfish-util.el ends here
