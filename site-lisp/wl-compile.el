;;; wl-compile.el --- A simple auto-compilation implementation

;; Copyright (C) 2009  Zhang Wenliang

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

;; How to use it?
;; (require 'wl-compile)
;; (setq wl-project-makefile-list
;;       '(("/path/to/projectA/" . "/path/to/projectA/makefile/")
;;         ("/path/to/projectB/" . "/path/to/projectB/makefile/")))

;; When wl-compile is called on a buffer:
;; 1. if there is a make file under current buffer's directory use it
;; 2. otherwise, if there is a predefined project for current buffer, use it
;; 3. if 1 and 2 fails, search makefile in top directories to root directory

;;; Code:

(defvar wl-make-file-list '("Makefile" "makefile" "GNUmakefile")
  "Possible makefile names")

(defvar wl-project-makefile-list nil
  "A list which contains project directory and corresponding makefile directory")

(defvar wl-search-end-dir "~/"
  "The directory that wl-find-top-directory stops if not find")

(defun wl-compile (command &optional comint)
  "Compile the program including the current buffer.
First check whether there is makefile in current directory. If there
is, use it; otherwise check whether current buffer's file part of
already defined project, if it is, use the corresponding directory, if
it is not, find Makefile automatically to root directory"
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
  	  (compilation-read-command command)
  	command))
    (consp current-prefix-arg)))
  (unless (equal command (eval compile-command))
    (setq compile-command command))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (setq-default compilation-directory default-directory)
  (compilation-start (concat "cd " (wl-find-makefile-directory wl-make-file-list) ";" command) comint))

(defun wl-find-makefile-directory (files &optional dir)
  "Find the directory contains the Makefile"
  (or dir (setq dir (expand-file-name default-directory)))
  (let ((expanded-files (mapcar #'(lambda (file) (expand-file-name file dir)) files)))
    (if (find-if #'(lambda (file) (file-exists-p file)) expanded-files)
	dir
      (or (wl-get-dir dir)
	  (wl-find-top-directory files dir)))))

(defun wl-get-dir (dir)
  "if dir in wl-project-makefile-list, return the corresponding makefile directory"
  (cdr (find-if #'(lambda (project)
		    (wl-head-substringp (directory-file-name (expand-file-name project))
					(directory-file-name (expand-file-name dir))))
		wl-project-makefile-list :key #'car)))

(defun wl-head-substringp (str1 str2)
  "return t if str1 is a substring of str2 from the beginning"
  (and (<= (length str1) (length str2))
       (string= str1 (substring str2 0 (length str1)))))
  
(defun wl-find-top-directory (files &optional dir)
  "Find the directory which contains one file in FILES
Return nil if not find."
  (or dir (setq dir (expand-file-name default-directory)))
  (let ((expanded-files (mapcar #'(lambda (file) (expand-file-name file dir)) files)))
    (if (find-if #'(lambda (file) (file-exists-p file)) expanded-files)
	dir
      (setq pdir (expand-file-name ".." dir))
      (if (or (string= (file-name-directory pdir) (file-name-directory wl-search-end-dir))
	      (string= (file-name-directory pdir) (file-name-directory dir)))
	  nil
	(wl-find-top-directory files pdir)))))

;; provide the function that process the compilation output
;; put the lines that contain "warning" or "error" to another buffer
(defvar wl-compilation-buffer-name "*wl-compilation*")
(defvar wl-filter-out-regexp nil
  "The regexp that filters out compilation error")

(defun wl-filter-compilation ()
  "Filter the compilation buffer for the lines containing warning or error infos."
  (interactive)
  (let ((buffer (get-buffer "*compilation*"))
	(has-error nil))
    (when buffer
      (with-current-buffer (get-buffer-create wl-compilation-buffer-name)
	(setq buffer-read-only nil)
	(erase-buffer)
	(insert-buffer buffer)
	(delete-non-matching-lines "warning:\\|error:" (point-min) (point-max))
	(when wl-filter-out-regexp
	  (delete-matching-lines wl-filter-out-regexp (point-min) (point-max)))
	(compilation-mode)
	(setq buffer-read-only t)
	(if (= (point-min) (point-max))
	    (message "%s" "Great! No errors and warnings found! Happy hacking!")
	  (setq has-error t)))
      (when has-error
	(switch-to-buffer-other-window (get-buffer wl-compilation-buffer-name))
	(setq default-directory compilation-directory)))))

(provide 'wl-compile)
;;; wl-compile.el ends here
