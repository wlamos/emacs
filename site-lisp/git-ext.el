;;; git-ext.el --- Some extension functions for using git in Emacs

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

(defun git/ext-call-process (buffer &rest args)
  "Wrapper for call-process that sets environment strings."
  (apply #'call-process "git" nil buffer nil args))

(defun git/ext-call-process-display-error (&rest args)
  "Wrapper for call-process that displays error messages."
  (let* ((dir default-directory)
         (buffer (get-buffer-create "*Git Command Output*"))
         (ok (with-current-buffer buffer
               (let ((default-directory dir)
                     (buffer-read-only nil))
                 (erase-buffer)
                 (eq 0 (apply #'git/ext-call-process (list buffer t) args))))))
    (unless ok (display-message-or-buffer buffer))
    ok))

(defun git/ext-call-process-string (&rest args)
  "Wrapper for call-process that returns the process output as a string,
or nil if the git command failed."
  (with-temp-buffer
    (and (eq 0 (apply #'git/ext-call-process t args))
         (buffer-string))))

(defun git/ext-call-process-string-display-error (&rest args)
  "Wrapper for call-process that displays error message and returns
the process output as a string, or nil if the git command failed."
  (with-temp-buffer
    (if (eq 0 (apply #'git/ext-call-process (list t t) args))
        (buffer-string)
      (display-message-or-buffer (current-buffer))
      nil)))

(defun git/ext-add-file (file)
  "Add FILE to stage area."
  (apply 'git/ext-call-process-display-error "update-index" "--add" "--" file))

(defun git/ext-auto-push-file (file)
  "Push file automatically."
  (interactive "f")
  (git/ext-add-file file))

(defun git/ext-push-current-file ()
  ""
  (interactive)
  (git/ext-add-file (buffer-file-name)))
(provide 'git-ext)
;;; git-ext.el ends here
