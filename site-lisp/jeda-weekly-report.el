;;; jeda-weekly-report.el --- Do weekly report automatically

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

;; To use it, just put the following into your .el file, which will be loaded

;; (require 'jeda-weekly-report)
;; (setq jeda-weekly-report-file-dir "/home/wenliang/work/weeklyreport/")
;; (setq jeda-weekly-report-my-name "ZhangWenliang")
;; (setq jeda-weekly-report-author-name "Wenliang Zhang")

;; To do report

;; M-x jeda-report

;;; Code:

(defvar jeda-weekly-report-file-dir nil
  "If set, create and search weekly report file on that directory;
otherwise, create and search on current directory")

(defvar jeda-weekly-report-my-name nil
  "The user name that in the report file name")

(defvar jeda-weekly-report-author-name nil
  "The user name that is in the report file's \"Author :\"")

(defun this-monday()
  (let* ((diff (string-to-number (format-time-string "%u" (current-time))))
	 (monday (time-subtract (current-time) (seconds-to-time (* 24 3600 (- diff 1))))))
    monday))

(defun this-sunday()
  (interactive)
  (let* ((diff (string-to-number (format-time-string "%u" (current-time))))
	 (sunday (time-add (current-time) (seconds-to-time (* 24 3600 (- 7 diff))))))
    sunday))

;; the full file name 
(defun get-file-name ()
  (concat jeda-weekly-report-file-dir
	  "Week_Report_Plan_"
	  (format-time-string "%Y-%m-%d" (this-monday))
	  "~"
	  (format-time-string "%Y-%m-%d" (this-sunday))
	  "_"
	  jeda-weekly-report-my-name
	  ".txt"))

(defun create-file-content ()
  (concat "Weekly report "
	  (format-time-string "%Y/%m/%d" (this-monday))
	  "-"
	  (format-time-string "%Y/%m/%d" (this-sunday))
	  "\n"
	  "Project Name :\n"
	  "Author : "
	  jeda-weekly-report-author-name
	  "\n\n"
	  "
summary:

  (1) Key accomplishes this week

  (2) Outstanding issue

  (3) Plan for the coming week
  
  (4) Anything else at your discretion



Daily Report: ( Part-timers need to fill out both 'task' and 'time', full timers only 'task'

Date	         Task	                  	                                Time
-------------------------------------------------------------------------------------------
Mon
-------------------------------------------------------------------------------------------
Tue
-------------------------------------------------------------------------------------------
Wed
-------------------------------------------------------------------------------------------
Thu
-------------------------------------------------------------------------------------------
Fri
-------------------------------------------------------------------------------------------
Sat
-------------------------------------------------------------------------------------------
Sun
-------------------------------------------------------------------------------------------
Time total	
-------------------------------------------------------------------------------------------
"
	  ))

;; Create the report file, if file doesn't exist
(defun create-report-file (report-file-name)
  (find-file report-file-name)
  (insert (create-file-content))
  (goto-char (point-min))
  (save-buffer))

(defun jeda-report()
  (interactive)
  (let ((report-file-name (get-file-name)))
    (if (file-exists-p report-file-name)
	(find-file report-file-name)
      (create-report-file report-file-name))))

(provide 'jeda-weekly-report)
;;; jeda-weekly-report.el ends here
