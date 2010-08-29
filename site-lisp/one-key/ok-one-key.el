;;; ok-one-key.el --- one-key setting for one-key

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

;; 

;;; Code:

(defvar one-key-menu-toplevel-alist nil
  "`One-key' menu list for ONE-KEY.")

(setq one-key-menu-toplevel-alist
      '(
	(("e" . "Emms") . one-key-menu-emms)
	(("g" . "Gnus") . one-key-menu-gnus)
	(("o" . "Org") . one-key-menu-org)
	(("s" . "Svn") . one-key-menu-svn)
	))

(defun one-key-menu-toplevel ()
  "`One-Key' menu for ONE-KEY."
  (interactive)
  (one-key-menu "ONE-KEY" one-key-menu-toplevel-alist t))

(provide 'ok-one-key)
;;; ok-one-key.el ends here
