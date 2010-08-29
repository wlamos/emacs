;;; random-color-theme.el --- 

;; Copyright (C) 2009  Zhang Wenliang

;; Author: Zhang Wenliang <wenliang@jedatechnologies.net>
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

(require 'color-theme)
(color-theme-initialize)
(setq color-theme-history-max-length 10)

(defun color-theme-current-theme()
  (interactive)
  (message (format "Current theme is: %s" 
		   (symbol-name (car (car color-theme-history))))))

(defvar color-theme-random-init nil)

(defvar my-fav-color-themes
  '(
    ;; (color-theme-andreas)
    (color-theme-arjen)
    (color-theme-charcoal-black 10)
    (color-theme-calm-forest 2)
    (color-theme-gnome2 10)
    ;; (give-other-themes-a-chance)
    ))

(defun give-other-themes-a-chance()
  (funcall (car (nth ( random (length color-themes)) color-themes))))

(defun color-theme-random()
  (interactive)
  (unless color-theme-random-init (random t))
  (setq color-theme-random-init t)
  (let (selected-theme (weight-so-far 0) weight)
    (dolist (theme my-fav-color-themes)
      (setq weight (nth 1 theme))
      (unless weight (setq weight 1)) ;; Default 1
      (if (>= (random (+ weight weight-so-far)) weight-so-far)
	  (setq selected-theme (car theme)))
      (setq weight-so-far (+ weight-so-far weight)))
    (if selected-theme
	(funcall selected-theme))
    (message (format "Random color theme: %s" (symbol-name selected-theme)))
    ))

(provide 'random-color-theme)
;;; random-color-theme.el ends here
