;;; org-ext.el --- Extensions of Org-mode

;; Copyright (C) 2010 

;; Author: 
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
(require 'org)

(defun org/ext-find-file-hook ()
  "When opening a new org-type file, assume first line is a header
Just insert an *."
  (when (and buffer-file-name
             (string-match "\\.org$" buffer-file-name)
             (= (point-min) (point-max)))
      (insert "* ")
      (goto-char (point-max))))

(add-hook 'find-file-hook 'org/ext-find-file-hook)

(defun org/ext-heading-expanded ()
 "Return t if current heading is expanded"
 (let ((goal-column 0) eoh eol eos)
   ;; First, some boundaries
   (save-excursion
     (org-back-to-heading)
     (save-excursion
       (beginning-of-line 2)
       (while (and (not (eobp)) ;; this is like `next-line'
                   (get-char-property (1- (point)) 'invisible))
         (beginning-of-line 2)) (setq eol (point)))
     (outline-end-of-heading)   (setq eoh (point))
     (org-end-of-subtree t)
     (unless (eobp)
       (skip-chars-forward " \t\n")
       (beginning-of-line 1) ; in case this is an item
       )
     (setq eos (1- (point))))
   (< eol eos)))

(defun org/ext-heading-collapsed ()
 "Return t if current heading is collapsed."
 (let ((goal-column 0) eoh eol eos)
   ;; First, some boundaries
   (save-excursion
     (org-back-to-heading)
     (save-excursion
       (beginning-of-line 2)
       (while (and (not (eobp)) ;; this is like `next-line'
                   (get-char-property (1- (point)) 'invisible))
         (beginning-of-line 2)) (setq eol (point)))
     (outline-end-of-heading)   (setq eoh (point))
     (org-end-of-subtree t)
     (unless (eobp)
       (skip-chars-forward " \t\n")
       (beginning-of-line 1) ; in case this is an item
       )
     (setq eos (1- (point))))
   (and (>= eol eos) (/= eos eoh))))

(defun org/ext-collapse ()
 "Collapse if expanded"
 (interactive)
 (if (and (save-excursion (beginning-of-line) (looking-at outline-regexp))
         (bolp))
    ;; At a heading now, check when current heading is expanded
    ;; if it's expanded, Collapse it
    ;; if it's already Collapsed, goto previous sibling
    (if (org/ext-heading-expanded)
        (hide-subtree)
      (outline-previous-visible-heading 1))

  ;; Not at a heading, just call the globally binded command for -
  (call-interactively (global-key-binding "-"))))

(defun org/ext-expand ()
 "Expand if collapsed."
 (interactive)
 (if (and (save-excursion (beginning-of-line) (looking-at outline-regexp))
         (bolp))
     (if (org/ext-heading-collapsed)
         (progn
           (org-show-entry)
           (show-children))
       (outline-next-visible-heading 1))
   ;; Not at a heading, just call the globally binded command for +
   (call-interactively (global-key-binding "+"))))

;; TODO check whether this can be merged with the above function using '+'
(defun org/ext-expand= ()
 "Expand if collapsed."
 (interactive)
 (if (and (save-excursion (beginning-of-line) (looking-at outline-regexp))
         (bolp))
     (if (org/ext-heading-collapsed)
         (progn
           (org-show-entry)
           (show-children))
       (outline-next-visible-heading 1))
   ;; Not at a heading, just call the globally binded command for +
   (call-interactively (global-key-binding "="))))

(defun org/ext-not-in-heading-content-p ()
  "Return t if current point is in a heading but not in heading content."
  (interactive)
  (when (org-on-heading-p)
    (let ((idx (save-excursion
                 (re-search-forward outline-regexp (line-end-position) t)
                 (point))))
      (<= (point) (- idx 2)))))

(defun org/ext-return ()
  "Create new heading if possible.
If cursor is at the end of a heading line, create a new heading at the same level;
else if cursor is at the middle of a heading line, split current heading to
two heading lines, otherwise, the normal enter key."
  (interactive)
  (if (org-on-heading-p) ; at a heading line
      (let ((heads (org-heading-components))
            (level (save-excursion
                     (goto-char (line-beginning-position))
                     (org-outline-level))))
        (if  (eolp)                    ; at end of line
            (if (null (third heads))
                (org-insert-heading-respect-content)
              (org-insert-todo-heading 0))
          ;; not at end of line
          (if (org/ext-not-in-heading-content-p) ; not in the content of heading
              (progn
                (goto-char (line-end-position))
                (if (null (third heads))
                    (org-insert-heading-respect-content)
                  (progn
                    (outline-end-of-subtree)
                    (org-insert-todo-heading 0)
                    (goto-char (line-beginning-position))
                    (while (/= (org-outline-level) level)
                      (org-do-promote))
                    (goto-char (line-end-position)))))
            ;; in content of the heading split the heading
            (if (null (third heads))     ; a todo heading 
                (org-insert-heading t)
              (let ((text (delete-and-extract-region (point) (line-end-position))))
                (org-insert-todo-heading 0)
                (insert text))))))
    (call-interactively 'org-return)))

(defun org/ext-S-return ()
  ""
  (interactive)
  (if (org-on-heading-p) ; at a heading line
      (let ((heads (org-heading-components)))
	(if (eolp)			; at end of line
	    (if (null (third heads))
		(progn
		  (org-insert-heading-respect-content)
		  (org-metaright))
	      (org-insert-todo-subheading 0))
	  ;; not at end of line
	  (if (org/ext-not-in-heading-content-p) ; not in the content of heading
	      (call-interactively 'org-return)
	    ;; in content of the heading split the heading
	    (if (null (third heads))	;a todo heading
		(org-insert-subheading 0)
	      (let ((text (delete-and-extract-region (point) (line-end-position))))
		(org-insert-todo-subheading 0)
		(insert text))))))
    (call-interactively 'org-table-copy-down)))

(defun org/ext-delete-line ()
  "Delete the whole heading if cursor is in a heading line.
Otherwise, delete current line."
  (interactive)
  (if (org-on-heading-p)
      (org/ext-delete-subtree)
    (my-delete-lines)))

(defun org/ext-delete-subtree (&optional n)
  ""
  (interactive "p")
  (let (beg end folded (beg0 (point)))
    (if (interactive-p)
	(org-back-to-heading nil) ; take what looks like a subtree
      (org-back-to-heading t)) ; take what is really there
    (org-back-over-empty-lines)
    (setq beg (point))
    (skip-chars-forward " \t\r\n")
    (save-match-data
      (save-excursion (outline-end-of-heading)
		      (setq folded (org-invisible-p)))
      (condition-case nil
	  (outline-forward-same-level (1- n))
	(error nil))
      (org-end-of-subtree t t))
    (org-back-over-empty-lines)
    (setq end (point))
    (goto-char beg0)
    (when (> end beg)
      (delete-region beg end))))

;; TODO enhance it for cases that current heading has following siblings
;; For those cases, we should move current sub-tree to end of the last siblings
;; then promote current subtree
(defun org/ext-metaleft (&optional arg)
  ""
  (interactive "P")
  (cond
   ((org-at-table-p) (org-call-with-arg 'org-table-move-column 'left))
   ((org-on-heading-p)
    (call-interactively 'org-promote-subtree))
   ((org-region-active-p)
    (call-interactively 'org-do-promote))
   ((org-at-item-p) (call-interactively 'org-outdent-item))
   (t (call-interactively 'backward-word))))

(defun org/ext-demote-subtree ()
  "Only demote the subtree when it's legal.
"
  (interactive)
  (let ((has-prev (save-excursion
                    (outline-back-to-heading)
                    (save-excursion
                      (outline-get-last-sibling)))))
    (when has-prev
      (call-interactively 'org-demote-subtree))))
  
(defun org/ext-metaright (&optional arg)
  ""
  (interactive "P")
  (cond
   ((org-at-table-p) (call-interactively 'org-table-move-column))
   ((org-on-heading-p)
    (call-interactively 'org/ext-demote-subtree))
   ((org-region-active-p)
    (call-interactively 'org-do-demote))
   ((org-at-item-p) (call-interactively 'org-indent-item))
   (t (call-interactively 'forward-word))))

(define-key org-mode-map "-" 'org/ext-collapse)
(define-key org-mode-map "+" 'org/ext-expand)
(define-key org-mode-map "=" 'org/ext-expand=)
(define-key org-mode-map "-" 'org/ext-collapse)
(define-key org-mode-map [(return)] 'org/ext-return)
(define-key org-mode-map [(shift return)] 'org/ext-S-return)
(define-key org-mode-map [(ctrl d)] 'org/ext-delete-line)
(define-key org-mode-map [(meta left)] 'org/ext-metaleft)
(define-key org-mode-map [(meta right)] 'org/ext-metaright)

(provide 'org-ext)
;;; org-ext.el ends here
