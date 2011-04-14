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
 "Collapse the heading if it's expanded"
 (interactive)
 (if (and (save-excursion (beginning-of-line) (looking-at outline-regexp))
         (bolp))
    ;; At a heading now, check whether current heading is expanded
    ;; if it's expanded, Collapse it
    ;; if it's already Collapsed, goto previous sibling
    (if (org/ext-heading-expanded)
        (hide-subtree)
      (outline-previous-visible-heading 1))

  ;; Not at a heading, just call the globally binded command for -
  (call-interactively (global-key-binding "-"))))

(defun org/ext-expand+ ()
  "Using + to navigate all headings when it's "
  (interactive)
  (org/ext-expand-1 "+"))

(defun org/ext-expand= ()
  "Using = to navigate all headings"
  (interactive)
  (org/ext-expand-1 "="))

(defun org/ext-expand-1 (keys)
  "
If KEYS is pressed when point is at the begining of a heading,
expand current heading if it's collapsed"
  (if (and (bolp)
	   (save-excursion (beginning-of-line) (looking-at outline-regexp)))
      (if (org/ext-heading-collapsed)
	  (progn
	    (org-show-entry)
	    (show-children))
	(outline-next-visible-heading 1))
    ;; Not at the beginning of a heading, call the global binding for KEYS
    (call-interactively (global-key-binding keys))))

(defun org/ext-not-in-heading-content-p ()
  "Return t if current point is in a heading but not in heading content."
  (interactive)
  (when (org-on-heading-p)
    (let ((idx (save-excursion
                 (re-search-forward outline-regexp (line-end-position) t)
                 (point))))
      (<= (point) (- idx 2)))))

(defun org/ext-adjust-level (level)
  "Adjust (promote/demote) current heading to level `level'.
When this function is called, the point must be at a heading line
"
  (goto-char (line-beginning-position))
  (let ((curr-level (org-outline-level)))
    (while (/= (org-outline-level) level)
      (if (> level curr-level)
          (org-do-demote)
        (org-do-promote))))
  (goto-char (line-end-position)))

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
                     (org-outline-level)))
            (moveup (bolp)))
        (cond
         ((or (eolp) (bolp))
          (outline-end-of-subtree)
          (if (null (third heads))
              (org-insert-heading-respect-content)
            (org-insert-todo-heading 0))
          (org/ext-adjust-level level)
          
          (when moveup                  ; when cursor is at the beginning of the line, move it up
            (org-move-subtree-up)
            (goto-char (line-end-position))))
         (t
          (if (org/ext-not-in-heading-content-p) ; not in the content of heading
              (progn
                (goto-char (line-end-position))
                (if (null (third heads))
                    (org-insert-heading-respect-content)
                  (progn
                    (outline-end-of-subtree)
                    (org-insert-todo-heading 0)
                    (org/ext-adjust-level level))))
            ;; in content of the heading split the heading
            (if (null (third heads))     ; a todo heading 
                (org-insert-heading t)
              (let ((text (delete-and-extract-region (point) (line-end-position))))
                (org-insert-todo-heading 0)
                (insert text)))))))
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
	      (progn
		(outline-end-of-subtree)
		(org-insert-todo-subheading 0)))
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

(defun org/ext-point-at-heading-preface-p ()
  "Return t if point is at heading's preface, i.e not in the content of heading"
  (and (org-on-heading-p)
       (let ((heads (org-heading-components))
	     (col (current-column)))
	 (if (third heads)		; A TODO heading
	     (if (fourth heads)		; A TODO heading with priority
		 (save-excursion
		   (goto-char (line-beginning-position))
		   (re-search-forward "\\[#.\\]" (line-end-position))
		   (skip-chars-forward " \t")
		   (< col (current-column)))
	       (save-excursion
		 (goto-char (line-beginning-position))
		 (re-search-forward (concat "\\("
				     (mapconcat 'regexp-quote org-todo-keywords-1 "\\|")
				     "\\)")
				    (line-end-position))
		 (skip-chars-forward " \t")
		 (< col (current-column))))
	   ;; A normal heading
	   (if (fourth heads)		; A normal heading with priority
	       (save-excursion
		 (goto-char (line-beginning-position))
		 (re-search-forward "\\[#.\\]" (line-end-position))
		 (skip-chars-forward " \t")
		 (< col (current-column)))
	     ;; A normal heading without priority
	     (save-excursion
	       (goto-char (line-beginning-position))
	       (re-search-forward "\\(\\*+\\)[ \t]+" (line-end-position))
	       (skip-chars-forward " \t")
	       (< col (current-column))))))))

(defun org/ext-delete ()
  "Delete the whole subtree when cursor is in heading line
Otherwise, delete one char."
  (interactive)
  (if (org/ext-point-at-heading-preface-p)
      (org/ext-delete-subtree)
    (org-delete-char 1)))

(defun org/ext-delete-subtree (&optional n)
  ""
  (interactive "p")
  (let (beg end folded (beg0 (point)))
    (if (called-interactively-p 'interactive)
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

(defun org/ext-promote-subtree ()
  "Promote current subtree
Move current subtree down and promote it if it has following siblings."
  (interactive)
  (let ((level (save-excursion
                 (goto-char (line-beginning-position))
                 (org-outline-level))))
    (when (> level 1)
      (while (save-excursion
               (outline-get-next-sibling))
        (call-interactively 'org-move-subtree-down))
      (call-interactively 'org-promote-subtree))))

(defun org/ext-metaleft (&optional arg)
  ""
  (interactive "P")
  (cond
   ((org-at-table-p) (org-call-with-arg 'org-table-move-column 'left))
   ((org-on-heading-p)
    (call-interactively 'org/ext-promote-subtree))
   ((org-region-active-p)
    (call-interactively 'org-do-promote))
   ((org-at-item-p) (call-interactively 'org-outdent-item))
   (t (call-interactively 'backward-word))))

(defun org/ext-demote-subtree ()
  "Only demote the subtree when it's legal."
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

(defun org/ext-ctrl-return ()
  "Insert a non-heading line"
  (interactive)
  (when (org/ext-not-in-heading-content-p)
    (goto-char (line-end-position)))
  (call-interactively 'org-return-indent))

(defun org/ext-ctrl-w ()
  "Cut current subtree if called at a heading line and there is no selected region
If called with cursor not at a heading line or having selected region, calling `kill-region"
  (interactive)
  (if (and (org-at-heading-p)
           (not (use-region-p)))
      (call-interactively 'org-cut-subtree)
    (call-interactively 'kill-region)))

(defun org/ext-meta-w ()
  "Copy current subtree if called at a heading line and there is no selected region
If called with cursor not at a heading line or having selected region,
calling the default command"
  (interactive)
  (if (and (org-at-heading-p)
	   (not (use-region-p)))
      (call-interactively 'org-copy-subtree)
    (call-interactively 'kill-ring-save)))

(defun org/ext-meta-d ()
  "If cursor is in a heading, duplicate tree; otherwise duplicate current line."
  (interactive)
  (if (org-at-heading-p)
      (progn
        (call-interactively 'org-copy-subtree)
        (let ((col (current-column)))
          (save-excursion
            (goto-char (line-beginning-position))
            (call-interactively 'org-paste-subtree))
          (move-to-column col)))
    (call-interactively 'my-duplicate-lines)))

;;;###autoload
(defun org/ext-tab (&optional arg)
  "Refined tab key."
  (interactive "P")
  (if (use-region-p)
      (let* ((beg (if (< (region-beginning) (region-end))
                      (region-beginning)
                    (region-end)))
             (end (if (< (region-beginning) (region-end))
                      (region-end)
                    (region-beginning)))
             (beg-lb (save-excursion
                       (goto-char beg)
                       (line-beginning-position)))
             (end-le (save-excursion
                       (goto-char end)
                       (line-end-position)))
             (beg-lb-at-heading (save-excursion
                                  (goto-char beg-lb)
                                  (org-at-heading-p))))
        (if (and (<= (line-beginning-position) beg)
                 (<= end (line-end-position)))
            ;; marked region at the same line, delete the marked region and insert tab
            (progn
              (delete-region beg end)
              (insert ?\t))
          ;; marked region not at the same line
          (if (and beg-lb-at-heading
                   (save-excursion
                     (outline-get-last-sibling)))
              ;; demote all the marked headings
              (save-excursion
                (my-change-mark)
                (org-do-demote)
                (setq deactivate-mark nil))
            ;; check whether there is a heading in the marked resions
            ;; if there is no heading, shift the marked region right
            (when (save-excursion
                    (goto-char beg-lb)
                    (outline-next-heading)
                    (> (point) end-le))
              (move-text-right 1)))))
    (if (org-at-table-p)
        (call-interactively 'org-cycle)
      (insert ?\t))))

;;;###autoload
(defun org/ext-shifttab (&optional arg)
  "Refined shift-tab key."
  (interactive "P")
  (if (use-region-p)
      (let* ((beg (if (< (region-beginning) (region-end))
                      (region-beginning)
                    (region-end)))
             (end (if (< (region-beginning) (region-end))
                      (region-end)
                    (region-beginning)))
             (beg-lb (save-excursion
                       (goto-char beg)
                       (line-beginning-position)))
             (end-le (save-excursion
                       (goto-char end)
                       (line-end-position)))
             (beg-lb-at-heading (save-excursion
                                  (goto-char beg-lb)
                                  (org-at-heading-p))))
        
        ;; if marked region not at the same line and the first marked line is a heading line
        ;; promote the marked region if possible
        (if (and beg-lb-at-heading
                 (save-excursion
                   (goto-char end-le)
                   (> (line-beginning-position) beg-lb)))
            (save-excursion
              (my-change-mark)
              (org-do-promote)
              (setq deactivate-mark nil))
          ;; if there is no heading in the marked region, shift the region left
          (when (save-excursion
                  (goto-char beg-lb)
                  (outline-next-heading)
                  (> (point) end-le))
            (move-text-left 1))))
    (call-interactively 'org-shifttab)))

(define-key org-mode-map "-" 'org/ext-collapse)
(define-key org-mode-map "+" 'org/ext-expand+)
(define-key org-mode-map "=" 'org/ext-expand=)
(define-key org-mode-map "-" 'org/ext-collapse)
(define-key org-mode-map [(return)] 'org/ext-return)
(define-key org-mode-map [(shift return)] 'org/ext-S-return)
(define-key org-mode-map [(ctrl d)] 'org/ext-delete-line)
(define-key org-mode-map [(delete)] 'org/ext-delete)
(define-key org-mode-map [(meta left)] 'org/ext-metaleft)
(define-key org-mode-map [(meta right)] 'org/ext-metaright)
(define-key org-mode-map [(ctrl return)] 'org/ext-ctrl-return)
(define-key org-mode-map [(ctrl w)] 'org/ext-ctrl-w)
(define-key org-mode-map [(meta w)] 'org/ext-meta-w)
(define-key org-mode-map [(meta d)] 'org/ext-meta-d)
(define-key org-mode-map "\C-c\C-xc" 'org/ext-meta-d)
(define-key org-mode-map [(tab)] 'org/ext-tab)

(unless (featurep 'xemacs)
  (org-defkey org-mode-map [S-iso-lefttab]  'org/ext-shifttab))
(org-defkey org-mode-map [(shift tab)]    'org/ext-shifttab)
(define-key org-mode-map [backtab] 'org/ext-shifttab)

(defun test-promote ()
  (interactive)
  (org-do-promote))
(provide 'org-ext)
;;; org-ext.el ends here
