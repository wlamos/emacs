;;; some handy functions and KEYBINDINGs

(defun my-mark-lines (num)
  "Mark NUM lines from current line.
If NUM is not specified, by default current line will be marked."
  (interactive "nNumber of lines to mark: ")
  (when (> num 0)
    (goto-char (line-beginning-position))
    (set-mark-command nil)
    (forward-line (1- num))
    (goto-char (line-end-position)))
  (when (< num 0)
    (goto-char (line-end-position))
    (set-mark-command nil)
    (forward-line num)
    (goto-char (line-beginning-position))))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; comment current line
(defun comment-current-line ()
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))


;;; Functions to operate on lines according to Larry's request
(defun my-change-mark ()
  "An internal function, which changes current mark.
If the mark is not over [line-beginning-position, line-end-position], change it."
  (interactive)
  (let ((mark (mark)))
    (if (use-region-p)
	(let ((beg (region-beginning))
	      (end (region-end)))
	  (save-excursion
	    (goto-char beg)
	    (unless (= beg (line-beginning-position))
	      (setq beg (line-beginning-position)))
	    (goto-char end)
	    (unless (= end (line-end-position))
	      (setq end (line-end-position))))
	  (push-mark beg t t)          
	  (goto-char end))
      (push-mark (line-beginning-position) t t)
      (goto-char (line-end-position)))))

(defun my-delete-lines ()
  "Delete current line or the marked lines."
  (interactive)
  (let ((diff (- (point) (line-beginning-position))))
    (my-change-mark)
    (delete-region (region-beginning) (region-end))
    (delete-char 1)
    (when (and (not (eobp))
               (not (eolp)))
      (forward-char diff))))

(defun my-duplicate-lines ()
  "Duplicate current line or the marked lines."
  (interactive)
  (if (not (use-region-p))
      (let ((point (point))
            (col (current-column)))
        (my-change-mark)
        (save-excursion
          (goto-char (region-end))
          (if (eobp)
              (let ((end (point)))
                (newline)
                (goto-char end)))
          (forward-line)
          (insert (buffer-substring (region-beginning) (region-end))))
        (goto-char point)
        (forward-line)
        (move-to-column col))
    ;; active region exists
    (let ((beg (region-beginning))
          (end (region-end))
          (col (current-column)))
      (my-change-mark)
      (save-excursion
        (goto-char end)
        (if (eobp)
            (let ((tmpend (point)))
              (newline)
              (goto-char tmpend)))
        (forward-line)
        (insert (buffer-substring (region-beginning) (region-end))))
      (goto-char beg)
      (push-mark beg t t)
      (goto-char end)
      (setq deactivate-mark nil))))

(defun shift-text-internal (distance)
  (cond
   ((and mark-active transient-mark-mode)
    (let (point-changed)
      (when (> (point) (mark))
        (exchange-point-and-mark)
        (setq point-changed t))
      (let* ((column (current-column))
             (end-column (save-excursion
                           (goto-char (region-end))
                           (current-column)))
             (beg (save-excursion
                    (goto-char (point))
                    (line-beginning-position)))
             (end (save-excursion
                    (goto-char (region-end))
                    (if (not (eobp))
                        (1+ (line-end-position))
                      (line-end-position))))
             (mark (mark)))
        (save-excursion
          (indent-rigidly beg end distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))

      (if point-changed
          (exchange-point-and-mark))))
   (t
    (let ((column (current-column)))
      (ignore-errors
        (indent-rigidly (line-beginning-position) (line-end-position) distance)
        (move-to-column (+ column distance))
        (setq deactivate-mark t))
      ))))

(defun move-text-left (arg)
  (interactive "*p")
  (shift-text-internal (* tab-width (- arg))))

(defun move-text-right (arg)
  (interactive "*p")
  (shift-text-internal (* tab-width arg)))

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let* ((column (current-column))
           (end-column (save-excursion
                         (goto-char (region-end))
                         (current-column)))
           (beg (save-excursion
                  (goto-char (point))
                  (line-beginning-position)))
           (end (save-excursion
                  (goto-char (region-end))
                  (if (not (eobp))
                      (1+ (line-end-position))
                    (line-end-position))))
           (text (delete-and-extract-region beg end))
           tmpbeg)
      (forward-line arg)
      (setq tmpbeg (point))
      (goto-char (line-beginning-position))
      (insert text)
      (forward-line -1)
      (move-to-column end-column t)
      (set-mark (point))
      (goto-char tmpbeg)
      (move-to-column column t)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun move-text-home ()
  "Move region (transient-mark-mode active) or current line to
begin of the buffer."
  (interactive)
  (if (and mark-active transient-mark-mode)
      (progn
        (if (> (point) (mark))
            (exchange-point-and-mark))
        (move-text-internal (1+ (- (line-number-at-pos)))))
    (move-text-internal (1+ (- (line-number-at-pos))))))

(defun move-text-end ()
  "Move region (transient-mark-mode active) or current line to
end of the buffer."
  (interactive)
  (if (and mark-active transient-mark-mode)
      (progn
        (if (> (point) (mark))
            (exchange-point-and-mark))
        (move-text-internal (1- (count-lines (point) (point-max)))))
    (move-text-internal (1- (count-lines (point) (point-max))))))

(defun move-text-pageup ()
  "Move region (transient-mark-mode active) or current line 
pageup."  
  (interactive)
  
  )

(defun move-text-pagedown ()
  "Move region (transient-mark-mode active) or current line 
pagedown."
  (interactive)
  )


;; comment-a-block
;; A-;
;; C-;
(defun global-set-keys (&rest keycommands)
  "Register keys to commands."
  (while keycommands
    (let ((key (car keycommands))
          (command (cadr keycommands)))
      (eval `(global-set-key (kbd ,key) (quote ,command))))
    (setq keycommands (cdr (cdr keycommands)))))

(defun gtd ()
  (interactive)
  (find-file "~/_db/org/home.org"))

(global-set-keys
 "C-c g"     'gtd
 "M-g"       'goto-line
 "C-o"       'other-window
 "<f10>"     'ibuffer
 "%"         'match-paren
 "C-x k"     'kill-this-buffer          ; when C-xk, kill this buffer
 "C-;"       'comment-current-line
 "C-d"       'my-delete-lines           ; delete lines
 "M-d"       'my-duplicate-lines        ; duplicate lines
 "<M-left>"  'move-text-left
 "<M-right>" 'move-text-right
 "<M-up>"    'move-text-up
 "<M-down>"  'move-text-down
 "<M-home>"  'move-text-home
 "<M-end>"   'move-text-end
 "<delete>"  'delete-char
 )
