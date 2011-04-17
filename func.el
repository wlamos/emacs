;;some funcions and some key bindings
(defun my-insert-date ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))

(defun global-set-keys (&rest keycommands)
  "Register keys to commands."
  (while keycommands
    (let ((key (car keycommands))
          (command (cadr keycommands)))
      (eval `(global-set-key (kbd ,key) (quote ,command))))
    (setq keycommands (cdr (cdr keycommands)))))

(global-set-keys
 "C-c m d"        'my-insert-date
 "C-SPC"          'toggle-input-method
 "M-g"            'goto-line                                 ;; default useless
 "C-o"            'other-window                              ;; handy
 "<f1>"           'shell                                     ;; run a shell in current directory
 "<f2>"           'set-mark-command                          ;; handy
 "<f3>"           'dired                                     ;; handy
 "<f5>"           'info
 "<f7>"           'wl-compile
 "<f4>"           'wl-filter-compilation
 ;; "<f8>"        'run-scheme                                ;; run scheme
 "<f8>"           'slime                              
 "<f9>"           'grep
 "<f10>"          'w3m                                       ;; explorer
 "<f12>"          'ibuffer
 "<f11>"          '(lambda ()
		     (interactive)
		     (server-edit)
		     (suspend-frame))         ;; when finish edit a file from terminal, suspend current frame
 "%"              'match-paren                ;; match paren, use % jump to the matched paren
 ;; "s-s"            'speedbar-get-focus         ;; speed bar get focus
 "C-c t"          'template-expand-template   ;; expand template
 ;; "s-k"            '(lambda ()
 ;; 		     (interactive)
 ;; 		     (kill-buffer (current-buffer))) ;; kill current buffer
 "s-u"            'undo
 "s-y"            'zwl-copy-current-line
 "<home>"         'beginning-of-buffer
 "<end>"          'end-of-buffer
 "C-x k"          'kill-this-buffer  ;; when C-xk, kill this buffer
 "C-c C-o"        'occur ;; find lines in current buffer
 ;;useful when read files, then use Space and BackSpace as C-v M-v
 "C-c C-v"      'view-mode 
 "C-;"          'toggle-comment-current-line
 ;; Becareful all the s- keybindings may conflict with Sawfish
 "<s-f1>"       'bm-toggle
 "<s-f2>"       'bm-next
 "<s-f3>"       'bm-previous
 "<M-up>"       'enlarge-window
 "<M-down>"     'shrink-window
 "<s-f4>"       'smart-compile
 )

;; find the match paren
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;clear the ^M of dos files
(defun dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun unix-dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;;copy current line
(defun zwl-copy-current-line ()
  "Copy current line to kill-ring"
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; View Mode Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; toggle on view mode automatically when buffer is toggled to read only
(setq view-read-only t)

(defun define-mode-maps (the-mode the-mode-maps)
  "Define mode maps for THE-MODE.
THE-MODE-MAPS is an association list. The car of each element is
the keybinding, the cdr of the element is the function."
  (dolist (the-key-map the-mode-maps)
    (define-key the-mode (car the-key-map) (cdr the-key-map))))

(eval-after-load "view"
  '(progn
     (setq view-mode-maps
	   '(("h" . backward-char)
	     ("l" . forward-char)
	     ("j" . next-line)
	     ("k" . previous-line)))
     (define-mode-maps view-mode-map view-mode-maps)))


(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name '*.[ch]pp' | etags -" dir-name)))

(defun goto-percent (percent)
  "Move point to a given percent in the file."
  (interactive "NGoto percent: ")
  (if (= percent 0)
      (goto-char (point-min))
    (goto-char (+ (/ (* (buffer-size) percent) 100) 1))))

(defun my-get-curr-word ()
  (interactive)
  (save-excursion
    (modify-syntax-entry ?_ "w")
    (backward-word 1)
    (let ((beg (point)))
      (forward-word 1)
      (let ((end (point)))
	(copy-region-as-kill beg end)))))

(defun djcb-zoom (n)
  "with positive N, increase the font size, otherwise decrease it"
  (set-face-attribute 'default (selected-frame) :height 
    (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10))))

(global-set-key (kbd "C-+")      '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key (kbd "C--")      '(lambda nil (interactive) (djcb-zoom -1)))

(defun djcb-full-screen-toggle ()
  "toggle full-screen mode"
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(defun get-lists-name ()
  (interactive)
  (let (procs (process-list))
    (dolist (proc procs)
      (princ (process-command proc)))))

(defun find-function-or-variable-at-point (&optional prefix)
  "Find function or variable define at current point."
  (interactive "P")
  (if (equal 0 (variable-at-point))     ;if have not variable at current point
      (if (function-called-at-point)    ;if have function call at current point
          (call-interactively (if (null prefix) 'find-function 'find-function-other-window))
        (if (face-at-point)             ;if have face define at current point
            (call-interactively (if (null prefix) 'find-face-definition 'find-face-definition-other-window))
          (message "Nothing at point.")))
    (call-interactively (if (null prefix) 'find-variable 'find-variable-other-window))))

(defun find-face-definition-other-window (face)
  "Find FACE definition at other window."
  (interactive (find-function-read 'defface))
  (find-function-do-it face 'defface 'switch-to-buffer-other-window))

(defun autocompile ()
  "Compile itself if this is config file"
  (interactive)
  (if (string-match ".*[a-z]+.el$" (buffer-file-name))
      (byte-compile-file (buffer-file-name))))

(add-hook 'after-save-hook 'autocompile)

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

;; kill a process by its name
(defun kill-process-by-name (name)
  (interactive "sProcess name to kill:")
  (let (process (get-process name))
    (when process
      (kill-process process))))


(defun add-custom-global-font-locking ()
  "Hilight some keywords globally."
  (interactive)
  (font-lock-add-keywords nil
			  '(("\\<\\(FIXME\\)" 0 font-lock-warning-face t)
			    ("\\<\\(TODO\\):" 0 font-lock-warning-face t)))
  (font-lock-mode 1))
(add-hook 'find-file-hook 'add-custom-global-font-locking)

