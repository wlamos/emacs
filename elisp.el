
;;; dired

;; put the directories first at dired mode, from ann77
(defun sof/dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))

(defun my-browse-with-emacs-w3m ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (w3m-browse-url file)))

;;make dired can copy and delete directory
(eval-after-load "dired"
  '(progn
     (require 'dired-x)
     ;; omit hidden files
     (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$")
     
     (setq dired-recursive-copies 'top)
     (setq dired-recursive-deletes 'top)
     ;; M-up to goto parent directory
     (define-key dired-mode-map (kbd "<M-up>") 'dired-up-directory)
     (define-key dired-mode-map (kbd "ESC <up>") 'dired-up-directory)
     (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
     (add-hook 'dired-after-readin-hook 'sof/dired-sort)
     (add-hook 'dired-mode-hook
	       (lambda ()
		 ;; W -> use emacs-w3m to browse the file of current line
		 (define-key dired-mode-map (kbd "W") 'my-browse-with-emacs-w3m)
		 (dired-omit-mode 1)))
     ;; sort function for dired mode, from ann77
     ;; s s : sort by Size of files
     ;; s x : sort by eXtension file name of the files
     ;; s t : sort by Time
     ;; s n : sort by Name
     (add-hook 'dired-mode-hook
	       (lambda ()
		 (interactive)
		 (defvar dired-sort-map nil)
		 (make-local-variable  'dired-sort-map)
		 (setq dired-sort-map (make-sparse-keymap))
		 (define-key dired-mode-map "s" dired-sort-map)
		 (define-key dired-sort-map "s"
		   '(lambda () "sort by Size"
		      (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
		 (define-key dired-sort-map "x"
		   '(lambda () "sort by eXtension"
		      (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
		 (define-key dired-sort-map "t"
		   '(lambda () "sort by Time"
		      (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
		 (define-key dired-sort-map "n"
		   '(lambda () "sort by Name"
		      (interactive) (dired-sort-other (concat dired-listing-switches ""))))))

     (add-hook 'dired-load-hook
	       (lambda ()
		 ;; My preferences for default shell commands
		 ;; set the default command to open a file
		 ;; use ! to open that file in a shell
		 (setq dired-guess-shell-alist-user
		       (list
			'("\\.pl$" "perl")
			'("\\.chm$" "xchm")
			'("\\.mp3$" "mpg321")
			'("\\.rm$" "mplayer")
			'("\\.rmvb$" "mplayer")
			'("\\.avi$" "mplayer")
			'("\\.flv$" "mplayer")
			'("\\.asf$" "mplayer")
			'("\\.wmv$" "mplayer")
			'("\\.mp4$" "mplayer")
			'("\\.3pg$" "mplayer")
			'("\\.htm$" "w3m")
			'("\\.html$" "w3m")
			'("\\.mpg$" "mplayer")
			'("\\.pdf$" "acroread")
			'("\\.doc$" "ooffice")
			'("\\.ppt$" "ooffice")       
			))))))

(autoload 'moccur "color-moccur" "Show all line of all buffers containing a match for REGEXP." t)

(require 'random-color-theme)
(if window-system
    (color-theme-random)
  (color-theme-tty-dark))


;;; ibuffer 
(global-set-key (kbd "C-x C-b") 'ibuffer)

(eval-after-load "ibuffer"
  '(progn
     (setq ibuffer-formats '((mark modified read-only " " (name 32 32 :left :elide) " "
				   (size 6 -1 :right) " " (mode 16 16 :center)
				   " " filename)
			     (mark " " (name 16 -1) " " filename))
	   ibuffer-eliding-string "&")
     (setq ibuffer-show-empty-filter-groups nil)
     (setq ibuffer-saved-filter-groups
	   '(("default"
	      ("version control" (or (mode . svn-status-mode)
				     (mode . svn-log-edit-mode)
				     (name . "^\\*svn-")
				     (name . "^\\*vc\\*$")
				     (name . "^\\*Annotate")
				     (name . "^\\*git-")
				     (name . "^\\*vc-")))
	      ("emacs" (or (name . "^\\*scratch\\*$")
			   (name . "^\\*Messages\\*$")
			   (name . "^TAGS\\(<[0-9]+>\\)?$")
			   (name . "^\\*Help\\*$")
			   (name . "^\\*info\\*$")
			   (name . "^\\*Occur\\*$")
			   (name . "^\\*grep\\*$")
			   (name . "^\\*Compile-Log\\*$")
			   (name . "^\\*Backtrace\\*$")
			   (name . "^\\*Process List\\*$")
			   (name . "^\\*gud\\*$")
			   (name . "^\\*Man")
			   (name . "^\\*WoMan")
			   (name . "^\\*Kill Ring\\*$")
			   (name . "^\\*Completions\\*$")
			   (name . "^\\*tramp")
			   (name . "^\\*shell\\*$")
			   (name . "^\\*compilation\\*$")))
	      ("emacs source" (or (mode . emacs-lisp-mode)
				  (filename . "/Applications/Emacs.app")
				  (filename . "/bin/emacs")))
	      ("agenda" (or (name . "^\\*Calendar\\*$")
			    (name . "^diary$")
			    (name . "^\\*Agenda")
			    (name . "^\\*org-")
			    (name . "^\\*Org")
			    (mode . org-mode)))
	      ("latex" (or (mode . latex-mode)
			   (mode . LaTeX-mode)
			   (mode . bibtex-mode)
			   (mode . reftex-mode)))
	      ("dired" (or (mode . dired-mode))))))

     (add-hook 'ibuffer-mode-hook
	       (lambda ()
		 (ibuffer-switch-to-saved-filter-groups "default")))
     ;; Order the groups so the order is : [Default], [agenda], [emacs]
     (defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups ()
						      activate)
       (setq ad-return-value (nreverse ad-return-value)))
     ))



(require 'browse-kill-ring)
(global-set-key [(control c)(k)] 'browse-kill-ring)
(browse-kill-ring-default-keybindings)



(require 'ido)
(ido-mode t)


;;emacs-w3m
(autoload 'w3m "w3m" "Visit World Wide Web pages using the external w3m command." t)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(setq browse-url-browser-function 'w3m-browse-url)
(eval-after-load "w3m"
  '(progn (setq w3m-command-arguments '("-cookie" "-F")
		w3m-use-cookies t
		w3m-home-page "http://www.google.com"
		w3m-default-display-inline-images t)))

;;(require 'org-install)
;;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)
(setq org-agenda-files '("~/work/github/notes/todo.org"))
(eval-after-load "org"
 '(progn
    (add-hook 'org-mode 'turn-on-font-lock)
    (setq org-log-done t
	  org-todo-keywords '((sequence "TODO(t)" "DOING(i!)" "HANGUP(h!)" "|" "DONE(d!)" "CANCEL(c!)")))
    (require 'org-ext)))
(setq org-publish-project-alist
      '(("notes" :base-directory "~/Dropbox/notes/"
	 :publishing-directory "~/Dropbox/notes_html/"
	 :htmlized-source t)))

;;sdcv mode
(require 'sdcv-mode)
(global-set-key (kbd "C-c s") 'zwl-sdcv-search)


(require 'template)
(template-initialize)
(add-to-list 'template-find-file-commands
             'ido-exit-minibuffer)


;; show the battery info
;;(when (require-maybe 'battery)
;;  (battery)
;;  (display-battery-mode t))

;; If not on AC power line, then display battery status on the mode line
;;(and (require 'battery nil t)
;;     (functionp 'battery-status-function)
;;     (or (equal (cdr (assoc ?L (funcall battery-status-function))) "on-line")
;;	 (display-battery)))


;;uniquify the buffer name, useful
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;; auctex
;(load "auctex.el" nil t t)
;(setq TeX-auto-save t)
;(setq TeX-parse-self t)
;(setq-default TeX-master nil)

;(require 'tex-site)
;(require 'tex)

;(add-to-list 'TeX-command-list
;	     (list "dvipdfmx" "dvipdfmx %s.dvi" 'TeX-run-command nil t))

;(setq TeX-electric-escape t)

;(add-hook 'LaTeX-mode-hook
;          (function (lambda ()
;                      ;(auto-fill-mode)
;                      (outline-minor-mode)
;                      (flyspell-mode)
;                      (tex-source-specials-mode))))


;;; bookmark
(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)



;;; TWiki page editing
(require 'erin)
(add-hook 'erin-mode-hook
	  (lambda ()
	    (auto-fill-mode t)
	    (flyspell-mode 'on)))


;; eim input method
(autoload 'eim-use-package "eim" "Another emacs input method")
(eval-after-load "eim"
  '(progn
     ;; Tooltip 暂时还不好用
     (setq eim-use-tooltip nil)
     (when (require-maybe 'eim-extra)
       ;; 用 ; 暂时输入英文
       (global-set-key ";" 'eim-insert-ascii))))
  
(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "拼音" "汉字拼音输入法" "py.txt")


;;(require 'weblogger)

;; cn-weather
;; (when (require-maybe 'cn-weather)
;;   (setq cn-weather-city "北京")
;;   (cn-weather)
;;   (display-cn-weather-mode t))

;;; One-key settings
;;; How to according current buffer's major mode to select
;;(require 'one-key)
;;(require 'ok-one-key)
;;(require 'ok-gnus)
;;(require 'ok-emms)
;;(require 'ok-svn)
;;(require 'ok-org)

;;(global-set-key (kbd "C-c o") 'one-key-menu-toplevel)

;;; Yasnippet setting
;;(require 'yasnippet)
;;(setq yas/root-directory "~/.emacs.d/snippets")
;;(yas/load-directory yas/root-directory)
 
;;; Integration of one-key and Yasnippet
;;(require 'one-key-yas)

;;(global-set-key (kbd "C-c y") 'one-key-yas/show-mode)
;;(global-set-key (kbd "C-c Y") 'one-key-yas/show-modes)

;; flyspell

(eval-after-load "flyspell"
  '(progn
     (setq flyspell-large-region 100000000)
     (setq ispell-local-dictionary "american")))

(dolist  (hook (list 'message-mode-hook
		     'text-mode-hook
		     'org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 'on))))


;;; easypg，emacs 自带
(require 'epa-file)
(epa-file-enable)
;; 总是使用对称加密
(setq epa-file-encrypt-to nil)
;; 允许缓存密码，否则编辑时每次保存都要输入密码
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; 允许自动保存
(setq epa-file-inhibit-auto-save nil)


(eval-after-load "shell"
  '(progn
     (defun wcy-shell-mode-hook-func  ()
       (set-process-sentinel (get-buffer-process (current-buffer))
			     #'wcy-shell-mode-kill-buffer-on-exit))

     (defun wcy-shell-mode-kill-buffer-on-exit (process state)
       (message "%s" state)
       (if (or
	    (string-match "exited abnormally with code.*" state)
	    (string-match "finished" state))
	   (kill-buffer (current-buffer))))
     
     ;;use ansi color in shell mode
     (setq explicit-shell-file-name "/bin/bash")
     (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
     ;; set the color of ansi-term
     (setq ansi-term-color-vector [unspecified "#262626" "#7fffd4" "#98fb98" "#fbfaa2"
					       "#87ceeb" "#afeeee" "#40e0d0" "#bebebe"])
     (add-hook 'shell-mode-hook #'(lambda ()
				    (ansi-color-for-comint-mode-on)
				    (wcy-shell-mode-hook-func) ;when exit shell, kill the buffer
				    (local-set-key (kbd "<f7>") 'wl-compile)))))


(load-if-exist "~/.emacs.d/mytramp.el")
(load-if-exist "~/.emacs.d/bbdb-config.el")

;;(require 'top-mode)

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
			 ("gnu" . "http://elpa.gnu.org/packages/")))

;; using the following so that xmonad will not catch the ediff frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain) 

;; default to unified diffs
(setq diff-switches "-u")

;; set the grep common 
(setq grep-command "grep -nH -R . -e ")

;;appt
(setq appt-issue-message t)

(autoload 'twit "twittering-mode" nil t)
(eval-after-load "twittering-mode"
  '(progn
     (setq twittering-use-master-password t)
     (twittering-icon-mode 1)))

