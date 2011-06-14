;;;Emacs title bar to reflect file name
(defun frame-title-string ()
  "Return the file name of current buffer, using ~ if under home directory"
  (let ((fname (or
		(buffer-file-name (current-buffer))
		(buffer-name))))
    ;;let body
    (when (string-match (getenv "HOME") fname)
      (setq fname (replace-match "~" t t fname))        )
    fname))

;;(setq frame-title-format '("" system-name " / "(:eval (frame-title-string))))
(setq frame-title-format '("Emacs@"(:eval (frame-title-string))))

;;turn on font-lock mode
(global-font-lock-mode t)

;;always end a file with newline
(setq require-final-newline t)

;;stop at the end of the file not just add lines
(setq next-line-add-newlines nil)

;;shut down voice when error
(setq visible-bell t)

;; shut down welcome message
(setq inhibit-startup-message t)

;;show column number
(setq column-number-mode t)
(setq row-number-mode t)

;;mouse yank when near the cursor
(setq mouse-yank-at-point t)

;;
(setq kill-ring-max 200)

;; using 80 is better
(setq fill-column 80)

;;
(setq major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;
(mouse-avoidance-mode 'animate)

;;
(setq auto-image-file-mode 1)

;;
(and (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(and (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(scroll-bar-mode -1)

;; setup parentheses handling
(show-paren-mode 1)
(setq show-paren-style 'parentheses)

;;set default directory
(setq default-directory "~/")

(setq scroll-step 1
;;      scroll-margin 3
      scroll-conservatively 10000)

;;cursor always in the end when moving from end
(setq track-eol t)

;;display time at mode line
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-use-mail-icon t)
(setq display-time-interval 10)

;;use y or whitespace to say yes, n to no
(fset 'yes-or-no-p 'y-or-n-p)

;;cursor not flicker
(blink-cursor-mode  -1)

;; make passwd invisible in shell mode
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

;;backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq backup-by-copying t)

;;backup vesion control, numbered backup
(setq version-control t)
(setq kept-new-versions 3)
(setq delete-old-versions t)
(setq kept-old-versions 2)
(setq dired-kept-versions 1)
;; also backup the files in version control system
(setq vc-make-backup-files t)

(setq truncate-partial-width-windows nil) 
;;can copy from emacs to other applications
(when (eq window-system 'x)
  (setq x-select-enable-clipboard t))

;; 在退出 emacs 之前确认是否退出
(setq confirm-kill-emacs 'yes-or-no-p)

;;auto insert
;; Auto Insert
;;(setq auto-insert-directory  ;; don't forget last slash!
;;      (expand-file-name "~/.emacs.d/template/"))
(auto-insert-mode t)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-query nil)

;; 在行首 C-k 时，同时删除该行。
(setq-default kill-whole-line t)

;; 当浏览 man page 时，直接跳转到 man buffer。
;;(setq Man-notify-method 'pushy)

(setq initial-scratch-message ";; Happy hacking, man!")

;; M-x customize 功能保存设置的文件。默认的是 Emacs 的初始化文件 .emacs。
;;(setq custom-file "~/.emacs.d/.emacs_custom.el")

;; 设置所在地的经纬度和地名，calendar 可以根据这些信息告知你每天的日出和日落的时间。
(setq calendar-latitude +39.54)
(setq calendar-longitude +116.28)
(setq calendar-location-name "Beijing")

;; Calendar 中 p C 可以看到我们的阴历有中文的天干地支。
(setq calendar-chinese-celestial-stem
      ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
(setq calendar-chinese-terrestrial-branch
      ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])

(setq exec-path (cons "~/bin/" exec-path))
(if (eq system-type 'darwin)
    (setq exec-path (append '("/usr/local/bin"
			      "/opt/local/bin")
			    exec-path)))

