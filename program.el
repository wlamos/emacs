
(setq auto-mode-alist
      (append '(("\\.cpp$" . c++-mode)
		("\\.hpp$" . c++-mode)
		("\\.h$" . c++-mode)
		("\\.lsp$" . list-mode)
		("\\.scm$" . scheme-mode)
		("\\.pl$" . perl-mode)
		("\\.py$" . python-mode)
		("\\.l$" . flex-mode)
		("\\.y$" . bison-mode)
		("\\.tr$" . text-mode)
		("\\.tr.i$" . text-mode)
		("Make.apache" . makefile-mode)
		("Make.exe.apache" . makefile-mode)
		) auto-mode-alist))


(autoload 'company-mode "company" nil t)
(setq company-backends nil)



(autoload 'flex-mode "flex-mode")
(autoload 'bison-mode "bison-mode.el")


;;; C/C++ dev setting
;; Load CEDET
(eval-after-load "cc-mode"
  '(progn
     (require 'cedet)
     ;;(require-maybe 'semantic-gcc)
     ;;(load (concat my-config-dir "site-lisp/cedet-1.0/common/cedet.el"))
     ;; Enable EDE (Project Management) features
     (global-ede-mode 1)
     (semantic-load-enable-code-helpers)
     (semantic-load-enable-semantic-debugging-helpers)
     (setq semantic-load-turn-everything-on t)
     (define-key-after (lookup-key global-map [menu-bar tools])
       [speedbar] '("Speedbar". speed-frame-mode) [calendar])
     (setq semantic-idle-scheduler-idle-time 432000)
     
     ;;the directory of semantic files
     (setq semanticdb-default-save-directory (expand-file-name "~/.emacs.d/backup/semantic.cache/semanticdb"))
     (defconst cedet-user-include-dirs
       (list "./"))

     (require 'semantic-c nil 'noerror)
     (let ((include-dirs cedet-user-include-dirs))
       (mapc (lambda (dir)
	       (semantic-add-system-include dir 'c++-mode)
	       (semantic-add-system-include dir 'c-mode))
	     include-dirs))
     ;;auto complete
     (defun my-indent-or-complete ()
       (interactive)
       (if (looking-at "\\>")
	   (hippie-expand nil)
	 (indent-for-tab-command)))
     
     (global-set-key [(control tab)] 'my-indent-or-complete)
     (autoload 'senator-try-expand-semantic "senator")
     (setq hippie-expand-try-functions-list
	   '(
	     senator-try-expand-semantic
	     try-expand-dabbrev
	     try-expand-dabbrev-visible
	     try-expand-dabbrev-all-buffers
	     try-expand-dabbrev-from-kill
	     try-expand-list
	     try-expand-list-all-buffers
	     try-expand-line
	     try-expand-line-all-buffers
	     try-complete-file-name-partially
	     try-complete-file-name
	     try-expand-whole-kill
	     ))
     
     (defun my-c-mode-common-hook()
       ;; the number of columns CC Mode indents nested code
       (setq c-basic-offset 4)
       (setq tab-width 4 indent-tabs-mode nil)
       ;;hungry-delete and auto-newline
       (c-toggle-auto-hungry-state 1)
       ;;key bindings
       (define-key c-mode-base-map [(control \`)] 'hs-toggle-hiding)
       ;;insert a line break suitable to the context, like newline-and-indent, more powerful
       (define-key c-mode-base-map [(return)] 'c-context-line-break)
       (define-key c-mode-base-map [(f7)] 'wl-compile)
       (define-key c-mode-base-map [(meta \`)] 'c-indent-command)
       (define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)
       ;;preprocessor
       (setq c-macro-shrink-window-flag t)
       (setq c-macro-preprocessor "cpp")
       (setq c-macro-cppflags " ")
       (setq c-macro-prompt-flag t)
       (setq c-electric-pound-behavior '(alignleft))
       (setq abbrev-mode t)
       (c-set-offset 'inline-open 0)
       ;;  (c-set-offset 'inline-close 0)
       (c-set-offset 'friend '-)
       (c-set-offset 'substatement-open 0))
     (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

     ;;cpp edit stratigies
     (defun my-c++-mode-hook()
       (setq c-basic-offset 4)
       (setq tab-width 4 indent-tabs-mode nil)
       (c-set-style "stroustrup")
       (c-set-offset 'inline-open 0)
       ;; (c-set-offset 'inline-close 0)
       (c-set-offset 'friend 0)
       (c-set-offset 'substatement-open 0)
       (setq c-electric-pound-behavior '(alignleft)) ;; when # is the first character, alignleft
       (add-to-list 'c-cleanup-list 'empty-defun-braces)
       (add-to-list 'c-cleanup-list 'defun-close-semi)
       (company-mode)
       (make-local-variable 'company-backends)
       (setq company-backends '((company-keywords company-semantic))))

     ;; (add-hook 'c++-mode-hook 'yas/minor-mode)
     (add-hook 'c++-mode-hook 'my-c++-mode-hook)

     ;;auto load the index of /usr/include created by semantic
     (setq semanticdb-search-system-databases t)
     (add-hook 'c-mode-common-hook
	       (lambda ()
		 (setq semanticdb-project-system-databases
		       (list (semanticdb-create-database
			      semanticdb-new-database-class
			      "/usr/include")))))
     ))


;;; hs-minor-mode
(dolist (hook (list 'c-mode-common-hook
		    'emacs-lisp-mode-hook
		    'lisp-mode-hook
		    'perl-mode-hook
		    'sh-mode-hook))
  (add-hook hook 'hs-minor-mode))


;;verilog mode
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(setq auto-mode-alist (cons  '("\\.\\(v\\|dv\\|sv\\)\\'" . verilog-mode) auto-mode-alist))

(eval-after-load "verilog-mode"
  '(progn
     (setq verilog-indent-level             3
	   verilog-indent-level-module      3
	   verilog-indent-level-declaration 3
	   verilog-indent-level-behavioral  3
	   verilog-indent-level-directive   1
	   verilog-case-indent              2
	   verilog-auto-newline             t
	   verilog-auto-indent-on-newline   t
	   verilog-tab-always-indent        t
	   verilog-auto-endcomments         t
	   verilog-minimum-comment-distance 40
	   verilog-indent-begin-after-if    t
	   verilog-auto-lineup              '(all))))


;; nxml-mode
;;(require 'rng-auto)
(load "rng-auto.el")
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
	    auto-mode-alist))

(eval-after-load "nxml-mode"
  '(progn
     ;;nxml auto-indent
     (add-hook 'nxml-mode-hook
	       '(lambda ()
		  (local-set-key (kbd "RET") 'newline-and-indent)))))



;;python mode
(setq interpreter-mode-alist (cons '("python" . python-mode)
				   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

(eval-after-load "python-mode"
  '(progn
     ;;python tab (ropemacs)
     (defun python-indent-or-expand (arg)
       "Either indent according to mode, or expand the word preceding point."
       (interactive "*P")
       (if (and
	    (or (bobp) (= ?w (char-syntax (char-before))))
	    (or (eobp) (not (= ?w (char-syntax (char-after))))))
	   (rope-code-assist arg)
	 (indent-according-to-mode)))
     
     (defun load-ropemacs ()
       "Load pymacs and ropemacs"
       (interactive)
       (require 'pymacs)
       (autoload 'pymacs-load "pymacs" nil t)
       (autoload 'pymacs-eval "pymacs" nil t)
       (autoload 'pymacs-apply "pymacs")
       (autoload 'pymacs-call "pymacs")
       (autoload 'pymacs-exec "pymacs" nil t)
       (pymacs-load "ropemacs" "rope-")
       (local-set-key [(meta ?/)] 'rope-code-assist)
       (local-set-key [tab] 'python-indent-or-expand)
       (setq rope-confirm-saving 'nil))
     
     (add-hook 'python-mode-hook 'load-ropemacs)))

;;define some keys
(eval-after-load "comint"
  '(progn
     (define-key comint-mode-map [(meta p)] 'comint-previous-matching-input-from-input)
     (define-key comint-mode-map [(meta n)] 'comint-next-matching-input-from-input)
     (define-key comint-mode-map [up] 'comint-next-input)
     (define-key comint-mode-map [down] 'comint-previous-input)))


;; doxymacs
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; always use the external parser, this is much faster
(setq doxymacs-use-external-xml-parser t)
;; use w3m to browse the doc
(defun doxymacs-display-url (root url)
  "Displays the given match."
  (w3m-browse-url (concat root "/" url)))

;; comment current line
(defun toggle-comment-current-line ()
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; smart-compile
(require-maybe 'smart-compile)


;;; Common Lisp Dev
;;; SLIME setting
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(if (eq system-type 'darwin)
    (progn
     (setq inferior-lisp-program "~/bin/sbcl")
     (setenv "PATH" (concat "~/bin" ":" (getenv "PATH")))
     (setenv "SBCL_HOME" "/Users/wenliang/lib/sbcl"))
  (setq inferior-lisp-program "/usr/bin/sbcl"))
(slime-setup '(slime-fancy slime-scratch slime-editing-commands slime-asdf slime-repl))
;; (setq slime-lisp-implementations
;;            '((sbcl ("/usr/bin/sbcl" "--core" "/home/velen/.emacs.d/sbcl.core-with-swank")
;; 		   :init (lambda (port-file _)
;; 			   (format "(swank:start-server %S)\n" port-file)))
;; 	     (cmucl ("cmucl" "-quiet"))))

(defun my-slime-tab ()
  " "
  (interactive)
  (if (or (bolp)
	  (= (char-before) ?\t)
	  (= (char-before) ?\ ))
      (indent-for-tab-command)
    (slime-complete-symbol)))

(add-hook 'slime-mode-hook
	  (lambda ()
	    (local-set-key [(tab)] 'my-slime-tab)))

(setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")

(defun scratch-lisp-file ()
  "Insert a template (with DEFPACKAGE and IN-PACKAGE forms) into
  the current buffer."
  (interactive)
  (goto-char 0)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (package (file-name-sans-extension file)))
    (insert ";;;; " file "\n")
    (insert "\n(defpackage #:" package "\n  (:use #:cl))\n\n")
    (insert "(in-package #:" package ")\n\n")))


;;; elisp dev
(require 'eldoc)
(setq eldoc-idle-delay 0)               ;显示延迟
(setq eldoc-argument-case 'upcase) ;高亮函数参数

(defun my-elisp-indent-or-complete ()
  ""
  (interactive)
  (if (or (bolp)
	  (= (char-before) ?\t)
	  (= (char-before) ?\ ))
      (indent-for-tab-command)
    (lisp-complete-symbol)))

(dolist (hook (list
               'ielm-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'message-mode-hook
               'Info-mode-hook
               'erc-mode-hook
               'org-mode-hook
	       ))
  (add-hook hook 'turn-on-eldoc-mode))

(autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)

(eval-after-load "lisp-mode"
  '(progn
     ;; Paredit
     (require 'paredit)
     (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
     (add-hook 'lisp-mode-hook 'paredit-mode)
     (dolist (hook '(emacs-lisp-mode-hook
		     lisp-mode-hook))
       (add-hook hook 'highlight-parentheses-mode))
     
     (require 'find-func)
     (find-function-setup-keys)
     
     ;; elisp-mode-hook
     (defun my-emacs-lisp-mode-hook ()
       (define-key emacs-lisp-mode-map [(tab)] 'my-elisp-indent-or-complete)       
       (local-set-key (kbd "RET") 'newline-and-indent)
       (company-mode)
       (make-local-variable 'company-backends)
       (setq company-backends '(company-elisp company-files)))
     
     (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
     
     (add-hook 'lisp-mode-hook
	       '(lambda ()
		  (local-set-key (kbd "RET") 'newline-and-indent)))
     ))

(setq backward-delete-char-untabify-method 'all)
(defun my-hungry-delete ()
  (interactive)
  (backward-delete-char-untabify 1))

;;; GDB/GUD
(eval-after-load "gdb-mi"
 '(progn
    (setq gdb-many-windows t
	  gdb-use-separate-io-buffer t
	  gud-gdb-command-name "gdb -silent --annotate=3"
	  gud-gud-gdb-command-name "gdb -silent --fullname")))

(eval-when-compile
  (require 'cl))

(defun gud-run-p ()
  "Check whether there is a gud process"
  (interactive)
  (let ((processes-names (mapcar #'process-name (process-list))))
    (find "gud-" processes-names :test '(lambda (seq1 seq2) (search seq1 seq2)))))


(require 'wl-compile)
;; don't ask the compile command
(setq compilation-read-command nil)

(require 'program-utils)

;; haskell settings
(load "~/work/github/emacs/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


(eval-after-load "scheme"
  '(progn
     (add-hook 'scheme-mode-hook
	       (lambda ()
		 (define-key scheme-mode-map "\C-m" 'reindent-then-newline-and-indent)
		 (setq linum-mode t)))))


(eval-after-load "sh-script"
    '(progn
       (add-hook 'sh-mode-hook
		 '(lambda ()
		    (local-set-key (kbd "RET") 'newline-and-indent)))))


;; Sawfish setting, only turn on when it's not macos
(unless (eq system-type 'darwin)
  (autoload 'sawfish-mode "sawfish" "sawfish-mode" t)
  (setq auto-mode-alist (cons '("\\.sawfishrc$"  . sawfish-mode) auto-mode-alist)
	auto-mode-alist (cons '("\\.jl$"         . sawfish-mode) auto-mode-alist)
	auto-mode-alist (cons '("\\.sawfish/rc$" . sawfish-mode) auto-mode-alist))

  (eval-after-load "sawfish-mode"
    '(progn
       (add-hook 'sawfish-mode-hook
		 '(lambda ()
		    (make-local-variable 'company-backends)
		    (setq company-backends '(company-files))))))
  (require 'sawfish-util)
  (setq sawfish-lisp-dir '("/home/velen/.sawfish/"
			   "/usr/local/share/sawfish/1.8.0/lisp"
			   "/usr/local/share/rep/0.91.1/lisp")))

