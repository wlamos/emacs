
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
		("\\.json$" . text-mode)
		("Make.apache" . makefile-mode)
		("Make.exe.apache" . makefile-mode)
		) auto-mode-alist))


(autoload 'company-mode "company" nil t)
(setq company-backends nil)



(autoload 'flex-mode "flex-mode")
(autoload 'bison-mode "bison-mode.el")


(require 'cedet)
(require 'semantic-gcc)
;;(global-ede-mode 1)
(semantic-load-enable-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)
(setq semantic-load-turn-everything-on t)
(setq semantic-idle-scheduler-idle-time 432000)
(require 'semantic-c-add-preprocessor-symbol nil 'noerror)
(setq semanticdb-default-save-directory (expand-file-name ".emacs.d/backup/semantic.cache/semanticdb"))
;;; C/C++ dev setting
(eval-after-load "cc-mode"
  '(progn
     (defun my-indent-or-complete ()
       (interactive)
       (if (looking-at "\\>")
	   (hippie-expand nil)
	 (indent-for-tab-command)))
     
     (global-set-key [(control tab)] 'my-indent-or-complete)
     ;;(autoload 'senator-try-expand-semantic "senator")
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
       (setq company-backends '((company-keywords)))
       (require 'xcscope))

     ;; (add-hook 'c++-mode-hook 'yas/minor-mode)
     (add-hook 'c++-mode-hook 'my-c++-mode-hook)

     ;;auto load the index of /usr/include created by semantic
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
;;use https://github.com/gabrielelanaro/emacs-for-python
(when (intern-soft "my-epy-dir")
  (load (concat my-epy-dir "epy-init.el")))



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
(setq inferior-lisp-program "sbcl")
(setenv "PATH" (concat "~/bin" ":" (getenv "PATH")))
;;(setenv "SBCL_HOME" (expand-file-name "~/lib/sbcl"))
(slime-setup '(slime-fancy slime-scratch slime-editing-commands slime-asdf slime-repl))
(setq slime-complete-symbol*-fancy t)
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
;; (setq slime-lisp-implementations
;;            '((sbcl ("/usr/bin/sbcl" "--core" "/home/velen/.emacs.d/sbcl.core-with-swank")
;; 		   :init (lambda (port-file _)
;; 			   (format "(swank:start-server %S)\n" port-file)))
;; 	     (cmucl ("cmucl" "-quiet"))))

(defun my-slime-tab ()
  "Use tab to indent and complete symbols."
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

;;(define-key compilation-mode-map "n" 'wl-goto-next-error)
;;(define-key compilation-mode-map "p" 'wl-goto-previous-error)

(defun wl-goto-next-error ()
  (interactive)
  (compilation-next-error 1)
  (compile-goto-error))

(defun wl-goto-previous-error ()
  (interactive)
  (compilation-previous-error 1)
  (compile-goto-error))

(require 'program-utils)

;; haskell settings
(load (concat my-config-dir "site-lisp/haskell-mode/haskell-site-file"))
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

;; php-mode
(require 'php-mode)

;; erlang-mode
(require 'erlang-start)
(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))
(defun my-erlang-mode-hook ()
        ;; when starting an Erlang shell in Emacs, default in the node name
        (setq inferior-erlang-machine-options '("-sname" "emacs"))
	;; customize keys
        (local-set-key [return] 'newline-and-indent))

(setq erlang-root-dir "/usr/lib/erlang/")
(setq erlang-man-root-dir "/usr/lib/erlang/man/")
(add-to-list 'exec-path "/usr/lib/erlang/bin/")

;; Some Erlang customizations
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(defun erlang-export-current-function()
  "export current function."
  (interactive)
  (save-excursion
    (goto-char (car (bounds-of-thing-at-point 'defun)))
    (when (re-search-forward "(\\(.*?\\))") ;search params
      (let ((params (match-string 1))
            param-count
            funname
            fun-declare)
        (backward-sexp)
        (skip-chars-backward " \t")
        (setq funname (thing-at-point 'symbol))
        (if (string-match "^[ \t]*$" params)
            (setq param-count 0)
          (setq param-count (length  (split-string params ","))))
        (setq fun-declare (format "%s/%d" funname param-count))
        (message "export function:%s" fun-declare)
        (goto-char (point-min))
        (if (re-search-forward "[ \t]*-export[ \t]*([ \t]*\\[" (point-max) t)
            (if (looking-at "[ \t]*\\]")
                (insert fun-declare )
              (insert fun-declare ","))
          (goto-char (point-min))
          (if (re-search-forward "[ \t]*-module[ \t]*(" (point-max) t)
              (progn
                (end-of-line)
                (insert "\n-export([" fun-declare "]).\n"))
            (goto-char (point-min))
            (insert "-export([" fun-declare "]).\n")))))))

(defun get-erl-man ()
  (interactive)
  (let* ((man-path "/usr/lib/erlang/man")
         (man-args (format "-M %s %s" man-path (current-word))))
    (man man-args)))


(defun getFuncNameAtPoint()
 (interactive)
 (if (and (featurep 'semantic) semantic--buffer-cache)
     (let ((ol (semantic-find-tag-by-overlay)))
       (message (funcall semantic-which-function ol)))))
