(setq inhibit-default-init t)
(setq load-path (append
		 '("~/work/github/emacs"
		   "~/work/github/emacs/site-lisp"
		   "~/work/github/emacs/site-lisp/color-theme"
		   "~/work/github/emacs/site-lisp/nxml-mode-20041004"
		   "~/work/github/emacs/site-lisp/company"
		   "~/share/emacs/site-lisp"
		   )
		 load-path))

(defmacro require-maybe (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror))

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))

(defun load-file-list (file-list)
  "*Load file list FILE-LIST in order."
  (dolist (file file-list)
    (load-file file)))

(load-file-list (mapcar #'(lambda (name)
			    (concat my-config-dir name))
			'("mycustom.el"
			  "elisp.el"
			  "program.el"
			  "func.el"
			  )))
		  


