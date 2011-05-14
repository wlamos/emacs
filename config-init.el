(setq inhibit-default-init t)

(setq load-path (append
		 '("~/work/github/emacs"
		   "~/work/github/emacs/site-lisp"
		   "~/work/github/emacs/site-lisp/color-theme"
		   )
		 load-path))

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
		  


