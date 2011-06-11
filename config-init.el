(setq inhibit-default-init t)

(dolist (dir (list my-config-dir
		   (concat my-config-dir "site-lisp/")))
  (add-to-list 'load-path dir))

(let ((default-directory (concat my-config-dir "site-lisp/")))
  (normal-top-level-add-subdirs-to-load-path))

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
			  "func.el")))


