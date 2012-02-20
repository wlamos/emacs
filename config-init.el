(setq inhibit-default-init t)

(dolist (dir (list my-config-dir
		   (concat my-config-dir "site-lisp/")))
  (add-to-list 'load-path dir))

(let ((default-directory (concat my-config-dir "site-lisp/")))
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory "/usr/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(setq load-path (sort load-path #'string<))
(defmacro require-maybe (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror))

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))

(defmacro load-if-exist (file)
  "Load FILE if it exists"
  `(when (file-exists-p ,file)
     (load-file ,file)))

(defun load-files-if-exist (files)
  "Load files in list FILES if they exist"
  (mapcar #'(lambda (file)
	      (when (file-exists-p file)
		(load-file file)))
	  files))

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


(unless (server-running-p)
  (server-start))

(unless (and (fboundp 'daemonp) (daemonp))
  (unless (file-exists-p "~/.desktopaid/")
    (make-directory "~/.desktopaid/"))
  (autoload 'dta-hook-up "desktopaid.el" "Desktop Aid" t)
  (dta-hook-up))
