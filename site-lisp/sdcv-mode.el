;;; sdcv-mode.el --- major mode to do dictionary query through sdcv

;; Copyright 2006 pluskid
;;
;; Author: pluskid@gmail.com
;; Version: $Id: sdcv-mode.el,v 0.0 2006/12/21 11:20:51 kid Exp $
;; Keywords: sdcv dictionary
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This is a major mode to view output of dictionary search of sdcv.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;   (require 'sdcv-mode)
;;   (global-set-key (kbd "C-c d") 'sdcv-search)

;;; Code:

(require 'outline)
(provide 'sdcv-mode)
(eval-when-compile
  (require 'cl))

;;; ==================================================================
;;; Frontend, search word and display sdcv buffer
(defun sdcv-search (force-all-dictionaries)
  "Prompt for a word to search through sdcv.
When provided with a prefix argument, use all the dictionaries
no matter what `sdcv-dictionary-list' is."
  (interactive "P")
  (let ((word (if (and transient-mark-mode mark-active)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end))
                (sdcv-current-word)))
        ;; note that elisp is dynamic-scoped
        (sdcv-dictionary-list (if force-all-dictionaries
                                  nil
                                sdcv-dictionary-list)))
    (setq word (read-string
                (format "Search the dictionary for (default %s): "
                        word)
                nil nil word))
    (sdcv-search-word word)))

(defun zwl-sdcv-search (force-all-dictionaries)
  "Prompt for a word to search through sdcv.
When provided with a prefix argument, use all the dictionaries
no matter what `sdcv-dictionary-list' is."
  (interactive "P")
  (let ( (old-buffer (current-buffer))
	(word (if (and transient-mark-mode mark-active)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end))
                (sdcv-current-word)))
	  ;; note that elisp is dynamic-scoped
	  (sdcv-dictionary-list (if force-all-dictionaries
				    nil
				  sdcv-dictionary-list)))
    (save-window-excursion
      (sdcv-search-word word))
    ;; (other-window 1)
    ;; (switch-to-buffer old-buffer)
    ;; (message "current buffer name %s" (buffer-name)
    ))

(defun sdcv-search-word (word)
  "Search WORD through the command-line tool sdcv.
The result will be displayed in buffer named with
`sdcv-buffer-name' with `sdcv-mode'."
  (with-current-buffer (get-buffer-create sdcv-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((process (start-process
                    "sdcv"
                    sdcv-buffer-name
                    "sdcv"
                    (sdcv-generate-dictionary-argument)
                    "-n"
                    (shell-quote-argument word))))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (unless (eq (current-buffer) (sdcv-get-buffer))
             (sdcv-goto-sdcv))
           (sdcv-mode-reinit)))))))

(defun sdcv-generate-dictionary-argument ()
  "Generate dictionary argument for sdcv from `sdcv-dictionary-list'."
  (if (null sdcv-dictionary-list)
      ""
    (mapconcat (lambda (dict)
                 (concat "-u "
                         (shell-quote-argument dict)))
               sdcv-dictionary-list
               " ")))

;;; ==================================================================
;;; utilities to switch from and to sdcv buffer
(defun sdcv-current-word ()
  "Get the current word under the cursor."
  (if (or (< emacs-major-version 21)
          (and (= emacs-major-version 21)
               (< emacs-minor-version 4)))
      (sdcv-current-word-1)
    ;; We have a powerful `current-word' function since 21.4
    (current-word nil t)))
(defun sdcv-current-word-1 ()
  (save-excursion
    (backward-word 1)
    (mark-word 1)
    (buffer-substring-no-properties (region-beginning)
                                    (region-end))))
(defvar sdcv-previous-window-conf nil
  "Window configuration before switching to sdcv buffer.")
(defun sdcv-goto-sdcv ()
  "Switch to sdcv buffer in other window."
  (interactive)
  (setq sdcv-previous-window-conf (current-window-configuration))
  (let* ((buffer (sdcv-get-buffer))
         (window (get-buffer-window buffer)))
    (if (null window)
        (switch-to-buffer-other-window buffer)
      (select-window window))))
(defun sdcv-return-from-sdcv ()
  "Bury sdcv buffer and restore the previous window configuration."
  (interactive)
  (if (window-configuration-p sdcv-previous-window-conf)
      (progn
        (set-window-configuration sdcv-previous-window-conf)
        (setq sdcv-previous-window-conf nil)
        (bury-buffer (sdcv-get-buffer)))
    (bury-buffer)))

(defun sdcv-get-buffer ()
  "Get the sdcv buffer. Create one if there's none."
  (let ((buffer (get-buffer-create sdcv-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'sdcv-mode)
        (sdcv-mode)))
    buffer))

;;; ==================================================================
;;; The very major mode
(defvar sdcv-mode-font-lock-keywords
  '(
    ;; dictionary name
    ("^-->\\(.*\\)$" . (1 font-lock-type-face))
    ;; property of word
    ("^<<\\([^>]*\\)>>$" . (1 font-lock-comment-face))
    ;; phonetic symbol
    ("^\\[\\([^]]*\\)\\]$" . (1 font-lock-string-face))
    )
  "Expressions to hilight in `sdcv-mode'")

(defvar sdcv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'sdcv-return-from-sdcv)
    (define-key map "s" 'isearch-forward-regexp)
    (define-key map "r" 'isearch-backward-regexp)
    (define-key map (kbd "C-s") 'isearch-forward)
    (define-key map (kbd "C-r") 'isearch-backward)
    (define-key map (kbd "RET") 'sdcv-mode-scroll-up-one-line)
    (define-key map (kbd "M-RET") 'sdcv-mode-scroll-down-one-line)
    (define-key map "v" 'scroll-up)
    (define-key map (kbd "M-v") 'scroll-down)
    (define-key map " " 'scroll-up)
    (define-key map (kbd "DEL") 'scroll-down)
    (define-key map (kbd "C-n") 'sdcv-mode-next-line)
    (define-key map "n" 'sdcv-mode-next-line)
    (define-key map "j" 'sdcv-mode-next-line)
    (define-key map (kbd "C-p") 'sdcv-mode-previous-line)
    (define-key map "p" 'sdcv-mode-previous-line)
    (define-key map "k" 'sdcv-mode-previous-line)    
    (define-key map "d" 'sdcv-search)
    (define-key map "?" 'describe-mode)
    (define-key map "a" 'show-all)
    (define-key map "h" 'hide-body)
    (define-key map "e" 'show-entry)
    (define-key map "c" 'hide-entry)
    (define-key map "o" 'sdcv-mode-other-window)
    map)
  "Keymap for `sdcv-mode'.")

(define-derived-mode sdcv-mode nil "sdcv"
  "Major mode to look up word through sdcv.
\\{sdcv-mode-map}
Turning on Text mode runs the normal hook `sdcv-mode-hook'."
  (setq font-lock-defaults '(sdcv-mode-font-lock-keywords))
  (setq buffer-read-only t)
  (set (make-local-variable 'outline-regexp) "^-->.*\n-->"))

(defun sdcv-mode-reinit ()
  "Re-initialize buffer.
Hide all entrys but the first one and goto
the beginning of the buffer."
  (ignore-errors
    (setq buffer-read-only t)
    (hide-body)
    (goto-char (point-min))
    (next-line 1)
    (show-entry)))

(defun sdcv-mode-scroll-up-one-line ()
  (interactive)
  (scroll-up 1))
(defun sdcv-mode-scroll-down-one-line ()
  (interactive)
  (scroll-down 1))
(defun sdcv-mode-next-line ()
  (interactive)
  (ignore-errors
    (next-line 1)
    (save-excursion
      (beginning-of-line nil)
      (when (looking-at outline-regexp)
        (show-entry)))))
;; added by wlamos
(defun sdcv-mode-other-window ()
  (interactive)
  (other-window 1))

;; I decide not to fold the definition entry when
;; doing previous-line. So `sdcv-mode-previous-line'
;; is only an alias of `previous-line'.
(defalias 'sdcv-mode-previous-line 'previous-line)

;;;;##################################################################
;;;;  User Options, Variables
;;;;##################################################################


(defvar sdcv-buffer-name "*sdcv*"
  "The name of the buffer of sdcv.")
(defvar sdcv-dictionary-list nil
  "A list of dictionaries to use.
Each entry is a string denoting the name of a dictionary, which
is then passed to sdcv through the '-u' command line option. If
this list is nil then all the dictionaries will be used.")

;;; sdcv-mode.el ends here
