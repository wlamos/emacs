;;; ok-tex.el --- One-key for tex

;; Copyright (C) 2009  Zhang Wenliang

;; Author: Zhang Wenliang <wlamos@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defvar one-key-menu-LaTeX-alist nil
  "The `one-key' menu alist for LaTeX.")

(setq one-key-menu-LaTeX-alist
      '(
	(("C-p" . "Preview Commands") . one-key-menu-LaTeX-preview)
	(("C-q" . "Fill Commands") . one-key-menu-LaTeX-fill)
	(("C-t" . "Mode Command") . one-key-menu-LaTeX-mode)
	(("C-c" . "Other Commands") . one-key-menu-LaTeX-other)
	(("C-d" . "CDlatex Commands") . one-key-menu-cdlatex)
	(("C-r" . "reftex Commands") . one-key-menu-reftex)
	(("C-j" . "Reindent Then Newline And Indent (C-j)")
. reindent-then-newline-and-indent)
	(("<return>" . "Tex Newline (RET)") . TeX-newline)
	(("\"" . "Tex Insert Quote (\")") . TeX-insert-quote)
	(("$" . "Tex Insert Dollar ($)") . TeX-insert-dollar)
	(("-" . "Latex Babel Insert Hyphen (-)") . LaTeX-babel-insert-hyphen)
	(("\\" . "Tex Insert Backslash (\\)") . TeX-insert-backslash)
	(("C-M-a" . "Latex Find Matching Begin (C-M-a)") . LaTeX-find-matching-begin)
	(("C-M-e" . "Latex Find Matching End (C-M-e)") . LaTeX-find-matching-end)
	(("C-M-i" . "Tex Complete Symbol (C-M-i)") . TeX-complete-symbol)
	(("C-M-j" . "Xdvi Jump To Line (C-M-j)") . xdvi-jump-to-line)
	(("M-RET" . "Latex Insert Item (M-RET)") . LaTeX-insert-item)
	(("C-b" . "Back to previous menu") . one-key-menu-toplevel)
	))

(defun one-key-menu-LaTeX ()
  "The `one-key' menu for LaTeX"
  (interactive)
  (one-key-menu "LaTeX" one-key-menu-LaTeX-alist))



(defvar one-key-menu-LaTeX-other-alist nil
  "The `one-key' menu alist for LaTeX.")

(setq one-key-menu-LaTeX-other-alist
      '(
	(("C-S-b" . "Tex Command Buffer (C-c C-b)") . TeX-command-buffer)
	(("C-c" . "Tex Command Master (C-c C-c)") . TeX-command-master)
	(("C-d" . "Tex Save Document (C-c C-d)") . TeX-save-document)
	(("C-e" . "Latex Environment (C-c C-e)") . LaTeX-environment)
	(("C-f" . "Tex Font (C-c C-f)") . TeX-font)
	(("<tab>" . "Tex Goto Info Page (C-c TAB)") . TeX-goto-info-page)
	(("C-j" . "Latex Insert Item (C-c C-j)") . LaTeX-insert-item)
	(("C-k" . "Tex Kill Job (C-c C-k)") . TeX-kill-job)
	(("C-l" . "Tex Recenter Output Buffer (C-c C-l)") . TeX-recenter-output-buffer)
	(("<return>" . "Tex Insert Macro (C-c RET)") . TeX-insert-macro)
	(("C-n" . "Tex Normal Mode (C-c C-n)") . TeX-normal-mode)
	(("C-r" . "Tex Command Region (C-c C-r)") . TeX-command-region)
	(("C-s" . "Latex Section (C-c C-s)") . LaTeX-section)
	(("C-v" . "Tex View (C-c C-v)") . TeX-view)
	(("C-w" . "Tex Toggle Debug Bad Boxes (C-c C-w)") . TeX-toggle-debug-bad-boxes)
	(("\"" . "Tex Uncomment (C-c \")") . TeX-uncomment)
	(("'" . "Tex Comment Or Uncomment Paragraph (C-c ')")
. TeX-comment-or-uncomment-paragraph)
	(("*" . "Latex Mark Section (C-c *)") . LaTeX-mark-section)
	(("." . "Latex Mark Environment (C-c .)") . LaTeX-mark-environment)
	(("d" . "Tex Doc (C-c ?)") . TeX-doc)
	(("]" . "Latex Close Environment (C-c ])") . LaTeX-close-environment)
	(("^" . "Tex Home Buffer (C-c ^)") . TeX-home-buffer)
	(("_" . "Tex Master File Ask (C-c _)") . TeX-master-file-ask)
	(("`" . "Tex Next Error (C-c `)") . TeX-next-error)
	(("{" . "Tex Insert Braces (C-c {)") . TeX-insert-braces)
	(("}" . "Up List (C-c })") . up-list)
	(("~" . "Latex Math Mode (C-c ~)") . LaTeX-math-mode)
	(("C-b" . "Back to previous menu") . one-key-menu-LaTeX)	
	))

(defun one-key-menu-LaTeX-other ()
  "The `one-key' menu for LaTeX other commands"
  (interactive)
  (one-key-menu "LaTeX" one-key-menu-LaTeX-other-alist))


(defvar one-key-menu-LaTeX-preview-alist nil
  "The `one-key' menu alist for LaTeX.")

(setq one-key-menu-LaTeX-preview-alist
      '(
	(("C-c" . "Clearout Commands (C-c C-p C-c)")
. one-key-menu-LaTeX-preview-clearout)
	(("C-S-b" . "Preview Buffer (C-c C-p C-b)") . preview-buffer)
	(("C-d" . "Preview Document (C-c C-p C-d)") . preview-document)
	(("C-e" . "Preview Environment (C-c C-p C-e)") . preview-environment)
	(("C-f" . "Preview Cache Preamble (C-c C-p C-f)") . preview-cache-preamble)
	(("<tab>" . "Preview Goto Info Page (C-c C-p TAB)") . preview-goto-info-page)
	(("C-p" . "Preview At Point (C-c C-p C-p)") . preview-at-point)
	(("C-r" . "Preview Region (C-c C-p C-r)") . preview-region)
	(("C-s" . "Preview Section (C-c C-p C-s)") . preview-section)
	(("C-w" . "Preview Copy Region As Mml (C-c C-p C-w)")
. preview-copy-region-as-mml)
	(("C-b" . "Back to previous menu") . one-key-menu-LaTeX)
	))

(defun one-key-menu-LaTeX-preview ()
  "The `one-key' menu for LaTeX preview commands"
  (interactive)
  (one-key-menu "LaTeX" one-key-menu-LaTeX-preview-alist))


(defvar one-key-menu-LaTeX-preview-clearout-alist nil
  "The `one-key' menu alist for LaTeX.")

(setq one-key-menu-LaTeX-preview-clearout-alist
      '(
	(("C-S-b" . "Clearout Buffer (C-c C-p C-c C-b)") . preview-clearout-buffer)
	(("C-d" . "Clearout Document (C-c C-p C-c C-d)") . preview-clearout-document)
	(("C-f" . "Cache Preamble Off (C-c C-p C-c C-f)") . preview-cache-preamble-off)
	(("C-p" . "Clearout At Point (C-c C-p C-c C-p)") . preview-clearout-at-point)
	(("C-r" . "Clearout (C-c C-p C-c C-r)") . preview-clearout)
	(("C-s" . "Clearout Section (C-c C-p C-c C-s)") . preview-clearout-section)
	(("C-b" . "Back to previous menu") . one-key-menu-LaTeX-preview)
	))

(defun one-key-menu-LaTeX-preview-clearout ()
  "The `one-key' menu for LaTeX preview clearout commands"
  (interactive)
  (one-key-menu "LaTeX" one-key-menu-LaTeX-preview-clearout-alist))


(defvar one-key-menu-LaTeX-fill-alist nil
  "The `one-key' menu alist for LaTeX.")

(setq one-key-menu-LaTeX-fill-alist
      '(
	(("C-e" . "Fill Environment (C-c C-q C-e)") . LaTeX-fill-environment)
	(("C-p" . "Fill Paragraph (C-c C-q C-p)") . LaTeX-fill-paragraph)
	(("C-r" . "Fill Region (C-c C-q C-r)") . LaTeX-fill-region)
	(("C-s" . "Fill Section (C-c C-q C-s)") . LaTeX-fill-section)
	(("C-b" . "Back to previous menu") . one-key-menu-LaTeX)
	))

(defun one-key-menu-LaTeX-fill ()
  "The `one-key' menu for LaTeX fill commands"
  (interactive)
  (one-key-menu "LaTeX" one-key-menu-LaTeX-fill-alist))


(defvar one-key-menu-LaTeX-mode-alist nil
  "The `one-key' menu alist for LaTeX mode commands.")

(setq one-key-menu-LaTeX-mode-alist
      '(
	(("C-f" . "Fold Mode (C-c C-o C-f)") . TeX-fold-mode)
	(("C-S-b" . "Toggle Debug Bad Boxes (C-c C-t C-b)") . TeX-toggle-debug-bad-boxes)
	(("<tab>" . "Interactive Mode (C-c C-t TAB)") . TeX-interactive-mode)
	(("C-o" . "Omega Mode (C-c C-t C-o)") . TeX-Omega-mode)
	(("C-p" . "Pdf Mode (C-c C-t C-p)") . TeX-PDF-mode)
	(("C-r" . "Pin Region (C-c C-t C-r)") . TeX-pin-region)
	(("C-s" . "Source Specials Mode (C-c C-t C-s)") . TeX-source-specials-mode)
	(("C-w" . "Toggle Debug Warnings (C-c C-t C-w)") . TeX-toggle-debug-warnings)
	(("C-b" . "Back to previous menu") . one-key-menu-LaTeX)
	))

(defun one-key-menu-LaTeX-mode ()
  "The `one-key' menu for LaTeX mode commands"
  (interactive)
  (one-key-menu "LaTeX" one-key-menu-LaTeX-mode-alist))


(defvar one-key-menu-cdlatex-alist nil
  "The `one-key' menu alist for cdlatex.")

(setq one-key-menu-cdlatex-alist
      '(
	(("<tab>" . "Cdlatex Tab (TAB)") . cdlatex-tab)
	(("'" . "Cdlatex Math Modify (')") . cdlatex-math-modify)
	(("`" . "Cdlatex Math Symbol (`)") . cdlatex-math-symbol)
	(("<C-return>" . "Cdlatex Item (<C-return>)") . cdlatex-item)
	(("h" . "Cdlatex Command Help (C-c ?)") . cdlatex-command-help)
	(("{" . "Cdlatex Environment (C-c {)") . cdlatex-environment)
	(("C-b" . "Back to previous menu") . one-key-menu-LaTeX)
	))

(defun one-key-menu-cdlatex ()
  "The `one-key' menu for cdlatex"
  (interactive)
  (one-key-menu "cdlatex" one-key-menu-cdlatex-alist))


(defvar one-key-menu-reftex-alist nil
  "The `one-key' menu alist for reftex.")

(setq one-key-menu-reftex-alist
      '(
	(("&" . "Reftex View Crossref (C-c &)") . reftex-view-crossref)
	(("(" . "Reftex Label (C-c ()") . reftex-label)
	((")" . "Reftex Reference (C-c ))") . reftex-reference)
	(("-" . "Reftex Toc Recenter (C-c -)") . reftex-toc-recenter)
	(("/" . "Reftex Index Selection Or Word (C-c /)")
. reftex-index-selection-or-word)
	(("<" . "Reftex Index (C-c <)") . reftex-index)
	(("=" . "Reftex Toc (C-c =)") . reftex-toc)
	((">" . "Reftex Display Index (C-c >)") . reftex-display-index)
	(("[" . "Reftex Citation (C-c [)") . reftex-citation)
	(("\\" . "Reftex Index Phrase Selection Or Word (C-c \\)")
. reftex-index-phrase-selection-or-word)
	(("|" . "Reftex Index Visit Phrases Buffer (C-c |)")
. reftex-index-visit-phrases-buffer)
	(("6" . "Browse paper at point (S-f6)") . browse-paper-at-point)
	(("C-b" . "Back to previous menu") . one-key-menu-LaTeX)
	))

(defun one-key-menu-reftex ()
  "The `one-key' menu for reftex"
  (interactive)
  (one-key-menu "reftex" one-key-menu-reftex-alist))

(provide 'ok-tex)
;;; ok-tex.el ends here
