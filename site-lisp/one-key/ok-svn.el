;;; ok-svn.el --- One-key setting for SVN

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

;;; one-key setting for psvn
(defvar one-key-menu-svn-alist nil
  "The `one-key' menu alist for svn commands.")

(setq one-key-menu-svn-alist
      '(
	(("s" . "Show status of current svn dir") . svn-status)
	(("c" . "Checkout file") . svn-checkout)
	(("l" . "list files") . svn-status-ls)
	(("b" . "show existing branches for working copy") . svn-status-ls-branches)
	(("t" . "show existing tags for working copy") . svn-status-ls-tags)
	(("C-b" . "Back to previous menu") . one-key-menu-toplevel)
	))

(defun one-key-menu-svn ()
  "The `one-key' menu for svn"
  (interactive)
  (one-key-menu "svn" one-key-menu-svn-alist))

;; svn-status
(defvar one-key-menu-svn-status-alist nil
  "The `one-key' menu alist for svn-status.")

(setq one-key-menu-svn-status-alist
      '(
	(("*" . "Mark commands (*)") . one-key-menu-svn-status-mark)
	(("P" . "Property commands (P)") . one-key-menu-svn-status-property)
	(("T" . "Trac commands (T)") . one-key-menu-svn-trac)
	(("O" . "Svn status Other commands (O)") . one-key-menu-svn-status-other)
	(("F" . "Find File/Examine Directory (RET)") . svn-status-find-file-or-examine-directory)
	(("C-o" . "Find File Noselect (C-o)") . svn-status-find-file-other-window-noselect)
	(("$" . "Toggle Elide ($)") . svn-status-toggle-elide)
	(("+" . "Make Directory (+)") . svn-status-make-directory)
	(("-" . "Negative Argument (-)") . negative-argument)
	(("." . "Goto Root Or Return (.)") . svn-status-goto-root-or-return)
	(("=" . "Show Svn Diff (=)") . svn-status-show-svn-diff)
	(("?" . "Toggle Hide Unknown (?)") . svn-status-toggle-hide-unknown)
	(("A" . "Add File Recursively (A)") . svn-status-add-file-recursively)
	(("C" . "Cp (C)") . svn-status-cp)
	(("D" . "Rm (D)") . svn-status-rm)
	(("E" . "Ediff With Revision (E)") . svn-status-ediff-with-revision)
	(("H" . "Use History (H)") . svn-status-use-history)
	(("I" . "Parse Info (I)") . svn-status-parse-info)
	(("K" . "Unlock (K)") . svn-status-unlock)
	(("R" . "Mv (R)") . svn-status-mv)
	(("U" . "Update and checkout directory (U)") . svn-status-update-cmd)
	(("V" . "Svnversion (V)") . svn-status-svnversion)
	(("^" . "Examine Parent (^)") . svn-status-examine-parent)
	(("_" . "Toggle Hide Unmodified (_)") . svn-status-toggle-hide-unmodified)
	(("a" . "Add File (a)") . svn-status-add-file)
	(("b" . "Blame (b)") . svn-status-blame)
	(("c" . "Commit (c)") . svn-status-commit)
	(("e" . "Toggle Edit Cmd Flag (e)") . svn-status-toggle-edit-cmd-flag)
	(("f" . "Find Files (f)") . svn-status-find-files)
	(("g" . "Update status (g)") . svn-status-update)
	(("h" . "Pop To Partner Buffer (h)") . svn-status-pop-to-partner-buffer)
	(("i" . "Info (i)") . svn-status-info)
	(("k" . "Lock (k)") . svn-status-lock)
	(("l" . "Show Svn Log (l)") . svn-status-show-svn-log)
	(("n" . "Next Line (n)") . svn-status-next-line)
	(("o" . "Find File Other Window (o)") . svn-status-find-file-other-window)
	(("p" . "Previous Line (p)") . svn-status-previous-line)
	(("Q" . "Bury Buffer (q)") . svn-status-bury-buffer)
	(("r" . "Revert (r)") . svn-status-revert)
	(("s" . "Show Process Buffer (s)") . svn-status-show-process-buffer)
	(("v" . "View File Other Window (v)") . svn-status-view-file-other-window)
	(("w" . "Copy Current Line Info (w)") . svn-status-copy-current-line-info)
	(("x" . "Redraw Status Buffer (x)") . svn-status-redraw-status-buffer)
	(("~" . "Get Specific Revision (~)") . svn-status-get-specific-revision)
	(("M-c" . "Cleanup (M-c)") . svn-status-cleanup)
	(("C-b" . "Back to previous menu") . one-key-menu-svn)
	))

(defun one-key-menu-svn-status ()
  "The `one-key' menu for svn-status"
  (interactive)
  (one-key-menu "svn-status" one-key-menu-svn-status-alist))

(defvar one-key-menu-svn-trac-alist nil
  "The `one-key' menu alist for svn-trac.")

(setq one-key-menu-svn-trac-alist
      '(
	(("c" . "Browse Changeset (T c)") . svn-trac-browse-changeset)
	(("i" . "Browse Ticket (T i)") . svn-trac-browse-ticket)
	(("m" . "Browse Roadmap (T m)") . svn-trac-browse-roadmap)
	(("r" . "Browse Report (T r)") . svn-trac-browse-report)
	(("s" . "Browse Source (T s)") . svn-trac-browse-source)
	(("t" . "Browse Timeline (T t)") . svn-trac-browse-timeline)
	(("w" . "Browse Wiki (T w)") . svn-trac-browse-wiki)
	(("C-b" . "Back to previous menu") . one-key-menu-svn-status)
	))

(defun one-key-menu-svn-trac ()
  "The `one-key' menu for svn-trac"
  (interactive)
  (one-key-menu "svn-trac" one-key-menu-svn-trac-alist))

(defvar one-key-menu-svn-status-property-alist nil
  "The `one-key' menu alist for svn-status-property.")

(setq one-key-menu-svn-status-property-alist
      '(
	(("TAB" . "Property Edit Svn Ignore (P TAB)") . svn-status-property-edit-svn-ignore)
	(("RET" . "Select Line (P RET)") . svn-status-select-line)
	(("I" . "Property Ignore File Extension (P I)") . svn-status-property-ignore-file-extension)
	(("d" . "Property Delete (P d)") . svn-status-property-delete)
	(("e" . "Property Edit One Entry (P e)") . svn-status-property-edit-one-entry)
	(("i" . "Property Ignore File (P i)") . svn-status-property-ignore-file)
	(("k" . "Property Set Keyword List (P k)") . svn-status-property-set-keyword-list)
	(("l" . "Property List (P l)") . svn-status-property-list)
	(("m" . "Property Set Mime Type (P m)") . svn-status-property-set-mime-type)
	(("s" . "Property Set (P s)") . svn-status-property-set)
	(("x" . "Property Set Executable (P x)") . svn-status-property-set-executable)
	(("y" . "Property Set Eol Style (P y)") . svn-status-property-set-eol-style)
	(("D" . "Property Set Keyword Date (P K d)") . svn-status-property-set-keyword-date)
	(("C-i" . "Property Set Keyword Id (P K i)") . svn-status-property-set-keyword-id)
	(("C-b" . "Back to previous menu") . one-key-menu-svn-status)
	))

(defun one-key-menu-svn-status-property ()
  "The `one-key' menu for svn-status-property"
  (interactive)
  (one-key-menu "svn-status-property" one-key-menu-svn-status-property-alist))

(defvar one-key-menu-svn-status-mark-alist nil
  "The `one-key' menu alist for svn-status-mark.")

(setq one-key-menu-svn-status-mark-alist
      '(
	(("m" . "Set User Mark (m)") . svn-status-set-user-mark)
	(("u" . "Unset User Mark (u)") . svn-status-unset-user-mark)
	(("DEL" . "Unset User Mark Backwards (DEL)") . svn-status-unset-user-mark-backwards)
	(("!" . "Unset All Usermarks (* !)") . svn-status-unset-all-usermarks)
	(("%" . "Mark Filename Regexp (* %)") . svn-status-mark-filename-regexp)
	(("*" . "Mark Changed (* *)") . svn-status-mark-changed)
	(("." . "Mark By File Ext (* .)") . svn-status-mark-by-file-ext)
	(("/" . "Mark Unknown (* ?)") . svn-status-mark-unknown)
	(("A" . "Mark Added (* A)") . svn-status-mark-added)
	(("D" . "Mark Deleted (* D)") . svn-status-mark-deleted)
	(("M" . "Mark Modified (* M)") . svn-status-mark-modified)
	(("P" . "Mark Modified Properties (* P)") . svn-status-mark-modified-properties)
	(("u" . "Show Svn Diff For Marked Files (* u)") . svn-status-show-svn-diff-for-marked-files)
	(("C-b" . "Back to previous menu") . one-key-menu-svn-status)
	))

(defun one-key-menu-svn-status-mark ()
  "The `one-key' menu for svn-status-mark"
  (interactive)
  (one-key-menu "svn-status-mark" one-key-menu-svn-status-mark-alist))

(defvar one-key-menu-svn-status-other-alist nil
  "The `one-key' menu alist for svn-status-other.")

(setq one-key-menu-svn-status-other-alist
      '(
	(("d" . "Svn Branch Diff (B d)") . svn-branch-diff)
	(("b" . "Set Branch List (O b)") . svn-status-set-branch-list)
	(("c" . "Set Changelog Style (O c)") . svn-status-set-changelog-style)
	(("f" . "Toggle Display Full Path (O f)") . svn-status-toggle-display-full-path)
	(("l" . "Load State (O l)") . svn-status-load-state)
	(("n" . "Set Module Name (O n)") . svn-status-set-module-name)
	(("s" . "Save State (O s)") . svn-status-save-state)
	(("t" . "Set Trac Project Root (O t)") . svn-status-set-trac-project-root)
	(("v" . "Toggle Svn Verbose Flag (O v)") . svn-status-toggle-svn-verbose-flag)
	(("x" . "Toggle Sort Status Buffer (O x)") . svn-status-toggle-sort-status-buffer)
	(("X" . "Resolve Conflicts (X X)") . svn-status-resolve-conflicts)
	(("e" . "Export (X e)") . svn-status-export)
	(("v" . "Resolved (X v)") . svn-status-resolved)
	(("g" . "Grep Files (S g)") . svn-status-grep-files)
	(("s" . "Search Files (S s)") . svn-status-search-files)
	(("j" . "Dired Jump (C-x C-j)") . svn-status-dired-jump)
	(("C-b" . "Back to previous menu") . one-key-menu-svn-status)
	))

(defun one-key-menu-svn-status-other ()
  "The `one-key' menu for svn-status-other"
  (interactive)
  (one-key-menu "svn-status-other" one-key-menu-svn-status-other-alist))


(defvar one-key-menu-svn-log-view-alist nil
  "The `one-key' menu alist for svn-log-view.")

(setq one-key-menu-svn-log-view-alist
      '(
	(("TAB" . "Svn Log Next Link (TAB)") . svn-log-next-link)
	(("F" . "Svn Log Find File At Point (RET)") . svn-log-find-file-at-point)
	(("#" . "Svn Log Mark Partner Revision (#)") . svn-log-mark-partner-revision)
	(("-" . "Negative Argument (-)") . negative-argument)
	(("=" . "Svn Log View Diff (=)") . svn-log-view-diff)
	(("E" . "Svn Log Ediff Specific Revision (E)") . svn-log-ediff-specific-revision)
	(("e" . "Svn Log Edit Log Entry (e)") . svn-log-edit-log-entry)
	(("f" . "Svn Log Get Specific Revision (f)") . svn-log-get-specific-revision)
	(("n" . "Svn Log View Next (n)") . svn-log-view-next)
	(("p" . "Svn Log View Prev (p)") . svn-log-view-prev)
	(("Q" . "Bury Buffer (q)") . bury-buffer)
	(("x" . "Svn Log Exchange Partner Mark With Point (x)") . svn-log-exchange-partner-mark-with-point)
	(("~" . "Svn Log Get Specific Revision (~)") . svn-log-get-specific-revision)
	(("<backtab>" . "Svn Log Prev Link (S-TAB)") . svn-log-prev-link)
	(("C-b" . "Back to previous menu (C-b)") . one-key-menu-toplevel)
	))

(defun one-key-menu-svn-log-view ()
  "The `one-key' menu for svn-log-view"
  (interactive)
  (one-key-menu "svn-log-view" one-key-menu-svn-log-view-alist))

(defvar one-key-menu-svn-log-edit-alist nil
  "The `one-key' menu alist for svn-log-edit.")

(setq one-key-menu-svn-log-edit-alist
      '(
	(("C-a" . "log-edit-insert-changelog (C-c C-a)") . log-edit-insert-changelog)
	(("C-c" . "log-edit-done (C-c C-c)") . log-edit-done)
	(("C-d" . "svn-log-edit-svn-diff (C-c C-d)") . svn-log-edit-svn-diff)
	(("C-f" . "log-edit-show-files (C-c C-f)") . log-edit-show-files)
	(("TAB" . "svn-log-edit-svn-status (C-c TAB)") . svn-log-edit-svn-status)
	(("C-l" . "svn-log-edit-svn-log (C-c C-l)") . svn-log-edit-svn-log)
	(("C-q" . "svn-log-edit-abort (C-c C-q)") . svn-log-edit-abort)
	(("C-s" . "svn-log-edit-save-message (C-c C-s)") . svn-log-edit-save-message)
	(("C-z" . "svn-log-edit-erase-edit-buffer (C-c C-z)") . svn-log-edit-erase-edit-buffer)
	(("h" . "log-edit-mode-help (C-c ?)") . log-edit-mode-help)
	(("s" . "svn-log-edit-show-files-to-commit (C-c C-?)") . svn-log-edit-show-files-to-commit)
	(("M-TAB" . "ispell-complete-word (M-TAB)") . ispell-complete-word)
	(("M-n"	. "log-edit-next-comment (M-n)") . log-edit-next-comment)
	(("M-p" . "log-edit-previous-comment (M-p)") . log-edit-previous-comment)
	(("M-r" . "log-edit-comment-search-backward (M-r)") . log-edit-comment-search-backward)
	(("M-s" . "log-edit-comment-search-forward (M-s)") . log-edit-comment-search-forward)
	(("C-d" . "log-edit-show-diff (C-c C-d)") . log-edit-show-diff)
	(("C-b" . "Back to previous menu (C-b)") . one-key-menu-toplevel)
	))

(defun one-key-menu-svn-log-edit ()
  "The `one-key' menu for svn-log-edit"
  (interactive)
  (one-key-menu "svn-log-edit" one-key-menu-svn-log-edit-alist))

(provide 'ok-svn)
;;; ok-svn.el ends here
