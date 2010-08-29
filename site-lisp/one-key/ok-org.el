;;; ok-org.el --- One-key for Org

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

(defun one-key-menu-org ()
  "The `one-key' menu for org"
  (interactive)
  (one-key-menu "org" one-key-menu-org-alist))


(defvar one-key-menu-org-columns-alist nil
  "The `one-key' menu alist for org columns.")

(setq one-key-menu-org-columns-alist
      '(
	(("<" . "org-columns-narrow (<)") . org-columns-narrow)
	((">" . "org-columns-widen (>)") . org-columns-widen)
	(("a" . "org-columns-edit-allowed (a)") . org-columns-edit-allowed)
	(("c" . "org-columns-content (c)") . org-columns-content)
	(("e" . "org-columns-edit-value (e)") . org-columns-edit-value)
	(("g" . "org-columns-redo (g)") . org-columns-redo)
	(("n" . "org-columns-next-allowed-value (n)") . org-columns-next-allowed-value)
	(("o" . "org-overview (o)") . org-overview)
	(("p" . "org-columns-previous-allowed-value (p)")
. org-columns-previous-allowed-value)
	(("Q" . "org-columns-quit (q)") . org-columns-quit)
	(("r" . "org-columns-redo (r)") . org-columns-redo)
	(("s" . "org-columns-edit-attributes (s)") . org-columns-edit-attributes)
	(("v" . "org-columns-show-value (v)") . org-columns-show-value)
	(("<M-S-left>"	. "org-columns-delete (<M-S-left>)") . org-columns-delete)
	(("<M-S-right>" . "org-columns-new (<M-S-right>)") . org-columns-new)
	(("<M-left>" . "org-columns-move-left (<M-left>)") . org-columns-move-left)
	(("<M-right>" . "org-columns-move-right (<M-right>)") . org-columns-move-right)
	(("<S-left>" . "org-columns-previous-allowed-value (<S-left>)")
. org-columns-previous-allowed-value)
	(("<S-right>" . "org-columns-next-allowed-value (<S-right>)")
. org-columns-next-allowed-value)
	(("C-c" . "org-columns-set-tags-or-toggle (C-c C-c)")
. org-columns-set-tags-or-toggle)
	(("C-o" . "org-columns-open-link (C-c C-o)") . org-columns-open-link)
	(("C-t" . "org-columns-todo (C-c C-t)") . org-columns-todo)
	(("C-b" . "Back to previous menu") . one-key-menu-org)
	))

(defun one-key-menu-org-columns ()
  "The `one-key' menu for org columns"
  (interactive)
  (one-key-menu "columns" one-key-menu-org-columns-alist))


(defvar one-key-menu-org-latex-alist nil
  "The `one-key' menu alist for org latex commands.")

(setq one-key-menu-org-latex-alist
      '(
	(("p" . "Preview Latex Fragment (C-c C-x C-l)") . org-preview-latex-fragment)
	(("L" . "toggle cdlatex mode") . org-cdlatex-mode)
	(("`" . "insert math symbol (`)") . cdlatex-math-symbol)
	(("'" . "modify math symbol") . org-cdlatex-math-modify)
	(("h" . "cdlatex command help") . cdlatex-command-help)
	(("C-b" . "Back to previous menu") . one-key-menu-org)
	))

(defun one-key-menu-org-latex ()
  "The `one-key' menu for org latex commands"
  (interactive)
  (one-key-menu "org" one-key-menu-org-latex-alist))

(defvar one-key-menu-org-misc-alist nil
  "The `one-key' menu alist for org miscellaneous commands.")

(setq one-key-menu-org-misc-alist
      '(
	(("r" . "File Remember Item (C-c r)") . org-remember)
	(("C-e" . "Export") . org-export)
	(("%" . "Mark Ring Push") . org-mark-ring-push)
	(("'" . "Edit Special") . org-edit-special)
	((":" . "Toggle Fixed Width Section") . org-toggle-fixed-width-section)
	(("f" . "Footnote Action") . org-footnote-action)
	(("C-b" . "Back to previous menu") . one-key-menu-org)
	))

(defun one-key-menu-org-misc ()
  "The `one-key' menu for org miscellaneous commands."
  (interactive)
  (one-key-menu "org" one-key-menu-org-misc-alist))


(defvar one-key-menu-org-table-alist nil
  "The `one-key' menu alist for org tables.")

(setq one-key-menu-org-table-alist
      '(
	(("c" . "Realign table (C-c C-c)") . org-ctrl-c-ctrl-c)
	(("f" . "Formula Commands") . one-key-menu-org-formula)
	(("<M-up>" . "Shift Row Up (M-up)") . org-metaup)
	(("<M-down>" . "Shift Row Down (M-down)") . org-metadown)
	(("<M-S-up>" . "Delete Row (M-S-up)") . org-shiftmetaup)
	(("<M-S-down>" . "Insert Row (M-S-down)") . org-shiftmetadown)
	(("<M-left>" . "Shift Column Left (M-left)") . org-metaleft)
	(("<M-right>" . "Shift Column Right (M-right)") . org-metaright)
	(("<M-S-left>" . "Delete Column (M-S-left)") . org-shiftmetaleft)
	(("<M-S-right>" . "Insert Column (M-S-right)") . org-shiftmetaright)
	(("-" . "Insert seperator line (C-c -)") . org-ctrl-c-minus)
	(("<return>" . "Insert hline and new row (C-c Ret)") . org-ctrl-c-ret)
	(("`" . "Edit Field (C-c `)") . org-table-edit-field)
	(("SPC" . "Blank Field (C-c SPC)") . org-table-blank-field)
	(("^" . "Sort rows (C-c ^)") . org-sort)
	(("|" . "Create Or Convert From Region (C-c |)")
. org-table-create-or-convert-from-region)
	(("#" . "Create With Table.El (C-c ~)") . org-table-create-with-table.el)
	(("E" . "Export table") . org-table-export)
	(("I" . "Import table") . org-table-import)
	(("C-b" . "Back to previous menu") . one-key-menu-org)
	))

(defun one-key-menu-org-table ()
  "The `one-key' menu for org tables"
  (interactive)
  (one-key-menu "tables" one-key-menu-org-table-alist))

(defvar one-key-menu-org-formula-alist nil
  "The `one-key' menu alist for org table formulas.")

(defun my-func-org-table-eval-field-formula nil
  "Evaluate a field formula."
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'org-table-eval-formula))

(defun my-func-org-table-eval-update-all-formulas nil
  "Update all formulas in table."
  (interactive)
  (setq current-prefix-arg 'all)
  (call-interactively 'org-ctrl-c-star))

(defun my-func-org-table-eval-update-all-formulas-iteratively nil
  "Update all formulas in table iteractively until no further changes."
  (interactive)
  (setq current-prefix-arg 'iterate)
  (call-interactively 'org-ctrl-c-star))


(setq one-key-menu-org-formula-alist
      '(
	(("=" . "Eval Column Formula (C-c =)") . org-table-eval-formula)
	(("f" . "Eval Field Formula (C-u C-c =)") . my-func-org-table-eval-field-formula)
	(("+" . "Sum entries in column (C-c +)") . org-table-sum)
	(("'" . "Edit Formulas in special buffer (C-c ')") . org-edit-special)
	(("]" . "Toggle Coordinate Overlays (C-c })")
. org-table-toggle-coordinate-overlays)
	(("[" . "Toggle Formula Debugger (C-c {)") . org-table-toggle-formula-debugger)
	(("i" . "Field Info (C-c ?)") . org-table-field-info)
	(("8" . "Recalculate Formulas On This Line (C-c *)") . org-ctrl-c-star)
	(("*" . "Recalculate All Formulas (C-u C-c *)")
. my-func-org-table-eval-update-all-formulas)
	(("C-*" . "Recalculate All Formulas Iteratively (C-u C-u C-c *)")
. my-func-org-table-eval-update-all-formulas-iteratively)
	(("C-b" . "Back to previous menu") . one-key-menu-org-table)
	))

(defun one-key-menu-org-formula ()
  "The `one-key' menu for org table formulas"
  (interactive)
  (one-key-menu "tables" one-key-menu-org-formula-alist))

(defvar one-key-menu-org-timestamp-alist nil
  "The `one-key' menu alist for org timestamps.")

(setq one-key-menu-org-timestamp-alist
      '(
	(("d" . "Deadline (C-c C-d)") . org-deadline)
	(("s" . "Schedule (C-c C-s)") . org-schedule)
	(("y" . "Evaluate Time Range (C-c C-y)") . org-evaluate-time-range)
	(("1" . "Time Stamp Inactive (C-c !)") . org-time-stamp-inactive)
	(("." . "Time Stamp (C-c .)") . org-time-stamp)
	(("," . "Date From Calendar (C-c <)") . org-date-from-calendar)
	((">" . "Goto Calendar (C-c >)") . org-goto-calendar)
	(("t" . "Toggle Time Stamp Overlays (C-c C-t)") . org-toggle-time-stamp-overlays)
	(("C-b" . "Back to previous menu") . one-key-menu-org)
	))

(defun one-key-menu-org-timestamp ()
  "The `one-key' menu for org timestamps"
  (interactive)
  (one-key-menu "timestamps" one-key-menu-org-timestamp-alist))

(defvar one-key-menu-org-clock-alist nil
  "The `one-key' menu alist for org clock.")

(setq one-key-menu-org-clock-alist
      '(
	(("d" . "Clock Display (C-c C-x C-d)") . org-clock-display)
	(("<tab>" . "Clock In (C-c C-x C-i)\n(with prefix select task to clock into from
list)") . org-clock-in)
	(("o" . "Clock Out (C-c C-x C-o)") . org-clock-out)
	(("j" . "Clock Goto (C-c C-x C-j)") . org-clock-goto)
	(("x" . "Clock Cancel (C-c C-x C-x)") . org-clock-cancel)
	(("r" . "Insert/update dynamic clock report table (C-c C-x C-r)")
. org-clock-report)
	(("," . "Timer Pause Or Continue (C-c C-x ,)\n(with prefix, stop the timer)")
. org-timer-pause-or-continue)
	(("-" . "Insert Timer List Item (C-c C-x -)") . org-timer-item)
	(("." . "Insert Timer (C-c C-x .)") . org-timer)
	(("0" . "Start/Restart Timer Start (C-c C-x 0)") . org-timer-start)
	(("C-b" . "Back to previous menu") . one-key-menu-org)
	))

(defun one-key-menu-org-clock ()
  "The `one-key' menu for org clock"
  (interactive)
  (one-key-menu "clock" one-key-menu-org-clock-alist))


(defvar one-key-menu-org-movement-alist nil
  "The `one-key' menu alist for org movement commands.")

(setq one-key-menu-org-movement-alist
      '(
	(("b" . "Outline Backward Same Level (C-c C-b)") . outline-backward-same-level)
	(("f" . "Outline Forward Same Level (C-c C-f)") . outline-forward-same-level)
	(("j" . "Goto (C-c C-j)") . org-goto)
	(("7" . "Mark Ring Goto (C-c &)") . org-mark-ring-goto)
	(("n" . "Outline Next Visible Heading (C-c C-n)") . outline-next-visible-heading)
	(("p" . "Outline Previous Visible Heading (C-c C-p)")
. outline-previous-visible-heading)
	(("u" . "Outline Up Heading (C-c C-u)") . outline-up-heading)
	(("." . "Goto Calendar (C-c >)") . org-goto-calendar)
	(("o" . "Open At Point (C-c C-o)") . org-open-at-point)
	(("F" . "Footnote Action (C-c C-x f)") . org-footnote-action)
	(("C-b" . "Back to previous menu") . one-key-menu-org)
	))

(defun one-key-menu-org-movement ()
  "The `one-key' menu for org movement commands"
  (interactive)
  (one-key-menu "movement" one-key-menu-org-movement-alist))

(defvar one-key-menu-org-view-alist nil
  "The `one-key' menu alist for org views.")

(setq one-key-menu-org-view-alist
      '(
	(("<tab>" . "Show Children (C-c TAB)") . show-children)
	(("k" . "Kill Note Or Show Branches (C-c C-k)") . org-kill-note-or-show-branches)
	(("o" . "Open Current/Next Link (C-c C-o)") . org-open-at-point)
	(("r" . "Reveal (C-c C-r)") . org-reveal)
	(("v" . "Show Todo Tree (C-c C-v)") . org-show-todo-tree)
	(("b" . "Tree To Indirect Buffer (C-c C-x b)") . org-tree-to-indirect-buffer)
	(("w" . "Widen, i.e. show all of buffer (M-x widen)") . widen)
	(("/" . "Sparse Tree (C-c /)") . org-sparse-tree)
	(("\\" . "Match Sparse Tree (C-c \\)") . org-match-sparse-tree)
	(("s" . "Outlines Show (C-c s)") . outlines-show)
	(("n" . "Next Link (C-c C-x C-n)") . org-next-link)
	(("p" . "Previous Link (C-c C-x C-p)") . org-previous-link)
	(("c" . "Columns (C-c C-x C-c)") . org-columns)
	(("l" . "Preview Latex Fragment (C-c C-x C-l)") . org-preview-latex-fragment)
	(("C-b" . "Back to previous menu") . one-key-menu-org)
	))

(defun one-key-menu-org-view ()
  "The `one-key' menu for org views"
  (interactive)
  (one-key-menu "views" one-key-menu-org-view-alist))

(defvar one-key-menu-org-manipulate-alist nil
  "The `one-key' menu alist for org outline manipulations.")

(setq one-key-menu-org-manipulate-alist
      '(
	(("^" . "Move Subtree Up (M-up)") . outline-move-subtree-up)
	(("-" . "Move Subtree Down (M-down)") . outline-move-subtree-down)
	(("'" . "Mark Subtree (C-c @)") . outline-mark-subtree)
	(("<return>" . "Insert new header (C-c Ret)") . org-ctrl-c-ret)
	(("8" . "Convert line/region To/From Header (C-c *)") . org-ctrl-c-star)
	(("6" . "Sort items (C-c ^)") . org-sort)
	(("," . "Promote Header (M-left)") . outline-promote)
	(("." . "Demote Header (M-right)") . outline-demote)
	(("<left>" . "Promote Subtree (M-S-left)") . outline-promote-subtree)
	(("<right>" . "Demote Subtree (M-S-right)") . outline-demote-subtree)
	(("w" . "Refile subtree (C-c C-w)") . org-refile)
	(("C-w" . "Cut Special (C-c C-x C-w)") . org-cut-special)
	(("C-y" . "Paste Special (C-c C-x C-y)") . org-paste-special)
	(("M-w" . "Copy Special (C-c C-x M-w)") . org-copy-special)
	(("C-b" . "Back to previous menu") . one-key-menu-org)
	))

(defun one-key-menu-org-manipulate ()
  "The `one-key' menu for org manipulations"
  (interactive)
  (one-key-menu "manipulate/edit structure" one-key-menu-org-manipulate-alist))

(defvar one-key-menu-org-archive-alist nil
  "The `one-key' menu alist for org archiving.")

(setq one-key-menu-org-archive-alist
      '(
	(("4" . "Archive Subtree (C-c $)") . org-archive-subtree)
	(("s" . "Advertized Archive Subtree (C-c C-x C-s)")
. org-advertized-archive-subtree)
	(("a" . "Toggle Archive Tag (C-c C-x a)") . org-toggle-archive-tag)
	(("A" . "Archive To Archive Sibling (C-c C-x A)")
. org-archive-to-archive-sibling)
	(("C-b" . "Back to previous menu") . one-key-menu-org)
	))

(defun one-key-menu-org-archive ()
  "The `one-key' menu for org archiving"
  (interactive)
  (one-key-menu "archive commands" one-key-menu-org-archive-alist))

(defvar one-key-menu-org-dblock-alist nil
  "The `one-key' menu alist for org dynamic blocks.")

(setq one-key-menu-org-dblock-alist
      '(
	(("u" . "Dblock Update (C-c C-x C-u)") . org-dblock-update)
	(("i" . "Insert Columns Dblock (C-c C-x i)") . org-insert-columns-dblock)
	(("C-b" . "Back to previous menu") . one-key-menu-org)
	))

(defun one-key-menu-org-dblock ()
  "The `one-key' menu for org dynamic blocks"
  (interactive)
  (one-key-menu "dynamic block commands" one-key-menu-org-dblock-alist))

(defvar one-key-menu-org-agenda-alist nil
  "The `one-key' menu alist for org agenda commands.")

(setq one-key-menu-org-agenda-alist
      '(
	(("k" . "Action Commands (k)") . org-agenda-action)
	(("b" . "Agenda Commands In Org Files") . one-key-menu-org-agenda-org)
	(("i" . "Commands For Item Under Cursor") . one-key-menu-org-agenda-item)
	(("t" . "Clock/Timestamp Commands") . one-key-menu-org-agenda-time)
	(("v" . "View Commands") . one-key-menu-org-agenda-view)
	(("c" . "Calendar Commands") . one-key-menu-org-agenda-calendar)
	(("C-w" . "Write To File (C-x C-w)") . org-write-agenda)
	(("C-q" . "Quit and Remove Agenda Buffer (q)") . org-agenda-quit)
	(("x" . "Exit Agenda and Remove Associated Buffers (x)") . org-agenda-exit)
	(("C-b" . "Back to previous menu") . one-key-menu-org)
	))

(defun one-key-menu-org-agenda ()
  "The `one-key' menu for org agenda commands"
  (interactive)
  (one-key-menu "agenda commands" one-key-menu-org-agenda-alist))

(defvar one-key-menu-org-agenda-org-alist nil
  "The `one-key' menu alist for org agenda commands in org files.")

(setq one-key-menu-org-agenda-org-alist
      '(
	(("k" . "Mark Entry For Agenda Action (C-c C-x C-k)")
. org-mark-entry-for-agenda-action)
	(("," . "Agenda Set Restriction Lock (C-c C-x <)")
. org-agenda-set-restriction-lock)
	(("." . "Agenda Remove Restriction Lock (C-c C-x >)")
. org-agenda-remove-restriction-lock)
	(("[" . "Agenda File To Front (C-c [)") . org-agenda-file-to-front)
	(("]" . "Remove Agenda File (C-c ])") . org-remove-file)
	(("C-b" . "Back to previous menu") . one-key-menu-org-agenda)
	))

(defun one-key-menu-org-agenda-org ()
  "The `one-key' menu for org agenda commands in org files"
  (interactive)
  (one-key-menu "agenda commands" one-key-menu-org-agenda-org-alist))

(defvar one-key-menu-org-agenda-item-alist nil
  "The `one-key' menu alist for org agenda commands on items.")

(setq one-key-menu-org-agenda-item-alist
      '(
	(("SPC" . "Show Current Item In Other Window (SPC)") . org-agenda-show)
	(("<return>" . "Goto Current Item (RET)") . org-agenda-switch-to)
	(("f" . "Toggle Follow Mode (f)") . org-agenda-follow-mode)
	(("b" . "Show Item Subtree In Indirect Buffer (b)")
. org-agenda-tree-to-indirect-buffer)
	(("t" . "Change TODO State (t)") . org-agenda-todo)
	(("C-_" . "Undo Change (C-_)") . org-agenda-undo)
	(("a" . "Toggle Archive Tag (a)") . org-agenda-toggle-archive-tag)
	(("A" . "Move Subtree to Archive Sibling (A)")
. org-agenda-archive-to-archive-sibling)
	(("4" . "Archive Subtree ($)") . org-agenda-archive)
	(("T" . "Show Tags (T)") . org-agenda-show-tags)
	((";" . "Set Tags (:)") . org-agenda-set-tags)
	(("," . "Set Priority (,)") . org-agenda-priority)
	(("P" . "Show Weighted Priority (P)") . org-agenda-show-priority)
	(("<S-up>" . "Increase Priority (S-up)") . org-agenda-priority-up)
	(("<S-down>" . "Decrease Priority (S-down)") . org-agenda-priority-down)
	(("z" . "Add Note (z)") . org-agenda-add-note)
	(("C-s" . "Schedule Item (C-c C-s)") . org-agenda-schedule)
	(("C-d" . "Set Deadline (C-c C-d)") . org-agenda-deadline)
	(("C-b" . "Back to previous menu") . one-key-menu-org-agenda)
	))

(defun one-key-menu-org-agenda-item ()
  "The `one-key' menu for org agenda commands on items"
  (interactive)
  (one-key-menu "agenda commands" one-key-menu-org-agenda-item-alist))


(defvar one-key-menu-org-agenda-time-alist nil
  "The `one-key' menu alist for org agenda commands related to timestamps/clock")

(setq one-key-menu-org-agenda-time-alist
      '(
	(("<S-left>" . "Change Timestamp 1 day Back (S-left)") . org-agenda-date-later)
	(("<S-right>" . "Change Timestamp 1 day Forward (S-right)")
. org-agenda-date-earlier)
	((">" . "Prompt For Timestamp (>)") . org-agenda-date-prompt)
	(("O" . "Stop Clock (O)") . org-agenda-clock-out)
	(("X" . "Cancel Clock (X)") . org-agenda-clock-cancel)
	(("J" . "Jump To Clock In Other Window (J)") . org-clock-goto)
	(("R" . "Toggle Clockreport Mode (R)") . org-agenda-clockreport-mode)
	(("C-b" . "Back to previous menu") . one-key-menu-org-agenda)
	))

(defun one-key-menu-org-agenda-time ()
  "The `one-key' menu for org agenda commands related to timestamps/clock"
  (interactive)
  (one-key-menu "agenda commands" one-key-menu-org-agenda-time-alist))

(defvar one-key-menu-org-agenda-view-alist nil
  "The `one-key' menu alist for org agenda view commands.")

(setq one-key-menu-org-agenda-view-alist
      '(
	(("f" . "Toggle Follow Mode (f)") . org-agenda-follow-mode)
	(("o" . "Delete Other Windows (o)") . delete-other-windows)
	(("d" . "Day View (d)") . org-agenda-day-view)
	(("w" . "Week View (w)") . org-agenda-week-view)
	(("m" . "Month View (m)") . org-agenda-month-view)
	(("y" . "Year View (y)") . org-agenda-year-view)
	(("D" . "Toggle Diary Entries (D)") . org-agenda-toggle-diary)
	(("G" . "Toggle Time Grid (G)") . org-agenda-toggle-time-grid)
	(("g" . "Recreate Agenda View (g)") . org-agenda-redo)
	(("s" . "Save All Org Buffers (s)") . org-save-all-org-buffers)
	(("<left>" . "Show Previous Dates (left)") . org-agenda-earlier)
	(("<right>" . "Show Next Dates (right)") . org-agenda-later)
	(("." . "Goto Today (.)") . org-agenda-goto-today)
	(("c" . "Invoke Column View (C-c C-x C-c)") . org-agenda-columns)
	(("/" . "Filter Agenda View (/)") . org-agenda-filter-by-tag)
	(("\\" . "Refine Filter (\\)") . org-agenda-filter-by-tag-refine)
	(("[" . "Add Search Word With Positive Selection ([)")
. org-agenda-manipulate-query-add)
	(("]" . "Add Search Word With Negative Selection ([)")
. org-agenda-manipulate-query-subtract)
	(("{" . "Add Search Regexp With Positive Selection ({)")
. org-agenda-manipulate-query-add-re)
	(("}" . "Add Search Regexp With Negative Selection (})")
. org-agenda-manipulate-query-subtract-re)
	(("l" . "Toggle Logbook Mode (l)") . org-agenda-log-mode)
	(("v" . "Toggle Archives Mode (v)") . org-agenda-archives-mode)
	(("R" . "Toggle Clockreport Mode (R)") . org-agenda-clockreport-mode)
	(("C-b" . "Back to previous menu") . one-key-menu-org-agenda)
	))

(defun one-key-menu-org-agenda-view ()
  "The `one-key' menu for org agenda view commands."
  (interactive)
  (one-key-menu "agenda commands" one-key-menu-org-agenda-view-alist))

(defvar one-key-menu-org-agenda-calendar-alist nil
  "The `one-key' menu alist for org agenda calendar commands.")

(setq one-key-menu-org-agenda-calendar-alist
      '(
	(("c" . "Open Calendar (c)") . org-agenda-goto-calendar)
	(("i" . "Insert Entry Into Diary (i)") . org-agenda-diary-entry)
	(("M" . "Show Moon Phases (M)") . org-agenda-phases-of-moon)
	(("S" . "Show Sunrise and Sunset (S)") . org-agenda-sunrise-sunset)
	(("C" . "Convert Date To Other Calendar Format (C)") . org-agenda-convert-date)
	(("H" . "Show Holidays for 3 Months Around Date (H)") . org-agenda-holidays)
	(("C-b" . "Back to previous menu") . one-key-menu-org-agenda)
	))

(defun one-key-menu-org-agenda-calendar ()
  "The `one-key' menu for org agenda calendar commands."
  (interactive)
  (one-key-menu "agenda commands" one-key-menu-org-agenda-calendar-alist))

(defvar one-key-menu-org-list-alist nil
  "The `one-key' menu alist for org list commands.")

(setq one-key-menu-org-list-alist
      '(
	(("#" . "Update Checkbox Count (C-c #)") . org-update-checkbox-count)
	(("b" . "Toggle Checkbox (C-c C-x C-b)") . org-toggle-checkbox)
	(("i" . "Insert New Item (M-RET)") . org-meta-return)
	(("c" . "Insert Checkbox Item (M-S-RET)") . org-insert-todo-heading)
	(("-" . "Convert/Toggle Region/Line To List (C-c -)") . org-ctrl-c-minus)
	(("8" . "Convert line/region To/From Header (C-c *)") . org-ctrl-c-star)
	(("C-c" . "Renumber list/Toggle Checkbox (C-c C-c)") . org-ctrl-c-ctrl-c)
	(("C-b" . "Back to previous menu") . one-key-menu-org)
 	))

(defun one-key-menu-org-list ()
  "The `one-key' menu for org list commands"
  (interactive)
  (one-key-menu "list commands" one-key-menu-org-list-alist))

(defvar one-key-menu-org-alter-alist nil
    "The `one-key' menu alist for org commands to alter properties/tags/links etc.")

(setq one-key-menu-org-alter-alist
      '(
	(("a" . "Attach (C-c C-a)") . org-attach)
	(("z" . "Add Note (C-c C-z)") . org-add-note)
	(("k" . "Kill Note Or Show Branches (C-c C-k)") . org-kill-note-or-show-branches)
	(("l" . "Insert Link (C-c C-l)") . org-insert-link)
	(("C-q" . "Set Tags (C-c C-q)") . org-set-tags-command)
	(("o" . "Toggle Ordered Property (C-c C-x o)") . org-toggle-ordered-property)
	(("p" . "Set Property (C-c C-x p)") . org-set-property)
	(("d" . "Deadline (C-c C-d)") . org-deadline)
	((";" . "Toggle Comment (C-c ;)") . org-toggle-comment)
	(("s" . "Schedule (C-c C-s)") . org-schedule)
 	(("<left>" . "Cycle Todo State Backward (S-left)") . org-shiftleft)	
 	(("<right>" . "Cycle Todo State Forward (S-right)") . org-shiftright)	
	(("<S-up>" . "Cycle Priority Up (S-up)") . org-shiftup)
	(("<S-down>" . "Cycle Priority Down (S-down)") . org-shiftdown)
	(("f" . "Emphasize (C-c C-x C-f)") . org-emphasize)
	(("C-b" . "Back to previous menu") . one-key-menu-org)
	))

(defun one-key-menu-org-alter ()
  "The `one-key' menu for org commands to alter properties/tags/links etc."
  (interactive)
  (one-key-menu "alter properties/tags/links etc." one-key-menu-org-alter-alist))

(provide 'ok-org)
;;; ok-org.el ends here
