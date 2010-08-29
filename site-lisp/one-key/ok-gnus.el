;;; ok-gnus.el --- One-key setting for gus

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
(defvar one-key-menu-gnus-alist nil
  "The `one-key' top-level alist.")

(setq one-key-menu-gnus-alist
      '(
	(("a" . "article") . one-key-menu-gnus-article)
	(("g" . "group") . one-key-menu-gnus-group)
	(("m" . "message") . one-key-menu-message)
	(("s" . "summary") . one-key-menu-gnus-summary)
	(("S" . "server") . one-key-menu-gnus-server)
	(("C-b" . "Back to Top Level") . one-key-menu-toplevel)
	))

(defun one-key-menu-gnus ()
  "The `one-key' gnus menu."
  (interactive)
  (one-key-menu "gnus" one-key-menu-gnus-alist))


(defvar one-key-menu-gnus-group-alist nil
  "The `one-key' menu alist for gnus-group.")

(setq one-key-menu-gnus-group-alist
      '(
	(("G" . "Group Commands") . one-key-menu-gnus-group-group)
	(("A" . "List Commands") . one-key-menu-gnus-group-list)
	(("D" . "Sieve Commands") . one-key-menu-gnus-group-sieve)
	(("H" . "Help Commands") . one-key-menu-gnus-group-help)
	(("M" . "Mark Commands") . one-key-menu-gnus-group-mark)
	(("S" . "Sub Commands") . one-key-menu-gnus-group-sub)
	(("T" . "Topic Commands") . one-key-menu-gnus-group-topic)
	(("W" . "Score Commands") . one-key-menu-gnus-group-score)
	(("J" . "Agent Commands") . one-key-menu-gnus-group-agent)
	(("C-c" . "Other Commands") . one-key-menu-gnus-group-other)
	(("C-k" . "Kill Group") . gnus-group-kill-group)
	(("<return>" . "Select Group") . gnus-group-select-group)
	(("C-w" . "Kill Region") . gnus-group-kill-region)
	(("C-y" . "Yank Group") . gnus-group-yank-group)
	(("SPC" . "Read Group") . gnus-group-read-group)
	(("#" . "Mark Group") . gnus-group-mark-group)
	(("," . "Best Unread Group") . gnus-group-best-unread-group)
	(("." . "First Unread Group") . gnus-group-first-unread-group)
	(("<" . "Beginning Of Buffer") . beginning-of-buffer)
	((">" . "End Of Buffer") . end-of-buffer)
	(("d" . "Describe Briefly") . gnus-group-describe-briefly)
	(("B" . "Browse Foreign Server") . gnus-group-browse-foreign-server)
	(("C" . "Catchup Current All") . gnus-group-catchup-current-all)
	(("F" . "Find New Groups") . gnus-group-find-new-groups)
	(("Q" . "Quit") . gnus-group-quit)
	(("R" . "Restart") . gnus-group-restart)
	(("U" . "Unsubscribe Group") . gnus-group-unsubscribe-group)
	(("^" . "Enter Server Mode") . gnus-group-enter-server-mode)
	(("a" . "Post News") . gnus-group-post-news)
	(("b" . "Check Bogus Groups") . gnus-group-check-bogus-groups)
	(("c" . "Catchup Current") . gnus-group-catchup-current)
	(("g" . "Get New News") . gnus-group-get-new-news)
	(("i" . "News") . gnus-group-news)
	(("j" . "Jump To Group") . gnus-group-jump-to-group)
	(("m" . "Mail") . gnus-group-mail)
	(("n" . "Next Unread Group") . gnus-group-next-unread-group)
	(("p" . "Prev Unread Group") . gnus-group-prev-unread-group)
	(("C-q" . "Exit") . gnus-group-exit)
	(("r" . "Read Init File") . gnus-group-read-init-file)
	(("s" . "Save Newsrc") . gnus-group-save-newsrc)
	(("t" . "Gnus Topic Mode") . gnus-topic-mode)
	(("u" . "Unsubscribe Current Group") . gnus-group-unsubscribe-current-group)
	(("z" . "Suspend") . gnus-group-suspend)
	(("<C-M-return>" . "Select Group Ephemerally")
. gnus-group-select-group-ephemerally)
	(("M-RET" . "Quick Select Group") . gnus-group-quick-select-group)
	(("M-SPC" . "Visible Select Group") . gnus-group-visible-select-group)
	(("M-&" . "Universal Argument") . gnus-group-universal-argument)
	(("M-K" . "Edit Global Kill") . gnus-group-edit-global-kill)
	(("M-c" . "Clear Data") . gnus-group-clear-data)
	(("M-d" . "Describe All Groups") . gnus-group-describe-all-groups)
	(("M-g" . "Get New News This Group") . gnus-group-get-new-news-this-group)
	(("M-k" . "Edit Local Kill") . gnus-group-edit-local-kill)
	(("M-n" . "Next Unread Group Same Level")
. gnus-group-next-unread-group-same-level)
	(("M-p" . "Prev Unread Group Same Level")
. gnus-group-prev-unread-group-same-level)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus)
	))

(defun one-key-menu-gnus-group ()
  "The `one-key' menu for gnus-group"
  (interactive)
  (one-key-menu "gnus-group" one-key-menu-gnus-group-alist))

(defvar one-key-menu-gnus-group-group-alist nil
  "The `one-key' menu alist for gnus group group commands.")

(setq one-key-menu-gnus-group-group-alist
      '(
	(("b" . "nnmairix Commands") . one-key-menu-gnus-group-nnmairix)
	(("S" . "Sorting Commands") . one-key-menu-gnus-group-sort)
	(("P" . "Sorting Selected Groups Commands")
. one-key-menu-gnus-group-sort-selected)
	(("s" . "Soup Commands") . one-key-menu-gnus-group-soup)
	(("D" . "Enter Directory") . gnus-group-enter-directory)
	(("E" . "Edit Group") . gnus-group-edit-group)
	(("M" . "Read Ephemeral Group") . gnus-group-read-ephemeral-group)
	(("R" . "Make Rss Group") . gnus-group-make-rss-group)
	(("V" . "Make Empty Virtual") . gnus-group-make-empty-virtual)
	(("a" . "Make Archive Group") . gnus-group-make-archive-group)
	(("c" . "Customize") . gnus-group-customize)
	(("d" . "Make Directory Group") . gnus-group-make-directory-group)
	(("e" . "Edit Group Method") . gnus-group-edit-group-method)
	(("f" . "Make Doc Group") . gnus-group-make-doc-group)
	(("h" . "Make Help Group") . gnus-group-make-help-group)
	(("k" . "Make Kiboze Group") . gnus-group-make-kiboze-group)
	(("l" . "Nnimap Edit Acl") . gnus-group-nnimap-edit-acl)
	(("m" . "Make Group") . gnus-group-make-group)
	(("p" . "Edit Group Parameters") . gnus-group-edit-group-parameters)
	(("r" . "Rename Group") . gnus-group-rename-group)
	(("u" . "Make Useful Group") . gnus-group-make-useful-group)
	(("v" . "Add To Virtual") . gnus-group-add-to-virtual)
	(("w" . "Make Web Group") . gnus-group-make-web-group)
	(("x" . "Nnimap Expunge") . gnus-group-nnimap-expunge)
	(("z" . "Compact Group") . gnus-group-compact-group)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group)
	))

(defun one-key-menu-gnus-group-group ()
  "The `one-key' menu for gnus group group commands."
  (interactive)
  (one-key-menu "gnus-group-group" one-key-menu-gnus-group-group-alist))

(defvar one-key-menu-gnus-group-nnmairix-alist nil
  "The `one-key' menu alist for gnus group nnmairix commands.")

(setq one-key-menu-gnus-group-nnmairix-alist
      '(
	(("a" . "Group Toggle Allowfast This Group")
. nnmairix-group-toggle-allowfast-this-group)
	(("c" . "Create Server And Default Group")
. nnmairix-create-server-and-default-group)
	(("d" . "Group Delete Recreate This Group")
. nnmairix-group-delete-recreate-this-group)
	(("g" . "Create Search Group") . nnmairix-create-search-group)
	(("i" . "Search Interactive") . nnmairix-search-interactive)
	(("m" . "Widget Search") . nnmairix-widget-search)
	(("o" . "Propagate Marks") . nnmairix-propagate-marks)
	(("p" . "Group Toggle Propmarks This Group")
. nnmairix-group-toggle-propmarks-this-group)
	(("C-q" . "Group Change Query This Group")
. nnmairix-group-change-query-this-group)
	(("r" . "Group Toggle Readmarks This Group")
. nnmairix-group-toggle-readmarks-this-group)
	(("s" . "Search") . nnmairix-search)
	(("t" . "Group Toggle Threads This Group")
. nnmairix-group-toggle-threads-this-group)
	(("u" . "Update Database") . nnmairix-update-database)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-group)
	))

(defun one-key-menu-gnus-group-nnmairix ()
  "The `one-key' menu for gnus group nnmairix commands."
  (interactive)
  (one-key-menu "gnus-group-nnmairix" one-key-menu-gnus-group-nnmairix-alist))


(defvar one-key-menu-gnus-group-sort-alist nil
  "The `one-key' menu alist for gnus group sort commands.")

(setq one-key-menu-gnus-group-sort-alist
      '(
	(("a" . "Sort Groups By Alphabet") . gnus-group-sort-groups-by-alphabet)
	(("l" . "Sort Groups By Level") . gnus-group-sort-groups-by-level)
	(("m" . "Sort Groups By Method") . gnus-group-sort-groups-by-method)
	(("n" . "Sort Groups By Real Name") . gnus-group-sort-groups-by-real-name)
	(("r" . "Sort Groups By Rank") . gnus-group-sort-groups-by-rank)
	(("s" . "Sort Groups") . gnus-group-sort-groups)
	(("u" . "Sort Groups By Unread") . gnus-group-sort-groups-by-unread)
	(("v" . "Sort Groups By Score") . gnus-group-sort-groups-by-score)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-group)
	))

(defun one-key-menu-gnus-group-sort ()
  "The `one-key' menu for gnus group sort commands"
  (interactive)
  (one-key-menu "gnus-group-sort" one-key-menu-gnus-group-sort-alist))


(defvar one-key-menu-gnus-group-sort-selected-alist nil
  "The `one-key' menu alist for gnus group sort selected commands.")

(setq one-key-menu-gnus-group-sort-selected-alist
      '(
	(("a" . "Sort Selected Groups By Alphabet")
. gnus-group-sort-selected-groups-by-alphabet)
	(("l" . "Sort Selected Groups By Level")
. gnus-group-sort-selected-groups-by-level)
	(("m" . "Sort Selected Groups By Method")
. gnus-group-sort-selected-groups-by-method)
	(("n" . "Sort Selected Groups By Real Name")
. gnus-group-sort-selected-groups-by-real-name)
	(("r" . "Sort Selected Groups By Rank") . gnus-group-sort-selected-groups-by-rank)
	(("s" . "Sort Selected Groups") . gnus-group-sort-selected-groups)
	(("u" . "Sort Selected Groups By Unread")
. gnus-group-sort-selected-groups-by-unread)
	(("v" . "Sort Selected Groups By Score")
. gnus-group-sort-selected-groups-by-score)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-group)
	))

(defun one-key-menu-gnus-group-sort-selected ()
  "The `one-key' menu for gnus group sort selected commands"
  (interactive)
  (one-key-menu "gnus-group-sort-selected" one-key-menu-gnus-group-sort-selected-alist))


(defvar one-key-menu-gnus-group-soup-alist nil
  "The `one-key' menu alist for gnus group soup commands.")

(setq one-key-menu-gnus-group-soup-alist
      '(
	(("b" . "Brew Soup") . gnus-group-brew-soup)
	(("p" . "Pack Packet") . gnus-soup-pack-packet)
	(("r" . "Nnsoup Pack Replies") . nnsoup-pack-replies)
	(("s" . "Send Replies") . gnus-soup-send-replies)
	(("w" . "Save Areas") . gnus-soup-save-areas)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-group)
	))

(defun one-key-menu-gnus-group-soup ()
  "The `one-key' menu for gnus group soup commands"
  (interactive)
  (one-key-menu "gnus-group-soup" one-key-menu-gnus-group-soup-alist))

(defvar one-key-menu-gnus-group-list-alist nil
  "The `one-key' menu alist for gnus-group-list.")

(setq one-key-menu-gnus-group-list-alist
      '(
	(("/" . "Limit Commands") . gnus-group-list-limit)
	(("D" . "Dormant") . gnus-group-list-dormant)
	(("A" . "Active") . gnus-group-list-active)
	(("M" . "All Matching") . gnus-group-list-all-matching)
	(("a" . "Apropos") . gnus-group-apropos)
	(("c" . "Cached") . gnus-group-list-cached)
	(("d" . "Description Apropos") . gnus-group-description-apropos)
	(("f" . "Flush Commands") . gnus-group-list-flush)
	(("k" . "Killed") . gnus-group-list-killed)
	(("l" . "Level") . gnus-group-list-level)
	(("m" . "Matching") . gnus-group-list-matching)
	(("p" . "Plus Commands") . gnus-group-list-plus)
	(("s" . "Groups") . gnus-group-list-groups)
	(("u" . "All Groups") . gnus-group-list-all-groups)
	(("z" . "Zombies") . gnus-group-list-zombies)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group)
	))

(defun one-key-menu-gnus-group-list ()
  "The `one-key' menu for gnus-group-list"
  (interactive)
  (one-key-menu "gnus-group-list" one-key-menu-gnus-group-list-alist))

(defvar one-key-menu-gnus-group-sieve-alist nil
  "The `one-key' menu alist for gnus-group-sieve.")

(setq one-key-menu-gnus-group-sieve-alist
      '(
	(("g" . "Sieve Generate") . gnus-sieve-generate)
	(("u" . "Sieve Update") . gnus-sieve-update)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group)
	))

(defun one-key-menu-gnus-group-sieve ()
  "The `one-key' menu for gnus-group-sieve"
  (interactive)
  (one-key-menu "gnus-group-sieve" one-key-menu-gnus-group-sieve-alist))

(defvar one-key-menu-gnus-group-help-alist nil
  "The `one-key' menu alist for gnus-group-help.")

(setq one-key-menu-gnus-group-help-alist
      '(
	(("C" . "Fetch Control") . gnus-group-fetch-control)
	(("c" . "Fetch Charter") . gnus-group-fetch-charter)
	(("d" . "Describe Group") . gnus-group-describe-group)
	(("f" . "Fetch Faq") . gnus-group-fetch-faq)
	(("v" . "Gnus Version") . gnus-version)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group)
	))

(defun one-key-menu-gnus-group-help ()
  "The `one-key' menu for gnus-group-help"
  (interactive)
  (one-key-menu "gnus-group-help" one-key-menu-gnus-group-help-alist))

(defvar one-key-menu-gnus-group-mark-alist nil
  "The `one-key' menu alist for gnus-group-mark.")

(setq one-key-menu-gnus-group-mark-alist
      '(
	(("U" . "Unmark All Groups") . gnus-group-unmark-all-groups)
	(("b" . "Mark Buffer") . gnus-group-mark-buffer)
	(("m" . "Mark Group") . gnus-group-mark-group)
	(("r" . "Mark Regexp") . gnus-group-mark-regexp)
	(("u" . "Unmark Group") . gnus-group-unmark-group)
	(("w" . "Mark Region") . gnus-group-mark-region)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group)
	))

(defun one-key-menu-gnus-group-mark ()
  "The `one-key' menu for gnus-group-mark"
  (interactive)
  (one-key-menu "gnus-group-mark" one-key-menu-gnus-group-mark-alist))

(defvar one-key-menu-gnus-group-sub-alist nil
  "The `one-key' menu alist for gnus-group-sub.")

(setq one-key-menu-gnus-group-sub-alist
      '(
	(("C-k" . "Kill Level") . gnus-group-kill-level)
	(("k" . "Kill Group") . gnus-group-kill-group)
	(("l" . "Set Current Level") . gnus-group-set-current-level)
	(("s" . "Unsubscribe Group") . gnus-group-unsubscribe-group)
	(("t" . "Unsubscribe Current Group") . gnus-group-unsubscribe-current-group)
	(("w" . "Kill Region") . gnus-group-kill-region)
	(("y" . "Yank Group") . gnus-group-yank-group)
	(("z" . "Kill All Zombies") . gnus-group-kill-all-zombies)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group)
	))

(defun one-key-menu-gnus-group-sub ()
  "The `one-key' menu for gnus-group-sub"
  (interactive)
  (one-key-menu "gnus-group-sub" one-key-menu-gnus-group-sub-alist))

(defvar one-key-menu-gnus-group-topic-alist nil
  "The `one-key' menu alist for gnus-group-topic.")

(setq one-key-menu-gnus-group-topic-alist
      '(
	(("#" . "Mark Topic") . gnus-topic-mark-topic)
	(("C" . "Copy Matching") . gnus-topic-copy-matching)
	(("D" . "Remove Group") . gnus-topic-remove-group)
	(("H" . "Toggle Display Empty Topics") . gnus-topic-toggle-display-empty-topics)
	(("M" . "Move Matching") . gnus-topic-move-matching)
	(("S" . "Sort Commands") . one-key-menu-gnus-group-topic-sort)
	(("c" . "Copy Group") . gnus-topic-copy-group)
	(("h" . "Hide Topic") . gnus-topic-hide-topic)
	(("j" . "Jump To Topic") . gnus-topic-jump-to-topic)
	(("m" . "Move Group") . gnus-topic-move-group)
	(("n" . "Create Topic") . gnus-topic-create-topic)
	(("r" . "Rename") . gnus-topic-rename)
	(("s" . "Show Topic") . gnus-topic-show-topic)
	(("<delete>" . "Delete") . gnus-topic-delete)
	(("<delete>" . "Delete") . gnus-topic-delete)
	(("<tab>" . "Indent") . gnus-topic-indent)
	(("C-S-u" . "Un-indent") . gnus-topic-unindent)	
	(("M-#" . "Unmark Topic") . gnus-topic-unmark-topic)
	(("M-n" . "Goto Next Topic") . gnus-topic-goto-next-topic)
	(("M-p" . "Goto Previous Topic") . gnus-topic-goto-previous-topic)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group)
	))

(defun one-key-menu-gnus-group-topic ()
  "The `one-key' menu for gnus-group-topic"
  (interactive)
  (one-key-menu "gnus-group-topic" one-key-menu-gnus-group-topic-alist))

(defvar one-key-menu-gnus-group-topic-sort-alist nil
  "The `one-key' menu alist for gnus-group-topic-sort.")

(setq one-key-menu-gnus-group-topic-sort-alist
      '(
	(("a" . "Sort Groups By Alphabet") . gnus-topic-sort-groups-by-alphabet)
	(("e" . "Sort Groups By Server") . gnus-topic-sort-groups-by-server)
	(("l" . "Sort Groups By Level") . gnus-topic-sort-groups-by-level)
	(("m" . "Sort Groups By Method") . gnus-topic-sort-groups-by-method)
	(("r" . "Sort Groups By Rank") . gnus-topic-sort-groups-by-rank)
	(("s" . "Sort Groups") . gnus-topic-sort-groups)
	(("u" . "Sort Groups By Unread") . gnus-topic-sort-groups-by-unread)
	(("v" . "Sort Groups By Score") . gnus-topic-sort-groups-by-score)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-topic)
	))

(defun one-key-menu-gnus-group-topic-sort ()
  "The `one-key' menu for gnus-group-topic-sort"
  (interactive)
  (one-key-menu "gnus-group-topic-sort" one-key-menu-gnus-group-topic-sort-alist))

(defvar one-key-menu-gnus-group-score-alist nil
  "The `one-key' menu alist for gnus-group-score.")

(setq one-key-menu-gnus-group-score-alist
      '(
	(("e" . "Edit All Score") . gnus-score-edit-all-score)
	(("f" . "Flush Cache") . gnus-score-flush-cache)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group)
	))

(defun one-key-menu-gnus-group-score ()
  "The `one-key' menu for gnus-group-score"
  (interactive)
  (one-key-menu "gnus-group-score" one-key-menu-gnus-group-score-alist))

(defvar one-key-menu-gnus-group-agent-alist nil
  "The `one-key' menu alist for gnus-group-agent.")

(setq one-key-menu-gnus-group-agent-alist
      '(
	(("S" . "Send Queue") . gnus-group-send-queue)
	(("Y" . "Agent Synchronize Flags") . gnus-agent-synchronize-flags)
	(("a" . "Agent Add Group") . gnus-agent-add-group)
	(("c" . "Enter Category Buffer") . gnus-enter-category-buffer)
	(("j" . "Agent Toggle Plugged") . gnus-agent-toggle-plugged)
	(("o" . "Agent Toggle Group Plugged") . gnus-agent-toggle-group-plugged)
	(("r" . "Agent Remove Group") . gnus-agent-remove-group)
	(("s" . "Agent Fetch Session") . gnus-agent-fetch-session)
	(("u" . "Agent Fetch Groups") . gnus-agent-fetch-groups)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group)
	))

(defun one-key-menu-gnus-group-agent ()
  "The `one-key' menu for gnus-group-agent"
  (interactive)
  (one-key-menu "gnus-group-agent" one-key-menu-gnus-group-agent-alist))

(defvar one-key-menu-gnus-group-other-alist nil
  "The `one-key' menu alist for gnus group other commands.")

(setq one-key-menu-gnus-group-other-alist
      '(
	(("C-a" . "Group Apropos") . gnus-group-apropos)
	(("C-S-b" . "Gnus Bug") . gnus-bug)
	(("C-d" . "Describe Group") . gnus-group-describe-group)
	(("<tab>" . "Gnus Info Find Node") . gnus-info-find-node)
	(("C-l" . "List Killed") . gnus-group-list-killed)
	(("C-s" . "Sort Groups") . gnus-group-sort-groups)
	(("C-x" . "Expire Articles") . gnus-group-expire-articles)
	(("C-t" . "Transpose Groups") . gnus-group-transpose-groups)
	(("C-M-a" . "Description Apropos") . gnus-group-description-apropos)
	(("C-M-x" . "Expire All Groups") . gnus-group-expire-all-groups)
	(("M-g" . "Gnus Activate All Groups") . gnus-activate-all-groups)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group)
	))

(defun one-key-menu-gnus-group-other ()
  "The `one-key' menu for gnus group other commands."
  (interactive)
  (one-key-menu "gnus-group-other" one-key-menu-gnus-group-other-alist))

(defvar one-key-menu-gnus-summary-alist nil
  "The `one-key' menu alist for gnus-summary.")

(setq one-key-menu-gnus-summary-alist
      '(
	(("C-c" . "Other Commands") . one-key-menu-gnus-summary-other)
	(("$" . "nnmairix Commands") . one-key-menu-gnus-summary-nnmairix)
	(("/" . "Limit Commands") . one-key-menu-gnus-summary-limit)
	(("A" . "Article Commands") . one-key-menu-gnus-summary-article)
	(("B" . "Backend Commands") . one-key-menu-gnus-summary-backend)
	(("G" . "Goto Commands") . one-key-menu-gnus-summary-goto)
	(("H" . "Help Commands") . one-key-menu-gnus-summary-help)
	(("K" . "Mime Commands") . one-key-menu-gnus-summary-mime)
	(("M" . "Mark Commands") . one-key-menu-gnus-summary-mark)
	(("O" . "Save Commands") . one-key-menu-gnus-summary-save)
	(("S" . "Send Commands") . one-key-menu-gnus-summary-send)
	(("T" . "Thread Commands") . one-key-menu-gnus-summary-thread)
	(("V" . "Score Commands") . one-key-menu-gnus-summary-score)
	(("W" . "Wash Commands") . one-key-menu-gnus-summary-wash)
	(("X" . "Uu Extract Commands") . one-key-menu-gnus-summary-uu-extract)
	(("Y" . "Buffer Commands") . one-key-menu-gnus-summary-buffer)
	(("Z" . "Exit Commands") . one-key-menu-gnus-summary-exit)
	(("C-l" . "Gnus Recenter") . gnus-recenter)
	(("C-t" . "Toggle Truncation") . gnus-summary-toggle-truncation)
	(("!" . "Tick Article Forward") . gnus-summary-tick-article-forward)
	(("&" . "Execute Command") . gnus-summary-execute-command)
	(("*" . "Gnus Cache Enter Article") . gnus-cache-enter-article)
	((":" . "Bbdb/Gnus Show Sender") . bbdb/gnus-show-sender)
	((";" . "Bbdb/Gnus Edit Notes") . bbdb/gnus-edit-notes)
	(("=" . "Expand Window") . gnus-summary-expand-window)
	(("I" . "Increase Score") . gnus-summary-increase-score)
	(("L" . "Lower Score") . gnus-summary-lower-score)
	(("U" . "Tick Article Backward") . gnus-summary-tick-article-backward)
	(("h" . "Select Article Buffer") . gnus-summary-select-article-buffer)
	(("C-M-a" . "Customize Parameters") . gnus-summary-customize-parameters)
	(("C-M-d" . "Read Document") . gnus-summary-read-document)
	(("C-M-e" . "Edit Parameters") . gnus-summary-edit-parameters)
	(("M-&" . "Universal Argument") . gnus-summary-universal-argument)
	(("M-*" . "Gnus Cache Remove Article") . gnus-cache-remove-article)
	(("M-K" . "Edit Global Kill") . gnus-summary-edit-global-kill)
	(("M-R" . "Repeat Search Article Backward")
. gnus-summary-repeat-search-article-backward)
	(("M-S" . "Repeat Search Article Forward")
. gnus-summary-repeat-search-article-forward)
	(("M-U" . "Clear Mark Backward") . gnus-summary-clear-mark-backward)
	(("M-^" . "Refer Article") . gnus-summary-refer-article)
	(("M-g" . "Rescan Group") . gnus-summary-rescan-group)
	(("M-i" . "Gnus Symbolic Argument") . gnus-symbolic-argument)
	(("M-k" . "Edit Local Kill") . gnus-summary-edit-local-kill)
	(("M-r" . "Search Article Backward") . gnus-summary-search-article-backward)
	(("M-s" . "Search Article Forward") . gnus-summary-search-article-forward)
	(("M-t" . "Toggle Display Buttonized") . gnus-summary-toggle-display-buttonized)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus)
	))

(defun one-key-menu-gnus-summary ()
  "The `one-key' menu for gnus-summary"
  (interactive)
  (one-key-menu "gnus-summary" one-key-menu-gnus-summary-alist))

(defvar one-key-menu-gnus-summary-other-alist nil
  "The `one-key' menu alist for gnus-summary-other.")

(setq one-key-menu-gnus-summary-other-alist
      '(
	(("C-s" . "Sort/View Commands") . one-key-menu-gnus-summary-sort)
	(("C-S-b" . "Gnus Bug") . gnus-bug)
	(("C-d" . "Describe Group") . gnus-summary-describe-group)
	(("C-f" . "Mail Forward") . gnus-summary-mail-forward)
	(("<tab>" . "Gnus Info Find Node") . gnus-info-find-node)
	(("C-r" . "Caesar Message") . gnus-summary-caesar-message)
	(("C-M-s" . "Limit Include Expunged") . gnus-summary-limit-include-expunged)
	(("C-x C-s" . "Reselect Current Group") . gnus-summary-reselect-current-group)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-other ()
  "The `one-key' menu for gnus-summary-other"
  (interactive)
  (one-key-menu "gnus-summary-other" one-key-menu-gnus-summary-other-alist))


(defvar one-key-menu-gnus-summary-sort-alist nil
  "The `one-key' menu alist for gnus-summary-sort.")

(setq one-key-menu-gnus-summary-sort-alist
      '(
	(("C-v" . "Uu Decode Uu View") . gnus-uu-decode-uu-view)
	(("C-a" . "Sort By Author") . gnus-summary-sort-by-author)
	(("C-c" . "Sort By Chars") . gnus-summary-sort-by-chars)
	(("C-d" . "Sort By Date") . gnus-summary-sort-by-date)
	(("<tab>" . "Sort By Score") . gnus-summary-sort-by-score)
	(("C-l" . "Sort By Lines") . gnus-summary-sort-by-lines)
	(("C-n" . "Sort By Number") . gnus-summary-sort-by-number)
	(("C-o" . "Sort By Original") . gnus-summary-sort-by-original)
	(("C-r" . "Sort By Random") . gnus-summary-sort-by-random)
	(("C-s" . "Sort By Subject") . gnus-summary-sort-by-subject)
	(("C-t" . "Sort By Recipient") . gnus-summary-sort-by-recipient)
	(("C-S-d" . "Sort By Most Recent Date") . gnus-summary-sort-by-most-recent-date)
	(("C-S-n" . "Sort By Most Recent Number")
. gnus-summary-sort-by-most-recent-number)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))
(defun one-key-menu-gnus-summary-sort ()
  "The `one-key' menu for gnus-summary-sort"
  (interactive)
  (one-key-menu "gnus-summary-sort" one-key-menu-gnus-summary-sort-alist))

(defvar one-key-menu-gnus-summary-nnmairix-alist nil
  "The `one-key' menu alist for gnus-summary-nnmairix.")
(setq one-key-menu-gnus-summary-nnmairix-alist
      '(
	(("f" . "Search From This Article") . nnmairix-search-from-this-article)
	(("g" . "Create Search Group From Message")
. nnmairix-create-search-group-from-message)
	(("m" . "Widget Search From This Article")
. nnmairix-widget-search-from-this-article)
	(("o" . "Goto Original Article") . nnmairix-goto-original-article)
	(("t" . "Search Thread This Article") . nnmairix-search-thread-this-article)
	(("u" . "Remove Tick Mark Original Article")
. nnmairix-remove-tick-mark-original-article)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-nnmairix ()
  "The `one-key' menu for gnus-summary-nnmairix"
  (interactive)
  (one-key-menu "gnus-summary-nnmairix" one-key-menu-gnus-summary-nnmairix-alist))

(defvar one-key-menu-gnus-summary-limit-alist nil
  "The `one-key' menu alist for gnus-summary-limit.")

(setq one-key-menu-gnus-summary-limit-alist
      '(
	(("*" . "Limit Include Cached") . gnus-summary-limit-include-cached)
	(("." . "Limit To Unseen") . gnus-summary-limit-to-unseen)
	(("/" . "Limit To Subject") . gnus-summary-limit-to-subject)
	(("A" . "Limit To Address") . gnus-summary-limit-to-address)
	(("C" . "Limit Mark Excluded As Read") . gnus-summary-limit-mark-excluded-as-read)
	(("D" . "Limit Include Dormant") . gnus-summary-limit-include-dormant)
	(("E" . "Limit Include Expunged") . gnus-summary-limit-include-expunged)
	(("M" . "Limit Exclude Marks") . gnus-summary-limit-exclude-marks)
	(("N" . "Insert New Articles") . gnus-summary-insert-new-articles)
	(("R" . "Limit To Recipient") . gnus-summary-limit-to-recipient)
	(("S" . "Limit To Singletons") . gnus-summary-limit-to-singletons)
	(("T" . "Limit Include Thread") . gnus-summary-limit-include-thread)
	(("a" . "Limit To Author") . gnus-summary-limit-to-author)
	(("b" . "Limit To Bodies") . gnus-summary-limit-to-bodies)
	(("c" . "Limit Exclude Childless Dormant")
. gnus-summary-limit-exclude-childless-dormant)
	(("d" . "Limit Exclude Dormant") . gnus-summary-limit-exclude-dormant)
	(("h" . "Limit To Headers") . gnus-summary-limit-to-headers)
	(("m" . "Limit To Marks") . gnus-summary-limit-to-marks)
	(("n" . "Limit To Articles") . gnus-summary-limit-to-articles)
	(("o" . "Insert Old Articles") . gnus-summary-insert-old-articles)
	(("p" . "Limit To Display Predicate") . gnus-summary-limit-to-display-predicate)
	(("r" . "Limit To Replied") . gnus-summary-limit-to-replied)
	(("s" . "Limit To Subject") . gnus-summary-limit-to-subject)
	(("t" . "Limit To Age") . gnus-summary-limit-to-age)
	(("u" . "Limit To Unread") . gnus-summary-limit-to-unread)
	(("v" . "Limit To Score") . gnus-summary-limit-to-score)
	(("w" . "Pop Limit") . gnus-summary-pop-limit)
	(("x" . "Limit To Extra") . gnus-summary-limit-to-extra)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-limit ()
  "The `one-key' menu for gnus-summary-limit"
  (interactive)
  (one-key-menu "gnus-summary-limit" one-key-menu-gnus-summary-limit-alist))

(defvar one-key-menu-gnus-summary-article-alist nil
  "The `one-key' menu alist for gnus-summary-article.")

(setq one-key-menu-gnus-summary-article-alist
      '(
	(("<return>" . "Scroll Up") . gnus-summary-scroll-up)
	(("M-RET" . "Scroll Down") . gnus-summary-scroll-down)
	(("SPC" . "Next Page") . gnus-summary-next-page)
	(("<delete>" . "Prev Page") . gnus-summary-prev-page)
	(("<" . "Beginning Of Article") . gnus-summary-beginning-of-article)
	((">" . "End Of Article") . gnus-summary-end-of-article)
	(("D" . "Enter Digest Group") . gnus-summary-enter-digest-group)
	(("M" . "Gnus Mailing List Insinuate") . gnus-mailing-list-insinuate)
	(("P" . "Print Article") . gnus-summary-print-article)
	(("R" . "Refer References") . gnus-summary-refer-references)
	(("S" . "Gnus Sticky Article") . gnus-sticky-article)
	(("T" . "Refer Thread") . gnus-summary-refer-thread)
	(("^" . "Refer Parent Article") . gnus-summary-refer-parent-article)
	(("g" . "Show Article") . gnus-summary-show-article)
	(("r" . "Refer Parent Article") . gnus-summary-refer-parent-article)
	(("s" . "Isearch Article") . gnus-summary-isearch-article)
	(("t" . "Babel") . gnus-article-babel)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-article ()
  "The `one-key' menu for gnus-summary-article"
  (interactive)
  (one-key-menu "gnus-summary-article" one-key-menu-gnus-summary-article-alist))

(defvar one-key-menu-gnus-summary-backend-alist nil
  "The `one-key' menu alist for gnus-summary-backend.")

(setq one-key-menu-gnus-summary-backend-alist
      '(
	(("B" . "Crosspost Article") . gnus-summary-crosspost-article)
	(("I" . "Create Article") . gnus-summary-create-article)
	(("c" . "Copy Article") . gnus-summary-copy-article)
	(("e" . "Expire Articles") . gnus-summary-expire-articles)
	(("i" . "Import Article") . gnus-summary-import-article)
	(("m" . "Move Article") . gnus-summary-move-article)
	(("p" . "Article Posted P") . gnus-summary-article-posted-p)
	(("Q" . "Respool Query") . gnus-summary-respool-query)
	(("r" . "Respool Article") . gnus-summary-respool-article)
	(("t" . "Respool Trace") . gnus-summary-respool-trace)
	(("w" . "Edit Article") . gnus-summary-edit-article)
	(("<delete>" . "Delete Article") . gnus-summary-delete-article)
	(("C-M-e" . "Expire Articles Now") . gnus-summary-expire-articles-now)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-backend ()
  "The `one-key' menu for gnus-summary-backend"
  (interactive)
  (one-key-menu "gnus-summary-backend" one-key-menu-gnus-summary-backend-alist))

(defvar one-key-menu-gnus-summary-goto-alist nil
  "The `one-key' menu alist for gnus-summary-goto.")

(setq one-key-menu-gnus-summary-goto-alist
      '(
	(("C-n" . "Next Same Subject") . gnus-summary-next-same-subject)
	(("C-p" . "Prev Same Subject") . gnus-summary-prev-same-subject)
	(("N" . "Next Article") . gnus-summary-next-article)
	(("P" . "Prev Article") . gnus-summary-prev-article)
	(("b" . "Best Unread Article") . gnus-summary-best-unread-article)
	(("f" . "First Unread Article") . gnus-summary-first-unread-article)
	(("g" . "Goto Subject") . gnus-summary-goto-subject)
	(("j" . "Goto Article") . gnus-summary-goto-article)
	(("l" . "Goto Last Article") . gnus-summary-goto-last-article)
	(("n" . "Next Unread Article") . gnus-summary-next-unread-article)
	(("o" . "Pop Article") . gnus-summary-pop-article)
	(("p" . "Prev Unread Article") . gnus-summary-prev-unread-article)
	(("M-n" . "Next Unread Subject") . gnus-summary-next-unread-subject)
	(("M-p" . "Prev Unread Subject") . gnus-summary-prev-unread-subject)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-goto ()
  "The `one-key' menu for gnus-summary-goto"
  (interactive)
  (one-key-menu "gnus-summary-goto" one-key-menu-gnus-summary-goto-alist))

(defvar one-key-menu-gnus-summary-help-alist nil
  "The `one-key' menu alist for gnus-summary-help.")

(setq one-key-menu-gnus-summary-help-alist
      '(
	(("C" . "Fetch Control") . gnus-group-fetch-control)
	(("c" . "Fetch Charter") . gnus-group-fetch-charter)
	(("d" . "Describe Group") . gnus-summary-help-describe-group)
	(("f" . "Fetch Faq") . gnus-summary-help-fetch-faq)
	(("h" . "Describe Briefly") . gnus-summary-help-describe-briefly)
	(("i" . "Gnus Info Find Node") . gnus-info-find-node)
	(("v" . "Gnus Version") . gnus-version)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-help ()
  "The `one-key' menu for gnus-summary-help"
  (interactive)
  (one-key-menu "gnus-summary-help" one-key-menu-gnus-summary-help-alist))

(defvar one-key-menu-gnus-summary-mime-alist nil
  "The `one-key' menu alist for gnus-summary-mime.")

(setq one-key-menu-gnus-summary-mime-alist
      '(
	(("C" . "View Part As Charset") . gnus-article-view-part-as-charset)
	(("E" . "Encrypt Body") . gnus-article-encrypt-body)
	(("H" . "Browse Html Article") . gnus-article-browse-html-article)
	(("O" . "Save Part And Strip") . gnus-article-save-part-and-strip)
	(("b" . "Display Buttonized") . gnus-summary-display-buttonized)
	(("c" . "Copy Part") . gnus-article-copy-part)
	(("d" . "Delete Part") . gnus-article-delete-part)
	(("e" . "View Part Externally") . gnus-article-view-part-externally)
	(("i" . "Inline Part") . gnus-article-inline-part)
	(("j" . "Jump To Part") . gnus-article-jump-to-part)
	(("m" . "Repair Multipart") . gnus-summary-repair-multipart)
	(("o" . "Save Part") . gnus-article-save-part)
	(("r" . "Replace Part") . gnus-article-replace-part)
	(("t" . "View Part As Type") . gnus-article-view-part-as-type)
	(("v" . "View Part") . gnus-article-view-part)
	(("|" . "Pipe Part") . gnus-article-pipe-part)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-mime ()
  "The `one-key' menu for gnus-summary-mime"
  (interactive)
  (one-key-menu "gnus-summary-mime" one-key-menu-gnus-summary-mime-alist))

(defvar one-key-menu-gnus-summary-mark-alist nil
  "The `one-key' menu alist for gnus-summary-mark.")

(setq one-key-menu-gnus-summary-mark-alist
      '(
	(("M" . "Registry Commands") . one-key-menu-gnus-registry-mark)
	(("V" . "Mscore Commands") . one-key-menu-gnus-summary-mscore)
	(("P" . "UUmark Commands") . one-key-menu-gnus-uu-mark)
	(("C-c" . "Catchup All") . gnus-summary-catchup-all)
	(("SPC" . "Clear Mark Forward") . gnus-summary-clear-mark-forward)
	(("!" . "Tick Article Forward") . gnus-summary-tick-article-forward)
	(("#" . "Mark As Processable") . gnus-summary-mark-as-processable)
	(("D" . "Mark As Dormant") . gnus-summary-mark-as-dormant)
	(("B" . "Remove Bookmark") . gnus-summary-remove-bookmark)
	(("C" . "Catchup") . gnus-summary-catchup)
	(("H" . "Catchup To Here") . gnus-summary-catchup-to-here)
	(("K" . "Kill Same Subject") . gnus-summary-kill-same-subject)
	(("S" . "Limit Include Expunged") . gnus-summary-limit-include-expunged)
	(("b" . "Set Bookmark") . gnus-summary-set-bookmark)
	(("c" . "Clear Mark Forward") . gnus-summary-clear-mark-forward)
	(("e" . "Mark As Expirable") . gnus-summary-mark-as-expirable)
	(("h" . "Catchup From Here") . gnus-summary-catchup-from-here)
	(("k" . "Kill Same Subject And Select")
. gnus-summary-kill-same-subject-and-select)
	(("r" . "Mark As Read Forward") . gnus-summary-mark-as-read-forward)
	(("C-r" . "Mark As Read Backward") . gnus-summary-mark-as-read-backward)
	(("C-w" . "Mark Region As Read") . gnus-summary-mark-region-as-read)
	(("t" . "Tick Article Forward") . gnus-summary-tick-article-forward)
	(("x" . "Mark As Expirable") . gnus-summary-mark-as-expirable)
	(("M-#" . "Unmark As Processable") . gnus-summary-unmark-as-processable)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-mark ()
  "The `one-key' menu for gnus-summary-mark"
  (interactive)
  (one-key-menu "gnus-summary-mark" one-key-menu-gnus-summary-mark-alist))

(defvar one-key-menu-gnus-registry-mark-alist nil
  "The `one-key' menu alist for gnus-registry-mark.")

(setq one-key-menu-gnus-registry-mark-alist
      '(
	(("i" . "set Article Important Mark") . gnus-registry-set-article-Important-mark)
	(("l" . "set Article Later Mark") . gnus-registry-set-article-Later-mark)
	(("p" . "set Article Personal Mark") . gnus-registry-set-article-Personal-mark)
	(("t" . "set Article To Do Mark") . gnus-registry-set-article-To-Do-mark)
	(("w" . "set Article Work Mark") . gnus-registry-set-article-Work-mark)
	(("C-i" . "Remove Article Important Mark")
. gnus-registry-remove-article-Important-mark)
	(("C-l" . "Remove Article Later Mark") . gnus-registry-remove-article-Later-mark)
	(("C-p" . "Remove Article Personal Mark")
. gnus-registry-remove-article-Personal-mark)
	(("C-t" . "Remove Article To Do Mark") . gnus-registry-remove-article-To-Do-mark)
	(("C-w" . "Remove Article Work Mark") . gnus-registry-remove-article-Work-mark)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary-mark)
	))

(defun one-key-menu-gnus-registry-mark ()
  "The `one-key' menu for gnus-registry-mark"
  (interactive)
  (one-key-menu "gnus-registry-mark" one-key-menu-gnus-registry-mark-alist))

(defvar one-key-menu-gnus-summary-mscore-alist nil
  "The `one-key' menu alist for gnus-summary-mscore.")

(setq one-key-menu-gnus-summary-mscore-alist
      '(
	(("c" . "Clear Above") . gnus-summary-clear-above)
	(("k" . "Kill Below") . gnus-summary-kill-below)
	(("m" . "Mark Above") . gnus-summary-mark-above)
	(("u" . "Tick Above") . gnus-summary-tick-above)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary-mark)
	))

(defun one-key-menu-gnus-summary-mscore ()
  "The `one-key' menu for gnus-summary-mscore"
  (interactive)
  (one-key-menu "gnus-summary-mscore" one-key-menu-gnus-summary-mscore-alist))

(defvar one-key-menu-gnus-uu-mark-alist nil
  "The `one-key' menu alist for gnus-uu-mark.")

(setq one-key-menu-gnus-uu-mark-alist
      '(
	(("G" . "Uu Unmark By Regexp") . gnus-uu-unmark-by-regexp)
	(("R" . "Uu Mark By Regexp") . gnus-uu-mark-by-regexp)
	(("S" . "Uu Mark Sparse") . gnus-uu-mark-sparse)
	(("T" . "Uu Unmark Thread") . gnus-uu-unmark-thread)
	(("U" . "Unmark All Processable") . gnus-summary-unmark-all-processable)
	(("a" . "Uu Mark All") . gnus-uu-mark-all)
	(("b" . "Uu Mark Buffer") . gnus-uu-mark-buffer)
	(("g" . "Uu Unmark Region") . gnus-uu-unmark-region)
	(("i" . "Uu Invert Processable") . gnus-uu-invert-processable)
	(("k" . "Kill Process Mark") . gnus-summary-kill-process-mark)
	(("p" . "Mark As Processable") . gnus-summary-mark-as-processable)
	(("r" . "Uu Mark Region") . gnus-uu-mark-region)
	(("s" . "Uu Mark Series") . gnus-uu-mark-series)
	(("t" . "Uu Mark Thread") . gnus-uu-mark-thread)
	(("u" . "Unmark As Processable") . gnus-summary-unmark-as-processable)
	(("v" . "Uu Mark Over") . gnus-uu-mark-over)
	(("w" . "Save Process Mark") . gnus-summary-save-process-mark)
	(("y" . "Yank Process Mark") . gnus-summary-yank-process-mark)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary-mark)
	))

(defun one-key-menu-gnus-uu-mark ()
  "The `one-key' menu for gnus-uu-mark"
  (interactive)
  (one-key-menu "gnus-uu-mark" one-key-menu-gnus-uu-mark-alist))

(defvar one-key-menu-gnus-summary-save-alist nil
  "The `one-key' menu alist for gnus-summary-save.")

(setq one-key-menu-gnus-summary-save-alist
      '(
	(("B" . "Write Article Body File") . gnus-summary-write-article-body-file)
	(("F" . "Write Article File") . gnus-summary-write-article-file)
	(("P" . "Muttprint") . gnus-summary-muttprint)
	(("b" . "Save Article Body File") . gnus-summary-save-article-body-file)
	(("f" . "Save Article File") . gnus-summary-save-article-file)
	(("h" . "Save Article Folder") . gnus-summary-save-article-folder)
	(("m" . "Save Article Mail") . gnus-summary-save-article-mail)
	(("o" . "Save Article") . gnus-summary-save-article)
	(("p" . "Pipe Output") . gnus-summary-pipe-output)
	(("r" . "Save Article Rmail") . gnus-summary-save-article-rmail)
	(("s" . "Gnus Soup Add Article") . gnus-soup-add-article)
	(("v" . "Save Article Vm") . gnus-summary-save-article-vm)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-save ()
  "The `one-key' menu for gnus-summary-save"
  (interactive)
  (one-key-menu "gnus-summary-save" one-key-menu-gnus-summary-save-alist))

(defvar one-key-menu-gnus-summary-send-alist nil
  "The `one-key' menu alist for gnus-summary-send.")

(setq one-key-menu-gnus-summary-send-alist
      '(
	(("D" . "Gnus Send Bounce Commands") . one-key-menu-gnus-send-bounce)
	(("B" . "Reply Broken Commands") . one-key-menu-gnus-summary-reply-broken)
	(("F" . "Followup With Original") . gnus-summary-followup-with-original)
	(("N" . "Followup To Mail With Original")
. gnus-summary-followup-to-mail-with-original)
	(("O" . "UUdigest Commands") . one-key-menu-gnus-uu-digest)
	(("R" . "Reply With Original") . gnus-summary-reply-with-original)
	(("V" . "Very Wide Reply With Original")
. gnus-summary-very-wide-reply-with-original)
	(("W" . "Wide Reply With Original") . gnus-summary-wide-reply-with-original)
	(("c" . "Cancel Article") . gnus-summary-cancel-article)
	(("f" . "Followup") . gnus-summary-followup)
	(("i" . "News Other Window") . gnus-summary-news-other-window)
	(("m" . "Mail Other Window") . gnus-summary-mail-other-window)
	(("n" . "Followup To Mail") . gnus-summary-followup-to-mail)
	(("p" . "Post News") . gnus-summary-post-news)
	(("r" . "Reply") . gnus-summary-reply)
	(("s" . "Supersede Article") . gnus-summary-supersede-article)
	(("u" . "Uu Post News") . gnus-uu-post-news)
	(("v" . "Very Wide Reply") . gnus-summary-very-wide-reply)
	(("w" . "Wide Reply") . gnus-summary-wide-reply)
	(("y" . "Yank Message") . gnus-summary-yank-message)
	(("M-c" . "Mail Crosspost Complaint") . gnus-summary-mail-crosspost-complaint)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-send ()
  "The `one-key' menu for gnus-summary-send"
  (interactive)
  (one-key-menu "gnus-summary-send" one-key-menu-gnus-summary-send-alist))

(defvar one-key-menu-gnus-send-bounce-alist nil
  "The `one-key' menu alist for gnus-send-bounce.")

(setq one-key-menu-gnus-send-bounce-alist
      '(
	(("b" . "Resend Bounced Mail") . gnus-summary-resend-bounced-mail)
	(("e" . "Resend Message Edit") . gnus-summary-resend-message-edit)
	(("r" . "Resend Message") . gnus-summary-resend-message)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-summary-send)
	))

(defun one-key-menu-gnus-send-bounce ()
  "The `one-key' menu for gnus-send-bounce"
  (interactive)
  (one-key-menu "gnus-send-bounce" one-key-menu-gnus-send-bounce-alist))

(defvar one-key-menu-gnus-uu-digest-alist nil
  "The `one-key' menu alist for gnus-uu-digest.")

(setq one-key-menu-gnus-uu-digest-alist
      '(
	(("M" . "Uu Digest Mail Forward") . gnus-uu-digest-mail-forward)
	(("P" . "Uu Digest Post Forward") . gnus-uu-digest-post-forward)
	(("m" . "Mail Forward") . gnus-summary-mail-forward)
	(("p" . "Post Forward") . gnus-summary-post-forward)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-summary-send)
	))

(defun one-key-menu-gnus-uu-digest ()
  "The `one-key' menu for gnus-uu-digest"
  (interactive)
  (one-key-menu "gnus-uu-digest" one-key-menu-gnus-uu-digest-alist))

(defvar one-key-menu-gnus-summary-reply-broken-alist nil
  "The `one-key' menu alist for gnus-summary-reply-broken.")

(setq one-key-menu-gnus-summary-reply-broken-alist
      '(
	(("R" . "Reply Broken Reply To With Original")
. gnus-summary-reply-broken-reply-to-with-original)
	(("r" . "Reply Broken Reply To") . gnus-summary-reply-broken-reply-to)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-summary-send)
	))

(defun one-key-menu-gnus-summary-reply-broken ()
  "The `one-key' menu for gnus-summary-reply-broken"
  (interactive)
  (one-key-menu "gnus-summary-reply-broken" one-key-menu-gnus-summary-reply-broken-alist))

(defvar one-key-menu-gnus-summary-thread-alist nil
  "The `one-key' menu alist for gnus-summary-thread.")

(setq one-key-menu-gnus-summary-thread-alist
      '(
	(("#" . "Uu Mark Thread") . gnus-uu-mark-thread)
	(("E" . "Expire Thread") . gnus-summary-expire-thread)
	(("H" . "Hide All Threads") . gnus-summary-hide-all-threads)
	(("S" . "Show All Threads") . gnus-summary-show-all-threads)
	(("T" . "Toggle Threads") . gnus-summary-toggle-threads)
	(("^" . "Reparent Thread") . gnus-summary-reparent-thread)
	(("d" . "Down Thread") . gnus-summary-down-thread)
	(("h" . "Hide Thread") . gnus-summary-hide-thread)
	(("i" . "Raise Thread") . gnus-summary-raise-thread)
	(("k" . "Kill Thread") . gnus-summary-kill-thread)
	(("l" . "Lower Thread") . gnus-summary-lower-thread)
	(("n" . "Next Thread") . gnus-summary-next-thread)
	(("o" . "Top Thread") . gnus-summary-top-thread)
	(("p" . "Prev Thread") . gnus-summary-prev-thread)
	(("s" . "Show Thread") . gnus-summary-show-thread)
	(("t" . "Rethread Current") . gnus-summary-rethread-current)
	(("u" . "Up Thread") . gnus-summary-up-thread)
	(("M-#" . "Uu Unmark Thread") . gnus-uu-unmark-thread)
	(("M-^" . "Reparent Children") . gnus-summary-reparent-children)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-thread ()
  "The `one-key' menu for gnus-summary-thread"
  (interactive)
  (one-key-menu "gnus-summary-thread" one-key-menu-gnus-summary-thread-alist))

(defvar one-key-menu-gnus-summary-score-alist nil
  "The `one-key' menu alist for gnus-summary-score.")

(setq one-key-menu-gnus-summary-score-alist
      '(
	(("C" . "Gnus Score Customize") . gnus-score-customize)
	(("F" . "Gnus Score Flush Cache") . gnus-score-flush-cache)
	(("R" . "Rescore") . gnus-summary-rescore)
	(("S" . "Current Score") . gnus-summary-current-score)
	(("c" . "Gnus Score Change Score File") . gnus-score-change-score-file)
	(("e" . "Gnus Score Edit Current Scores") . gnus-score-edit-current-scores)
	(("f" . "Gnus Score Edit File") . gnus-score-edit-file)
	(("m" . "Gnus Score Set Mark Below") . gnus-score-set-mark-below)
	(("s" . "Set Score") . gnus-summary-set-score)
	(("t" . "Gnus Score Find Trace") . gnus-score-find-trace)
	(("w" . "Gnus Score Find Favourite Words") . gnus-score-find-favourite-words)
	(("x" . "Gnus Score Set Expunge Below") . gnus-score-set-expunge-below)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-score ()
  "The `one-key' menu for gnus-summary-score"
  (interactive)
  (one-key-menu "gnus-summary-score" one-key-menu-gnus-summary-score-alist))

(defvar one-key-menu-gnus-summary-wash-alist nil
  "The `one-key' menu alist for gnus-summary-wash.")

(setq one-key-menu-gnus-summary-wash-alist
      '(
	(("D" . "Wash Display Commands") . one-key-menu-gnus-summary-wash-display)
	(("E" . "Wash Empty Commands") . one-key-menu-gnus-summary-wash-empty)
	(("G" . "Wash Header Commands") . one-key-menu-gnus-summary-wash-header)
	(("H" . "Wash Highlight Commands") . one-key-menu-gnus-summary-wash-highlight)
	(("M" . "Wash Mime Commands") . one-key-menu-gnus-summary-wash-mime)
	(("T" . "Wash Time Commands") . one-key-menu-gnus-summary-wash-time)
	(("W" . "Wash Hide Commands") . one-key-menu-gnus-summary-wash-hide)
	(("Y" . "Wash Deuglify Commands") . one-key-menu-gnus-summary-wash-deuglify)
	(("6" . "De Base64 Unreadable") . gnus-article-de-base64-unreadable)
	(("A" . "Treat Ansi Sequences") . gnus-article-treat-ansi-sequences)
	(("B" . "Add Buttons To Head") . gnus-article-add-buttons-to-head)
	(("C" . "Capitalize Sentences") . gnus-article-capitalize-sentences)
	(("L" . "Toggle Truncate Lines") . gnus-article-toggle-truncate-lines)
	(("Q" . "Fill Long Lines") . gnus-article-fill-long-lines)
	(("Z" . "Decode Hz") . gnus-article-decode-HZ)
	(("a" . "Strip Headers In Body") . gnus-article-strip-headers-in-body)
	(("b" . "Add Buttons") . gnus-article-add-buttons)
	(("c" . "Remove Cr") . gnus-article-remove-cr)
	(("d" . "Treat Dumbquotes") . gnus-article-treat-dumbquotes)
	(("e" . "Emphasize") . gnus-article-emphasize)
	(("f" . "Display X Face") . gnus-article-display-x-face)
	(("g" . "Gnus Treat Smiley") . gnus-treat-smiley)
	(("h" . "Wash Html") . gnus-article-wash-html)
	(("i" . "Idna Message") . gnus-summary-idna-message)
	(("l" . "Stop Page Breaking") . gnus-summary-stop-page-breaking)
	(("m" . "Morse Message") . gnus-summary-morse-message)
	(("o" . "Treat Overstrike") . gnus-article-treat-overstrike)
	(("p" . "Verify X Pgp Sig") . gnus-article-verify-x-pgp-sig)
	(("C-q" . "De Quoted Unreadable") . gnus-article-de-quoted-unreadable)
	(("r" . "Caesar Message") . gnus-summary-caesar-message)
	(("s" . "Force Verify And Decrypt") . gnus-summary-force-verify-and-decrypt)
	(("t" . "Toggle Header") . gnus-summary-toggle-header)
	(("u" . "Unsplit Urls") . gnus-article-unsplit-urls)
	(("v" . "Verbose Headers") . gnus-summary-verbose-headers)
	(("w" . "Fill Cited Article") . gnus-article-fill-cited-article)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-wash ()
  "The `one-key' menu for gnus-summary-wash"
  (interactive)
  (one-key-menu "gnus-summary-wash" one-key-menu-gnus-summary-wash-alist))

(defvar one-key-menu-gnus-summary-wash-empty-alist nil
  "The `one-key' menu alist for gnus-summary-wash-empty.")

(setq one-key-menu-gnus-summary-wash-empty-alist
      '(
	(("A" . "Strip All Blank Lines") . gnus-article-strip-all-blank-lines)
	(("a" . "Strip Blank Lines") . gnus-article-strip-blank-lines)
	(("e" . "Strip Trailing Space") . gnus-article-strip-trailing-space)
	(("l" . "Strip Leading Blank Lines") . gnus-article-strip-leading-blank-lines)
	(("m" . "Strip Multiple Blank Lines") . gnus-article-strip-multiple-blank-lines)
	(("s" . "Strip Leading Space") . gnus-article-strip-leading-space)
	(("t" . "Remove Trailing Blank Lines") . gnus-article-remove-trailing-blank-lines)
	(("w" . "Remove Leading Whitespace") . gnus-article-remove-leading-whitespace)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-summary-wash)
	))

(defun one-key-menu-gnus-summary-wash-empty ()
  "The `one-key' menu for gnus-summary-wash-empty"
  (interactive)
  (one-key-menu "gnus-summary-wash-empty" one-key-menu-gnus-summary-wash-empty-alist))

(defvar one-key-menu-gnus-summary-wash-time-alist nil
  "The `one-key' menu alist for gnus-summary-wash-time.")

(setq one-key-menu-gnus-summary-wash-time-alist
      '(
	(("e" . "Date Lapsed") . gnus-article-date-lapsed)
	(("i" . "Date Iso8601") . gnus-article-date-iso8601)
	(("l" . "Date Local") . gnus-article-date-local)
	(("o" . "Date Original") . gnus-article-date-original)
	(("p" . "Date English") . gnus-article-date-english)
	(("s" . "Date User") . gnus-article-date-user)
	(("u" . "Date Ut") . gnus-article-date-ut)
	(("z" . "Date Ut") . gnus-article-date-ut)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-summary-wash)
	))

(defun one-key-menu-gnus-summary-wash-time ()
  "The `one-key' menu for gnus-summary-wash-time"
  (interactive)
  (one-key-menu "gnus-summary-wash-time" one-key-menu-gnus-summary-wash-time-alist))

(defvar one-key-menu-gnus-summary-wash-mime-alist nil
  "The `one-key' menu alist for gnus-summary-wash-mime.")

(setq one-key-menu-gnus-summary-wash-mime-alist
      '(
	(("b" . "View Part") . gnus-article-view-part)
	(("c" . "Decode Charset") . gnus-article-decode-charset)
	(("v" . "Gnus Mime View All Parts") . gnus-mime-view-all-parts)
	(("w" . "Decode Mime Words") . gnus-article-decode-mime-words)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-summary-wash)
	))

(defun one-key-menu-gnus-summary-wash-mime ()
  "The `one-key' menu for gnus-summary-wash-mime"
  (interactive)
  (one-key-menu "gnus-summary-wash-mime" one-key-menu-gnus-summary-wash-mime-alist))

(defvar one-key-menu-gnus-summary-wash-display-alist nil
  "The `one-key' menu alist for gnus-summary-wash-display.")

(setq one-key-menu-gnus-summary-wash-display-alist
      '(
	(("D" . "Remove Images") . gnus-article-remove-images)
	(("d" . "Display Face") . gnus-article-display-face)
	(("f" . "Gnus Treat From Picon") . gnus-treat-from-picon)
	(("m" . "Gnus Treat Mail Picon") . gnus-treat-mail-picon)
	(("n" . "Gnus Treat Newsgroups Picon") . gnus-treat-newsgroups-picon)
	(("s" . "Gnus Treat Smiley") . gnus-treat-smiley)
	(("x" . "Display X Face") . gnus-article-display-x-face)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-summary-wash)
	))

(defun one-key-menu-gnus-summary-wash-display ()
  "The `one-key' menu for gnus-summary-wash-display"
  (interactive)
  (one-key-menu "gnus-summary-wash-display" one-key-menu-gnus-summary-wash-display-alist))

(defvar one-key-menu-gnus-summary-wash-header-alist nil
  "The `one-key' menu alist for gnus-summary-wash-header.")

(setq one-key-menu-gnus-summary-wash-header-alist
      '(
	(("f" . "Treat Fold Headers") . gnus-article-treat-fold-headers)
	(("n" . "Treat Fold Newsgroups") . gnus-article-treat-fold-newsgroups)
	(("u" . "Treat Unfold Headers") . gnus-article-treat-unfold-headers)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-summary-wash)
	))

(defun one-key-menu-gnus-summary-wash-header ()
  "The `one-key' menu for gnus-summary-wash-header"
  (interactive)
  (one-key-menu "gnus-summary-wash-header" one-key-menu-gnus-summary-wash-header-alist))

(defvar one-key-menu-gnus-summary-wash-highlight-alist nil
  "The `one-key' menu alist for gnus-summary-wash-highlight.")

(setq one-key-menu-gnus-summary-wash-highlight-alist
      '(
	(("a" . "Highlight") . gnus-article-highlight)
	(("c" . "Highlight Citation") . gnus-article-highlight-citation)
	(("h" . "Highlight Headers") . gnus-article-highlight-headers)
	(("s" . "Highlight Signature") . gnus-article-highlight-signature)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-summary-wash)
	))

(defun one-key-menu-gnus-summary-wash-highlight ()
  "The `one-key' menu for gnus-summary-wash-highlight"
  (interactive)
  (one-key-menu "gnus-summary-wash-highlight"
one-key-menu-gnus-summary-wash-highlight-alist))

(defvar one-key-menu-gnus-summary-wash-hide-alist nil
  "The `one-key' menu alist for gnus-summary-wash-hide.")

(setq one-key-menu-gnus-summary-wash-hide-alist
      '(
	(("C-c" . "Hide Citation Maybe") . gnus-article-hide-citation-maybe)
	(("B" . "Strip Banner") . gnus-article-strip-banner)
	(("C" . "Hide Citation In Followups") . gnus-article-hide-citation-in-followups)
	(("P" . "Hide Pem") . gnus-article-hide-pem)
	(("a" . "Hide") . gnus-article-hide)
	(("b" . "Hide Boring Headers") . gnus-article-hide-boring-headers)
	(("c" . "Hide Citation") . gnus-article-hide-citation)
	(("h" . "Hide Headers") . gnus-article-hide-headers)
	(("l" . "Hide List Identifiers") . gnus-article-hide-list-identifiers)
	(("s" . "Hide Signature") . gnus-article-hide-signature)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-summary-wash)
	))

(defun one-key-menu-gnus-summary-wash-hide ()
  "The `one-key' menu for gnus-summary-wash-hide"
  (interactive)
  (one-key-menu "gnus-summary-wash-hide" one-key-menu-gnus-summary-wash-hide-alist))

(defvar one-key-menu-gnus-summary-wash-deuglify-alist nil
  "The `one-key' menu alist for gnus-summary-wash-deuglify.")

(setq one-key-menu-gnus-summary-wash-deuglify-alist
      '(
	(("a" . "Outlook Repair Attribution") . gnus-article-outlook-repair-attribution)
	(("c" . "Outlook Rearrange Citation") . gnus-article-outlook-rearrange-citation)
	(("f" . "Outlook Deuglify Article") . gnus-article-outlook-deuglify-article)
	(("u" . "Outlook Unwrap Lines") . gnus-article-outlook-unwrap-lines)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-summary-wash)
	))

(defun one-key-menu-gnus-summary-wash-deuglify ()
  "The `one-key' menu for gnus-summary-wash-deuglify"
  (interactive)
  (one-key-menu "gnus-summary-wash-deuglify"
one-key-menu-gnus-summary-wash-deuglify-alist))

(defvar one-key-menu-gnus-summary-uu-extract-alist nil
  "The `one-key' menu alist for gnus-summary-uu-extract.")

(setq one-key-menu-gnus-summary-uu-extract-alist
      '(
	(("v" . "Uu Extract View Commands") . one-key-menu-gnus-uu-extract-view)
	(("B" . "Uu Decode Binhex") . gnus-uu-decode-binhex)
	(("O" . "Uu Decode Save") . gnus-uu-decode-save)
	(("P" . "Uu Decode Postscript And Save") . gnus-uu-decode-postscript-and-save)
	(("S" . "Uu Decode Unshar And Save") . gnus-uu-decode-unshar-and-save)
	(("U" . "Uu Decode Uu And Save") . gnus-uu-decode-uu-and-save)
	(("Y" . "Uu Decode Yenc") . gnus-uu-decode-yenc)
	(("b" . "Uu Decode Binhex") . gnus-uu-decode-binhex)
	(("m" . "Save Parts") . gnus-summary-save-parts)
	(("o" . "Uu Decode Save") . gnus-uu-decode-save)
	(("p" . "Uu Decode Postscript") . gnus-uu-decode-postscript)
	(("s" . "Uu Decode Unshar") . gnus-uu-decode-unshar)
	(("u" . "Uu Decode Uu") . gnus-uu-decode-uu)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-uu-extract ()
  "The `one-key' menu for gnus-summary-uu-extract"
  (interactive)
  (one-key-menu "gnus-summary-uu-extract" one-key-menu-gnus-summary-uu-extract-alist))

(defvar one-key-menu-gnus-summary-uu-view-alist nil
  "The `one-key' menu alist for gnus-summary-uu-view.")

(setq one-key-menu-gnus-summary-uu-view-alist
      '(
	(("B" . "Binhex View") . gnus-uu-decode-binhex-view)
	(("O" . "Save View") . gnus-uu-decode-save-view)
	(("P" . "Postscript And Save View") . gnus-uu-decode-postscript-and-save-view)
	(("S" . "Unshar And Save View") . gnus-uu-decode-unshar-and-save-view)
	(("U" . "Uu And Save View") . gnus-uu-decode-uu-and-save-view)
	(("b" . "Binhex View") . gnus-uu-decode-binhex-view)
	(("o" . "Save View") . gnus-uu-decode-save-view)
	(("p" . "Postscript View") . gnus-uu-decode-postscript-view)
	(("s" . "Unshar View") . gnus-uu-decode-unshar-view)
	(("u" . "Uu View") . gnus-uu-decode-uu-view)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-group-summary-uu-extract)
	))

(defun one-key-menu-gnus-summary-uu-view ()
  "The `one-key' menu for gnus-summary-uu-view"
  (interactive)
  (one-key-menu "gnus-summary-uu-view" one-key-menu-gnus-summary-uu-view-alist))

(defvar one-key-menu-gnus-summary-buffer-alist nil
  "The `one-key' menu alist for gnus-summary-buffer.")

(setq one-key-menu-gnus-summary-buffer-alist
      '(
	(("c" . "Insert Cached Articles") . gnus-summary-insert-cached-articles)
	(("d" . "Insert Dormant Articles") . gnus-summary-insert-dormant-articles)
	(("g" . "Prepare") . gnus-summary-prepare)
	(("t" . "Insert Ticked Articles") . gnus-summary-insert-ticked-articles)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-buffer ()
  "The `one-key' menu for gnus-summary-buffer"
  (interactive)
  (one-key-menu "gnus-summary-buffer" one-key-menu-gnus-summary-buffer-alist))

(defvar one-key-menu-gnus-summary-exit-alist nil
  "The `one-key' menu alist for gnus-summary-exit.")

(setq one-key-menu-gnus-summary-exit-alist
      '(
	(("C" . "Catchup All And Exit") . gnus-summary-catchup-all-and-exit)
	(("E" . "Exit No Update") . gnus-summary-exit-no-update)
	(("G" . "Rescan Group") . gnus-summary-rescan-group)
	(("N" . "Next Group") . gnus-summary-next-group)
	(("P" . "Prev Group") . gnus-summary-prev-group)
	(("Q" . "Exit") . gnus-summary-exit)
	(("R" . "Reselect Current Group") . gnus-summary-reselect-current-group)
	(("c" . "Catchup And Exit") . gnus-summary-catchup-and-exit)
	(("n" . "Catchup And Goto Next Group") . gnus-summary-catchup-and-goto-next-group)
	(("p" . "Catchup And Goto Prev Group") . gnus-summary-catchup-and-goto-prev-group)
	(("s" . "Save Newsrc") . gnus-summary-save-newsrc)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-summary)
	))

(defun one-key-menu-gnus-summary-exit ()
  "The `one-key' menu for gnus-summary-exit"
  (interactive)
  (one-key-menu "gnus-summary-exit" one-key-menu-gnus-summary-exit-alist))

(defvar one-key-menu-gnus-article-alist nil
  "The `one-key' menu alist for gnus-article.")

(setq one-key-menu-gnus-article-alist
      '(
  	(("C-c" . "Other Commands") . one-key-menu-gnus-article-other)
	(("S" . "Commands From Summary Mode") . one-key-menu-gnus-summary-article)
	(("<tab>" . "Widget Forward") . widget-forward)
	(("<escape> <tab>" . "Widget Backward") . widget-backward)
	(("<return>" . "Widget Button Press") . widget-button-press)
	(("SPC" . "Goto Next Page") . gnus-article-goto-next-page)
	(("<delete>" . "Goto Prev Page") . gnus-article-goto-prev-page)
	(("<" . "Beginning Of Buffer") . beginning-of-buffer)
	((">" . "End Of Buffer") . end-of-buffer)
	(("d" . "Describe Briefly") . gnus-article-describe-briefly)
	(("r" . "Reply") . gnus-article-reply)
	(("F" . "Followup With Original") . gnus-article-followup-with-original)
	(("R" . "Reply With Original") . gnus-article-reply-with-original)
	(("W" . "Wide Reply With Original") . gnus-article-wide-reply-with-original)
	(("C-f" . "Mail Forward (C-c C-f)") . gnus-summary-mail-forward)
	(("C-s" . "Report 419 scam (C-c C-s)") . aleblanc/report-419-spam)
	(("e" . "Gnus Summary Edit Article") . gnus-summary-edit-article)
	(("s" . "Show Summary") . gnus-article-show-summary)
	(("g" . "Redisplay email (press C-u g to show raw)") . gnus-summary-show-article)
	(("C-d" . "Read Summary Keys") . gnus-article-read-summary-keys)
	(("t" . "Read Summary Send Keys") . gnus-article-read-summary-send-keys)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus)
	))

(defun one-key-menu-gnus-article ()
  "The `one-key' menu for gnus-article"
  (interactive)
  (one-key-menu "gnus-article" one-key-menu-gnus-article-alist))

(defvar one-key-menu-gnus-article-other-alist nil
  "The `one-key' menu alist for gnus-article-other.")

(setq one-key-menu-gnus-article-other-alist
      '(
	(("C-S-b" . "Send Bug Report") . gnus-bug)
	(("<tab>" . "Gnus Info Find Node") . gnus-info-find-node)
	(("<return>" . "Mail Address Near Point") . gnus-article-mail)
	(("^" . "Refer Article") . gnus-article-refer-article)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus-article)
	))

(defun one-key-menu-gnus-article-other ()
  "The `one-key' menu for gnus-article-other"
  (interactive)
  (one-key-menu "gnus-article-other" one-key-menu-gnus-article-other-alist))

(defvar one-key-menu-gnus-server-alist nil
  "The `one-key' menu alist for gnus-server.")

(setq one-key-menu-gnus-server-alist
      '(
	(("b" . "Browse server commands") . one-key-menu-gnus-browse)
	(("<return>" . "Read Server") . gnus-server-read-server)
	(("SPC" . "Read Server In Server Buffer")
. gnus-server-read-server-in-server-buffer)
	(("C" . "Close Server") . gnus-server-close-server)
	(("D" . "Deny Server") . gnus-server-deny-server)
	(("L" . "Offline Server") . gnus-server-offline-server)
	(("O" . "Open Server") . gnus-server-open-server)
	(("R" . "Remove Denials") . gnus-server-remove-denials)
	(("a" . "Add Server") . gnus-server-add-server)
	(("c" . "Copy Server") . gnus-server-copy-server)
	(("e" . "Edit Server") . gnus-server-edit-server)
	(("g" . "Regenerate Server") . gnus-server-regenerate-server)
	(("k" . "Kill Server") . gnus-server-kill-server)
	(("l" . "List Servers") . gnus-server-list-servers)
	(("Q" . "Exit") . gnus-server-exit)
	(("s" . "Scan Server") . gnus-server-scan-server)
	(("y" . "Yank Server") . gnus-server-yank-server)
	(("z" . "Compact Server") . gnus-server-compact-server)
	(("C-S-b" . "Send Bug Report") . gnus-bug)
	(("I" . "Gnus Info Find Node") . gnus-info-find-node)
	(("M-c" . "Close All Servers") . gnus-server-close-all-servers)
	(("M-o" . "Open All Servers") . gnus-server-open-all-servers)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus)
	))

(defun one-key-menu-gnus-server ()
  "The `one-key' menu for gnus-server"
  (interactive)
  (one-key-menu "gnus-server" one-key-menu-gnus-server-alist))

(defvar one-key-menu-gnus-browse-alist nil
  "The `one-key' menu alist for gnus browse server commands.")

(setq one-key-menu-gnus-browse-alist
      '(
	(("RET"	. "gnus-browse-select-group (RET)") . gnus-browse-select-group)
	(("SPC"	. "gnus-browse-read-group (SPC)") . gnus-browse-read-group)
	(("D" . "gnus-browse-describe-briefly (?)") . gnus-browse-describe-briefly)
	(("d" . "gnus-browse-describe-group (d)") . gnus-browse-describe-group)
	(("Q" . "gnus-browse-exit (q)") . gnus-browse-exit)
	(("u" . "gnus-browse-unsubscribe-current-group (u)")
. gnus-browse-unsubscribe-current-group)
	(("o" . "find occurences of word") . occur)
	(("C-S-b" . "gnus-bug (C-c C-b)") . gnus-bug)
	(("I" . "gnus-info-find-node (C-c TAB)") . gnus-info-find-node)
	(("C-b" . "Back to previous menu") . one-key-menu-gnus)	
	))

(defun one-key-menu-gnus-browse ()
  "The `one-key' menu for gnus browse server commands"
  (interactive)
  (one-key-menu "gnus-browse" one-key-menu-gnus-browse-alist))

(provide 'ok-gnus)
;;; ok-gnus.el ends here
