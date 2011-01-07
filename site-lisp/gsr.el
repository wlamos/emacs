;;; gsr.el --- Major mode for GSR files

;; Copyright (C) 2011  Wenliang Zhang

;; Author: Wenliang Zhang 
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

(defvar gsr-keywords
  '("TECH_FILE" "LEF_FILES" "DEF_FILES"
    "LIB_FILES" "PAD_FILES" "BLOCK_STA_FILE" "CELL_RC_FILE"
    "APL_FILES" "VDD_NETS" "GND_NETS" "VCD_FILE" "STATE_PROPAGATION"
    "BLOCK_POWER_FOR_SCALING_FILE"
    "INSTANCE_TOGGLE_RATE_FILE"
    "DECAP_CELL"))

(defvar gsr-variables
  '("FREQ" "TOGGLE_RATE" "VDD" "VSS" "INPUT_TRANSITION"
    "FILE_TYPE" "FRONT_PATH" "SUBSTITUTE_PATH" "FRAME_SIZE" "TRUE_TIME" "MAPPING"
    "PROPAGATION" "FREQ_OF_MISSING_INSTANCES"
    "IGNORE_LEF_DEF_MISMATCH"
    "IGNORE_DEF_ERROR"
    "IGNORE_PGARC_ERROR"
    "IGNORE_GDSMEM_ERROR"
    "CACHE_MODE"
    "CACHE_DIR"
    "STA_ONE_PASS"
    "POWER_SLEW"
    "LIB2AVM"
    "EM_REPORT_PERCENTAGE"
    "EM_REPORT_LINE_NUMBER"
    "AM_MODE"
    "DYNAMIC_PRESIM_TIME"
    "DYNAMIC_SIMULATION_TIME"
    "DYNAMIC_TIME_STEP"
    "DYNAMIC_EM"
    "IGNORE_APL_PROCESS_CORNER"
    "REPORT_REDUCTION"
    "POWER_MODE"
    "APACHE_FILES"
    "GSC_OVERRIDE_IPF"
    "IGNORE_APL_CHECK"
    "FAO_DRC"))

(defvar gsr-constants
  '("RTL_FSDB" "RTL_VCD"
    "ACTIVITY" "APL" "LIB"))

;; create the regex string for each class of keywords
(defvar gsr-keywords-regexp (regexp-opt gsr-keywords 'words))
(defvar gsr-constants-regexp (regexp-opt gsr-constants 'words))
(defvar gsr-variables-regexp (regexp-opt gsr-variables 'words))

(setq gsr-keywords nil
      gsr-variables nil
      gsr-constants nil)

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq gsr-font-lock-keywords
  `(
    (,gsr-keywords-regexp . font-lock-keyword-face)    
    (,gsr-constants-regexp . font-lock-constant-face)
    (,gsr-variables-regexp . font-lock-variable-name-face)    
    ;; note: order above matters. “gsr-keywords-regexp” goes first because
    ;; otherwise the keyword “VDD” 
    ;; would be highlighted.
))

(define-derived-mode gsr-mode fundamental-mode
  "GSR"
  "Major mode for editing GSR (Global System Requiement) files."
  (setq mode-name "gsr")
  
  ;; code for syntax highlighting
  (setq font-lock-defaults '((gsr-font-lock-keywords)))
  (setq gsr-keywords-regexp nil
	gsr-variables-regexp nil
	gsr-constants-regexp nil)
  ;; modify the keymap
  ;; (define-key gsr-mode-map )
  
  (modify-syntax-entry ?# "<")
  (modify-syntax-entry ?\n ">"))

(setq auto-mode-alist
      (append '(("\\.gsr$" . gsr-mode))
	      auto-mode-alist))

(provide 'gsr)
;;; gsr.el ends here
