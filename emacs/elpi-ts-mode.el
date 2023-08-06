;;; elpi-ts-mode.el --- Major mode for the ELPI dialect of λProlog -*- lexical-binding:t -*-

;; Copyright (c) 2023 Dominic Verity and The Australian National University

;; Original Author: Dominic Verity (dominic.verity@anu.edu.au)
;; License:         GPL v3
;; Funding:         Partly funded under US Army ITC-IPAC R&D Project
;;                  contract FA520923C0004.
;; Project title:   "Towards a synthetic theory of Extended TQFTs"
;; Created:         August 2023
;; Keywords:        ELPI, lambda prolog, tree-sitter

;; Some parts of this code is adapted from the prolog.el major mode:

;; Authors:         Emil Åström <emil_astrom(at)hotmail(dot)com>
;;                  Milan Zamazal <pdm(at)freesoft(dot)cz>
;;                  Stefan Bruda <stefan(at)bruda(dot)ca>

;; which is distributed under GPL v3 and is

;; Copyright (C) 1986-1987, 1997-1999, 2002-2003, 2011-2023 Free
;; Software Foundation, Inc.

(defvar elpi-ts-mode-version "1.22"
  "ELPI mode version number.")

;; This file is part of tree-sitter-elpi.
;;
;; tree-sitter-elpi is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; tree-sitter-elpi is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; tree-sitter-elpi. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'treesit)
(require 'rx)

(defcustom elpi-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `elpi-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'elpi)

;; adapted from prolog.el
(defvar elpi-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Special characters occurring in identifiers
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?+ "w" table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?= "w" table)
    (modify-syntax-entry ?< "w" table)
    (modify-syntax-entry ?> "w" table)
    (modify-syntax-entry ?^ "w" table)
    (modify-syntax-entry ?? "w" table)
    (modify-syntax-entry ?@ "w" table)
    (modify-syntax-entry ?# "w" table)
    (modify-syntax-entry ?~ "w" table)
    (modify-syntax-entry ?& "w" table)
    (modify-syntax-entry ?! "w" table)
    (modify-syntax-entry ?$ "w" table)

    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)

    (modify-syntax-entry ?` "\"" table)     ;; should be "w" inside a constant
    (modify-syntax-entry ?' "\"" table)     ;; should be "w" inside a constant
    (modify-syntax-entry ?\" "\"" table)

    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\\ "." table)     ;; should be char escape in a string

    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?/ ". 14" table)
    table)
  "Syntax table for `elpi-ts-mode`.")

;;; Faces

;;; Keyword and operator lists

(defvar elpi-ts-mode--keywords
  '(kind type typeabbrev shorten accumulate import accum_sig
    use_sig local mode pred macro rule namespace constraint)
  "ELPI keyword nodes for tree-sitter font locking.")

(defvar elpi-ts-mode--disabled
  '(module_decl sig_decl exportdef_decl localkind_decl
    useonly_decl closed_decl)
  "ELPI disabled declaration nodes for tree-sitter font locking.")

(defvar elpi-ts-mode--attributes
  '(external cexternal cif cname cafter cbefore creplace cindex)
  "ELPI attribute nodes for tree-sitter font locking.")

(defvar elpi-ts-mode--operators
  '(vdash qdash or conj conj2 arrow darrow eq eq2 family_lt
    family_gt cons family_tick family_exp family_plus
    minus minusr minusi minuss family_times slash div
    mod family_minus family_btick family_eq family_and
    family_sharp family_tilde)
  "ELPI operator nodes for tree-sitter font locking.")

(defun elpi-ts-mode--get-constant-name (node)
  "Extract and return the text of a constant NODE."
  )

(defun elpi-ts-mode--fontify-bound-variable (node &rest _)
  "Search up from a constant NODE to determine whether it is bound by a lambda.
Fontify the range associated with NODE accordingly."
  )

(defvar elpi-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   ;; Overriding rules (special cases)
   :language 'elpi
   :override t
   :feature 'comment
   '([(line_comment) (block_comment) (skip_comment)]
     @font-lock-comment-face)
   :language 'elpi
   :override t
   :feature 'escape
   '([(escape_sequence) (quote_escape)] @font-lock-escape-face)
   :language 'elpi
   :override t
   :feature 'lambda-binding
   '((abs_term left: (constant (_) @font-lock-variable-name-face))
     (multi_bind (params (constant (_) @font-lock-variable-name-face))))
   ;; :language 'elpi
   ;; :override t
   ;; :feature 'variable-bound
   :language 'elpi
   :override t
   :feature 'constant
   '((constant (mixfix_symb) @font-lock-constant-font))
   ;; Non-overriding rules
   :language 'elpi
   :override nil
   :feature 'keyword
   `(,(cl-map 'vector #'(lambda (x) `(,x)) elpi-ts-mode--keywords)
     @font-lock-keyword-face)
   :language 'elpi
   :override nil
   :feature 'disabled
   `(,(cl-map 'vector #'(lambda (x) `(,x)) elpi-ts-mode--disabled)
     @font-lock-comment-face)
   :language 'elpi
   :override nil
   :feature 'attribute
   `(,(cl-map 'vector #'(lambda (x) `(,x)) elpi-ts-mode--attributes)
     @font-lock-attribute-face)
   :language 'elpi
   :override nil
   :feature 'operator
   `(,(cl-map 'vector #'(lambda (x) `(,x)) elpi-ts-mode--operators)
     @font-lock-operator-face)
   :language 'elpi
   :override nil
   :feature 'punctuation-bracket
   `([(lparen) (rparen) (lcurly) (rcurly) (lbracket) (rbracket)]
     @font-lock-bracket-face)
   :language 'elpi
   :override nil
   :feature 'number
   '([(integer) (float)] @font-lock-number-face)
   :language 'elpi
   :override nil
   :feature 'string-delimiter
   '((string ("\"") @font-lock-bracket-face))
   :language 'elpi
   :override nil
   :feature 'string-content
   '((string_content) @font-lock-string-face)
   :language 'elpi
   :override nil
   :feature 'punctuation-delimiter
   '([(comma) (pipe) (bind) (iff) (full_stop)]
     @font-lock-delimiter-face)
   :language 'elpi
   :override nil
   :feature 'label
   '([(io) (io_colon)] @font-lock-property-use-face)
   :language 'elpi
   :override nil
   :feature 'operator-builtin
   '([(is) (as)] @font-lock-misc-punctuation-face)
   :language 'elpi
   :override nil
   :feature 'constant-builtin
   '([(pi) (sigma) (cut)] @font-lock-builtin-face)
   :language 'elpi
   :override nil
   :feature 'type-builtin
   '([(typeid)] @font-lock-type-face)
   ;; Variables, wildcards, constants and macros.
   ;; haven't yet worked out what to do with constants of the
   ;; form `<name>` and \'<name>\'.
   :language 'elpi
   :override nil
   :feature 'variable
   '((ucname) @font-lock-variable-use-face)
   :language 'elpi
   :override nil
   :feature 'wildcard
   '([(uname) (freshuv)] @font-lock-negation-char-face)
   :language 'elpi
   :override nil
   :feature 'constant
   '((lcname) @font-lock-constant-face)
   :language 'elpi
   :override nil
   :feature 'macro
   '((atname) @font-lock-preprocessor-face)
   )
  "Tree-sitter font-lock settings for `elpi-ts-mode'.")

;;;###autoload
(define-derived-mode elpi-ts-mode prog-mode "ELPI"
  "Major mode for editing ELPI, powered by tree-sitter."
  :group 'elpi
  :syntax-table elpi-ts-mode--syntax-table

  (unless (treesit-ready-p 'elpi)
    (error "Tree-sitter parser for ELPI isn't available"))

  (treesit-parser-create 'elpi)

  ;; Font lock
  (setq-local treesit-font-lock-settings elpi-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment number string-content string-delimiter escape
                 punctuation-delimiter)
                (punctuation-bracket keyword disabled operator attribute
                 operator-builtin label constant-builtin type-builtin)
                (variable wildcard constant macro lambda-binding)))

  (treesit-major-mode-setup))

(if (treesit-ready-p 'elpi)
    (progn
      (add-to-list 'auto-mode-alist '("\\.elpi\\'" . elpi-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.mod\\'" . elpi-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.sig\\'" . elpi-ts-mode))))

(provide 'elpi-ts-mode)
;;; elpi-ts-mode.el ends here
