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

(defcustom elpi-ts-mode-indent-offset 2
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
    (modify-syntax-entry ?` "w" table)
    (modify-syntax-entry ?' "w" table)

    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)

    (modify-syntax-entry ?\" "\"" table)

    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\\ "." table)     ;; should be char escape in a string

    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?/ ". 14" table)
    table)
  "Syntax table for `elpi-ts-mode`.")

;;; Customize group
(defgroup elpi '() "ELPI an implementation of λProlog."
  :group 'languages)

;;; Faces
(defgroup elpi-faces '() "Faces for font lock colouring of ELPI code."
  :group 'elpi)

(defface elpi-comment-face
  '((t . (:inherit font-lock-comment-face)))
  "Face for comments in ELPI mode.")
(defface elpi-disabled-face
  '((t . (:inherit font-lock-comment-face)))
  "Face for disabled Teyjus declarations in ELPI mode.")
(defface elpi-escape-face
  '((t . (:inherit font-lock-escape-face)))
  "Face for escape sequences in ELPI mode.")
(defface elpi-applied-occurrence-face
  '((t . (:inherit font-lock-variable-name-face)))
  "Face for applied occurrences of parameters in ELPI mode.")
(defface elpi-defining-occurrence-face
  '((t . (:inherit font-lock-variable-name-face
          :underline "firebrick")))
  "Face for defining occurrences of parameters in ELPI mode.")
(defface elpi-constant-face
  '((t . (:inherit font-lock-constant-face)))
  "Face for constant identifiers in ELPI mode.")
(defface elpi-clause-head-face
  '((t . (:inherit font-lock-constant-face
          :underline "firebrick")))
  "Face for head identifiers of clauses in ELPI mode.")
(defface elpi-keyword-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face for keywords in ELPI mode.")
(defface elpi-attributes-face
  '((t . (:inherit font-lock-attribute-face)))
  "Face for attributes in ELPI mode.")
(defface elpi-operator-face
  '((t . (:inherit font-lock-operator-face)))
  "Face for operators in ELPI mode.")
(defface elpi-punctuation-bracket-face
  '((t . (:inherit font-lock-bracket-face)))
  "Face for brackets in ELPI mode.")
(defface elpi-number-face
  '((t . (:inherit font-lock-number-face)))
  "Face for literal numbers in ELPI mode.")
(defface elpi-string-delimiter-face
  '((t . (:inherit font-lock-bracket-face)))
  "Face for string delimiters in ELPI mode.")
(defface elpi-string-content-face
  '((t . (:inherit font-lock-string-face)))
  "Face for string content in ELPI mode.")
(defface elpi-punctuation-delimiter-face
  '((t . (:inherit font-lock-delimiter-face)))
  "Face for delimiters in ELPI mode.")
(defface elpi-mode-label-face
  '((t . (:inherit font-lock-property-use-face)))
  "Face for IO mode labels in ELPI mode.")
(defface elpi-operator-builtin-face
  '((t . (:inherit font-lock-misc-punctuation-face)))
  "Face for built in operators in ELPI mode.")
(defface elpi-constant-builtin-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for built in constants in ELPI mode.")
(defface elpi-kind-builtin-face
  '((t . (:inherit font-lock-type-face)))
  "Face for built in kinds in ELPI mode.")
(defface elpi-type-face
  '((t . (:inherit font-lock-type-face)))
  "Face for types in ELPI mode.")
(defface elpi-variable-face
  '((t . (:inherit font-lock-variable-use-face)))
  "Face for logic (instantiable) variables in ELPI mode.")
(defface elpi-wildcard-face
  '((t . (:inherit font-lock-negation-char-face)))
  "Face for wildcards in ELPI mode.")
(defface elpi-constant-face
  '((t . (:inherit font-lock-constant-face)))
  "Face for constants in ELPI mode.")
(defface elpi-macro-face
  '((t . (:inherit font-lock-preprocessor-face)))
  "Face for macros in ELPI mode.")

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

(defvar elpi-ts-mode--brackets
  '(lparen rparen lcurly rcurly lbracket rbracket
           prog_begin prog_end)
  "ELPI bracket nodes for tree-sitter font locking."
)

;; Functions to implement more complex font-locking, such as determining
;; occurrences of bound variables so that they may be assigned a
;; distinguishing colour.

(defun elpi-ts-mode--find-binding (node &rest _)
  "Search up from a constant NODE to determine whether it is bound by a lambda.
Returns the parameter of the lambda abstraction that binds the given variable,
or nil otherwise."
  (when (equal (treesit-node-type node) "constant")
    (let ((name (treesit-node-text
                 (treesit-node-child-by-field-name
                  node "id")))
          (parent (treesit-node-parent node))
          (param nil))
      (cl-flet*
          ((has-same-name-as-node (other)
             (when (equal
                    (treesit-node-text
                     (treesit-node-child-by-field-name other "id"))
                    name)
               other))
           (find-param (ancestor)
             (cond
              ((equal (treesit-node-type ancestor) "abs_term")
               (has-same-name-as-node
                (treesit-node-child-by-field-name ancestor "left")))
              ((equal (treesit-node-type ancestor) "multi_bind")
               (seq-some
                #'has-same-name-as-node
                (reverse
                 (treesit-node-children
                  (treesit-node-child-by-field-name ancestor "left")
                  t)))))))
        (while (and parent (not (setq param (find-param parent))))
          (setq parent (treesit-node-parent parent)))
        param))))

(defun elpi-ts-mode--fontify-bound-variable (node override start end &rest _)
  "Apply face to NODE when it is a variable bound in a surrounding lambda.
OVERRIDE is the face override option of the calling font lock rule.
START and END specify the region to be fontified."
  (when (elpi-ts-mode--find-binding node)
    (treesit-fontify-with-override (treesit-node-start node)
                                   (treesit-node-end node)
                                   'elpi-applied-occurrence-face
                                   override start end)))

(defun elpi-ts-mode--fontify-clause-head (node override start end &rest _)
  "Fontify the head of a clause, by traversing its spine from NODE.
OVERRIDE is the face override option of the calling font lock rule.
START and END specify the region to be fontified."
  (while
      (let ((ntype (treesit-node-type node)))
       (cond
        ((equal ntype "constant")
         (treesit-fontify-with-override (treesit-node-start node)
                                        (treesit-node-end node)
                                        'elpi-clause-head-face
                                        override start end) nil)
        ((equal ntype "app_term")
         (setq node (treesit-node-child-by-field-name node "left")) t)
        ((equal ntype "paren_term")
         (setq node (treesit-node-child-by-field-name node "term")) t)
        (nil)))))

;; Font-locking rules

(defvar elpi-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   ;; Overriding rules (special cases)
   :language 'elpi
   :override t
   :feature 'comment
   '([(line_comment) (block_comment) (skip_comment)] @elpi-comment-face)
   :language 'elpi
   :override t
   :feature 'escape
   '([(escape_sequence) (quote_escape)] @elpi-escape-face)
   :language 'elpi
   :override t
   :feature 'lambda-parameter
   '((abs_term left: (constant (_) @elpi-defining-occurrence-face))
     (multi_bind (params (constant (_) @elpi-defining-occurrence-face)))
     (constant) @elpi-ts-mode--fontify-bound-variable)
   :language 'elpi
   :override t
   :feature 'constant
   `((constant ,(cl-map 'vector #'(lambda (x) `(,x)) elpi-ts-mode--operators)
               @elpi-constant-face))
   :language 'elpi
   :override t
   :feature 'clause-head
   '((clause_decl
      (infix_term
       left: ([(app_term) @elpi-ts-mode--fontify-clause-head
               (constant) @elpi-clause-head-face])
       op: (vdash)))
     (clause_decl[(app_term) @elpi-ts-mode--fontify-clause-head
                  (constant) @elpi-clause-head-face])
     (pred_decl (constant) @elpi-clause-head-face)
     (mode_decl (constant) @elpi-clause-head-face))

   ;; Non-overriding rules
   :language 'elpi
   :override nil
   :feature 'keyword
   `(,(cl-map 'vector #'(lambda (x) `(,x)) elpi-ts-mode--keywords)
     @elpi-keyword-face)
   :language 'elpi
   :override nil
   :feature 'disabled
   `(,(cl-map 'vector #'(lambda (x) `(,x)) elpi-ts-mode--disabled)
     @elpi-disabled-face)
   :language 'elpi
   :override nil
   :feature 'attribute
   `(,(cl-map 'vector #'(lambda (x) `(,x)) elpi-ts-mode--attributes)
     @elpi-attributes-face)
   :language 'elpi
   :override nil
   :feature 'operator
   `(,(cl-map 'vector #'(lambda (x) `(,x)) elpi-ts-mode--operators)
     @elpi-operator-face)
   :language 'elpi
   :override nil
   :feature 'punctuation-bracket
   `(,(cl-map 'vector #'(lambda (x) `(,x)) elpi-ts-mode--brackets)
     @elpi-punctuation-bracket-face)
   :language 'elpi
   :override nil
   :feature 'number
   '([(integer) (float)] @elpi-number-face)
   :language 'elpi
   :override nil
   :feature 'string-delimiter
   '((string ("\"") @elpi-string-delimiter-face))
   :language 'elpi
   :override nil
   :feature 'string-content
   '((string_content) @elpi-string-content-face)
   :language 'elpi
   :override nil
   :feature 'punctuation-delimiter
   '([(comma) (pipe) (bind) (iff) (full_stop)]
     @elpi-punctuation-delimiter-face)
   :language 'elpi
   :override nil
   :feature 'label
   '([(io) (io_colon)] @elpi-mode-label-face)
   :language 'elpi
   :override nil
   :feature 'operator-builtin
   '([(is) (as)] @elpi-operator-builtin-face)
   :language 'elpi
   :override nil
   :feature 'constant-builtin
   '([(pi) (sigma) (cut)] @elpi-constant-builtin-face)
   :language 'elpi
   :override nil
   :feature 'type-builtin
   '([(typeid)] @elpi-kind-builtin-face)
   ;; Variables, wildcards, constants and macros.
   :language 'elpi
   :override nil
   :feature 'variable
   '((ucname) @elpi-variable-face)
   :language 'elpi
   :override nil
   :feature 'wildcard
   '([(uname) (freshuv)] @elpi-wildcard-face)
   :language 'elpi
   :override nil
   :feature 'constant
   '([(lcname) (qname) (bqname)] @elpi-constant-face)
   :language 'elpi
   :override nil
   :feature 'macro
   '((atname) @elpi-macro-face)
   )
  "Tree-sitter font-lock settings for `elpi-ts-mode'.")

;; Code indentation

;; Simple indentation rules

(defvar elpi-ts-mode--indent-rules
  `((elpi
     ((parent-is "source_file") column-0 0)
     ;; Program and namespace sections
     ((node-is "prog_begin") parent-bol 0)
     ((node-is "prog_end") parent-bol 0)
     ((parent-is "program_section")
      parent-bol elpi-ts-mode-indent-offset)
     ((parent-is "namespace_section")
      parent-bol elpi-ts-mode-indent-offset)
     ;; Block comments
     ((node-is "start_block_comment") parent-bol 0)
     ((parent-is "block_comment") parent-bol 1)
     ))
  "Tree-sitter indentation rules for `elpi-ts-mode'.")

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
                                     operator-builtin label constant-builtin
                                     type-builtin)
                (variable wildcard constant macro lambda-parameter
                          clause-head)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules elpi-ts-mode--indent-rules)

  (treesit-major-mode-setup))

(if (treesit-ready-p 'elpi)
    (progn
      (add-to-list 'auto-mode-alist '("\\.elpi\\'" . elpi-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.mod\\'" . elpi-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.sig\\'" . elpi-ts-mode))))

(provide 'elpi-ts-mode)
;;; elpi-ts-mode.el ends here
