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

(defvar elpi-ts-mode-version "1.0"
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
(defface elpi-bound-variable-face
  '((t . (:inherit font-lock-variable-name-face
          :foreground "purple")))
  "Face for applied instances of bound variables in ELPI mode.")
(defface elpi-parameter-face
  '((t . (:inherit font-lock-variable-name-face
          :foreground "purple"
          :underline "firebrick")))
  "Face for parameters of abstractions in ELPI mode.")
(defface elpi-constant-face
  '((t . (:inherit font-lock-constant-face)))
  "Face for constant identifiers in ELPI mode.")
(defface elpi-defining-instance-face
  '((t . (:inherit font-lock-constant-face
          :underline "firebrick")))
  "Face for defining instances within clauses in ELPI mode.")
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

;; Functions to "unwrap" nodes of various kinds and make their
;; children available for pattern matching in a pcase block.

(defun elpi-ts-mode--unwrap-multi-bind (node)
  "Unwrap a milti_bind NODE and return a list representing its children."
  (when (equal (treesit-node-type node) "multi_bind")
    `(,(treesit-node-type (treesit-node-child node 0))
      ,(treesit-node-children (treesit-node-child-by-field-name node "left"))
      ,(treesit-node-child-by-field-name node "right"))))

(defun elpi-ts-mode--unwrap-app-term (node)
  "Unwrap an app_term NODE and return a list representing its children."
  (when (equal (treesit-node-type node) "app_term")
    `(,(treesit-node-child-by-field-name node "left")
      ,(treesit-node-child-by-field-name node "right"))))

(defun elpi-ts-mode--unwrap-constant (node)
  "Unwrap a constant NODE and return a list containing its name."
  (when (equal (treesit-node-type node) "constant")
    `(,(treesit-node-text node))))

(defun elpi-ts-mode--unwrap-infix-term (node)
  "Unwrap an infix_term NODE and return a list containing its children."
  (when (equal (treesit-node-type node) "infix_term")
    `(,(treesit-node-type (treesit-node-child-by-field-name node "op"))
      ,(treesit-node-child-by-field-name node "left")
      ,(treesit-node-child-by-field-name node "right"))))

(defun elpi-ts-mode--unwrap-paren-term (node)
  "Unwrap a paren_term NODE and return a list containing the term it wraps."
  (when (equal (treesit-node-type node) "paren_term")
    `(,(treesit-node-child-by-field-name node "term"))))

(defun elpi-ts-mode--unwrap-spilled-term (node)
  "Unwrap a spilled_term NODE and return a list containing the term it wraps."
  (when (equal (treesit-node-type node) "spilled_term")
    `(,(treesit-node-child-by-field-name node "term"))))

(defun elpi-ts-mode-unwrap-app-term (node)
  "Unwrap an app_term NODE and return a list containing its children."
  (when (equal (treesit-node-type node) "app_term")
    `(,(treesit-node-child-by-field-name node "left")
      ,(treesit-node-child-by-field-name node "right"))))

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
                                   'elpi-bound-variable-face
                                   override start end)))

(defun elpi-ts-mode--fontify-defining-instances (node override start end &rest _)
  "Fontify the defining instances within a clause NODE.
OVERRIDE is the face override option of the calling font lock rule.
START and END specify the region to be fontified."
  (dolist (n (elpi-ts-mode--defining-instances node))
    (treesit-fontify-with-override (treesit-node-start n)
                                   (treesit-node-end n)
                                   'elpi-defining-instance-face
                                   override start end)))

(defun elpi-ts-mode--defining-instances (node)
  "Get defining occurrences within a D-formula NODE."
  (pcase node
    ((app elpi-ts-mode--unwrap-multi-bind `(,"pi" ,params ,body))
     (seq-difference (elpi-ts-mode--defining-instances body) params
                     (lambda (a b) (equal (treesit-node-text a)
                                          (treesit-node-text b)))))
    ((app elpi-ts-mode--unwrap-app-term `(,left ,right))
     (pcase right
       ((app elpi-ts-mode--unwrap-spilled-term `(,term))
        (seq-concatenate 'list
                         (elpi-ts-mode--defining-instances left)
                         (elpi-ts-mode--defining-instances term)))
       (_ (elpi-ts-mode--defining-instances left))))
    ((app elpi-ts-mode--unwrap-paren-term `(,term))
     (elpi-ts-mode--defining-instances term))
    ((pred elpi-ts-mode--unwrap-constant) `(,node))
    ((app elpi-ts-mode--unwrap-infix-term `(,op ,left ,right))
     (pcase op
       ((or "as" "vdash") (elpi-ts-mode--defining-instances left))
       ("darrow" (elpi-ts-mode--defining-instances right))
       ((or "conj" "conj2")
        (seq-concatenate 'list
                         (elpi-ts-mode--defining-instances left)
                         (elpi-ts-mode--defining-instances right)))))))

;; Font-locking rules

(defvar elpi-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   ;; Overriding rules (special cases)
   :language 'elpi
   :override t
   :feature 'comment
   '([(line_comment) (block_comment) (skip_comment_head) (skip_comment_line)]
     @elpi-comment-face)
   :language 'elpi
   :override t
   :feature 'escape
   '([(escape_sequence) (quote_escape)] @elpi-escape-face)
   :language 'elpi
   :override t
   :feature 'lambda-parameter
   '((abs_term left: (constant (_) @elpi-parameter-face))
     (multi_bind (params (constant (_) @elpi-parameter-face)))
     (constant) @elpi-ts-mode--fontify-bound-variable)
   :language 'elpi
   :override t
   :feature 'constant
   `((constant ,(cl-map 'vector #'(lambda (x) `(,x)) elpi-ts-mode--operators)
               @elpi-constant-face))
   :language 'elpi
   :override t
   :feature 'defining-instances
   '((clause_decl clause: (_) @elpi-ts-mode--fontify-defining-instances)
     (pred_decl (constant) @elpi-defining-instance-face)
     (mode_decl (constant) @elpi-defining-instance-face)
     (kind_decl (constant) @elpi-defining-instance-face)
     (type_decl (constant) @elpi-defining-instance-face))
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

(defvar elpi-ts-mode--op-type-regex
  (rx
   string-start
   (or "infix_term" "prefix_term" "postfix_term")
   string-end)
  "Regex to match the type of an operator node.")

(defvar elpi-ts-mode--binder-type-regex
  (rx
   string-start
   (or "abs_term" "multi_bind")
   string-end)
  "Regex to match the type of a binder node.")

(defvar elpi-ts-mode--op-or-binder-type-regex
  (rx
   string-start
   (or "abs_term" "multi_bind" "infix_term" "prefix_term" "postfix_term")
   string-end)
  "Regex to match the type of an operator or binder node.")

(defvar elpi-ts-mode--term-type-regex
  (rx
   string-start
   (or "infix_term" "prefix_term" "postfix_term" "app_term"
       "abs_term" "multi_bind" "list_term" "spilled_term"
       "cut" "pi" "sigma" "constant" "integer" "float"
       "string" "paren_term")
   string-end)
  "Regex to match the type of any term node.")

(defun elpi-ts-mode--nth-sibling-bol (nth &optional named)
  "Return position of the first non-space character on the line of NTH sibling.
If NAMED is true then ignored children without names."
  (lambda (_ parent &rest _)
    (save-excursion
    (goto-char (treesit-node-start (treesit-node-child parent nth named)))
    (back-to-indentation)
    (point))))

(defun elpi-ts-mode--named-sibling-type (name type)
  "Test whether TYPE is the type of sibling of current node named NAME."
  (lambda (_ parent &rest _)
    (equal (treesit-node-type
            (treesit-node-child-by-field-name parent name))
           type)))

(defun elpi-ts-mode--app-term-params-start (_ parent &rest _)
  "Get the start position the first parameter of an applicative PARENT term.
Assumes that the current node is the right child of an \"app_term\" node."
  (let ((left-child (treesit-node-child-by-field-name parent "left")))
    (while (equal (treesit-node-type left-child) "app_term")
      (setq parent left-child)
      (setq left-child (treesit-node-child-by-field-name parent "left")))
    (treesit-node-start (treesit-node-child-by-field-name parent "right"))))

(defun elpi-ts-mode--enclosing-expr-on-line (node regex)
  "NODE REGEX."
  (let ((parent (treesit-node-parent node))
        (bol (save-excursion
               (goto-char (treesit-node-start node))
               (forward-line 0)
               (point))))
    (while (and (string-match-p regex (or (treesit-node-type parent) ""))
                (<= bol (treesit-node-start parent)))
    (setq node parent)
    (setq parent (treesit-node-parent node)))
    node))

(defun elpi-ts-mode--indent-as-standalone-body-p (_ parent &rest _)
  "Indent current sub-expression of PARENT as 1st line of binder body."
  (string-match-p
   elpi-ts-mode--binder-type-regex
   (or (treesit-node-type parent) "")))

(defun elpi-ts-mode--anchor-standalone-body (_ parent &rest _)
  "Anchor current sub-expression of PARENT to head binder."
  (treesit-node-start
   (elpi-ts-mode--enclosing-expr-on-line parent elpi-ts-mode--binder-type-regex)))

(defun elpi-ts-mode--indent-as-op-expr-second-line-p (_ parent &rest _)
  "Indent current sub-expression of PARENT as 2nd line of operator expression."
  (not (string-match-p
        elpi-ts-mode--op-or-binder-type-regex
        (or (treesit-node-type
             (treesit-node-parent
              (elpi-ts-mode--enclosing-expr-on-line
               parent
               elpi-ts-mode--op-type-regex)))
            ""))))

(defun elpi-ts-mode--anchor-op-expr-line (_ parent &rest _)
  "Anchor current sub-expression of PARENT to previous line of op expression."
  (treesit-node-start
   (elpi-ts-mode--enclosing-expr-on-line parent elpi-ts-mode--op-type-regex)))

(defun elpi-ts-mode--anchor-comment (node &rest _)
  "Anchor to immediately following non-empty line after NODE."
  (save-excursion
    (goto-char (treesit-node-end node))
    (forward-line)
    (if (re-search-forward (rx (not blank)) (line-end-position) t)
        (match-beginning 0)
      nil)))

(defvar elpi-ts-mode--indent-rules
  `((elpi
     ;; Note: In situations in which two rules in this list match the same
     ;; node, only the earlier of those two rules is applied.
     ;;
     ;; Empty lines
     (no-node column-0 0)
     ;; Comments, come early since they can occur anywhere in the tree
     ;; and we want to avoid them being handled by more specific rules later.
     ((and
       (node-is
        ,(rx string-start
             (or "block_comment" "line_comment" "skip_comment_head")
             string-end))
       elpi-ts-mode--anchor-comment)
      elpi-ts-mode--anchor-comment 0)
     ((node-is "skip_comment_line") no-indent 0)
     ((node-is "start_block_comment") parent 0)
     ((match ,(rx string-start
                  (or "end_block_comment" "block_comment_line")
                  string-end)
             nil nil 1 1)
      (nth-sibling 0) 1)
     ((node-is "block_comment_line") (nth-sibling 1) 0)
     ((node-is "end_block_comment") (elpi-ts-mode--nth-sibling-bol 1) 0)
     ;; Root node.
     ((parent-is "source_file") column-0 0)
     ;; Parameters of applicative terms
     ((and
       (match nil "app_term" "right")
       (not (elpi-ts-mode--named-sibling-type "left" "app_term")))
      parent  elpi-ts-mode-indent-offset)
     ((match nil "app_term" "right") elpi-ts-mode--app-term-params-start 0)
     ;; Parameters of a multi-bind.

     ;; Sub-terms within an expression.
     ((and
       (or (field-is "op")
           (node-is ,elpi-ts-mode--term-type-regex))
       elpi-ts-mode--indent-as-standalone-body-p)
      elpi-ts-mode--anchor-standalone-body elpi-ts-mode-indent-offset)
     ((and
       (or (field-is "op")
           (node-is ,elpi-ts-mode--term-type-regex))
       elpi-ts-mode--indent-as-op-expr-second-line-p)
      elpi-ts-mode--anchor-op-expr-line elpi-ts-mode-indent-offset)
     ((or (field-is "op")
          (node-is ,elpi-ts-mode--term-type-regex))
      elpi-ts-mode--anchor-op-expr-line 0)
     ((match "rparen" "paren_term") parent 0)
     ((match "rcurly" "spilled_term") parent 0)
     ((parent-is ,(rx string-start
                      (or "paren_term" "spilled_term")
                      string-end))
      parent elpi-ts-mode-indent-offset)
     ;; Indenting declarations
     ((parent-is "kind_term") parent-bol elpi-ts-mode-indent-offset)
     ;; Program and name-space sections
     ((node-is "prog_begin") parent-bol 0)
     ((node-is "prog_end") parent-bol 0)
     ((parent-is "program_section") parent-bol elpi-ts-mode-indent-offset)
     ((parent-is "namespace_section") parent-bol elpi-ts-mode-indent-offset)
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
                          defining-instances)))

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
