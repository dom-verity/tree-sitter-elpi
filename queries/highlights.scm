;; Tree sitter grammar for the ELPI (https://github.com/LPCIC/elpi)
;; dialect of λProlog (https://www.lix.polytechnique.fr/~dale/lProlog/).
;;
;; File:            Syntax highlights specifications.
;;
;; Original Author: Dominic Verity <dominic.verity@anu.edu.au>
;; License:         GPL v3
;; Created:         August 2023
;;
;; Funding:         Partly funded under US Army ITC-IPAC R&D Project
;;                  contract FA520923C0004.
;; Project title:   "Towards a synthetic theory of Extended TQFTs"
;;
;; Copyright (c) 2023 Dominic Verity and The Australian National University
;;
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

;; Keywords
(kind) @keyword
(type) @keyword
(typeabbrev) @keyword
(shorten) @keyword
(accumulate) @keyword
(import) @keyword
(accum_sig) @keyword
(use_sig) @keyword
(local) @keyword
(mode) @keyword
(pred) @keyword
(macro) @keyword
(rule) @keyword
(namespace) @keyword
(constraint) @keyword

;; Ignored keywords
(infix) @disabled
(infixl) @disabled
(infixr) @disabled
(prefix) @disabled
(prefixr) @disabled
(postfix) @disabled
(postfixl) @disabled
(module) @disabled
(sig) @disabled
(exportdef) @disabled
(localkind) @disabled
(useonly) @disabled
(closed) @disabled

;; Attributes
(external) @attribute
(cexternal) @attribute
(cif) @attribute
(cname) @attribute
(cafter) @attribute
(cbefore) @attribute
(creplace) @attribute
(cindex) @attribute

;; Comments
(block_comment) @comment
(line_comment) @comment
(skip_comment) @comment

;; Reserved names
(typeid) @type.builtin
(pi) @constant.builtin
(sigma) @constant.builtin
(cut) @constant.builtin

;; Brackets
(prog_begin) @punctuation.bracket
(prog_end) @punctuation.bracket
(lparen) @punctuation.bracket
(rparen) @punctuation.bracket
(lbracket) @punctuation.bracket
(rbracket) @punctuation.bracket
(lcurly) @punctuation.bracket
(rcurly) @punctuation.bracket

;; Delimiters
(comma) @punctuation.delimiter
(pipe) @punctuation.delimiter
(bind) @punctuation.delimiter
(iff) @punctuation.delimiter
(full_stop) @punctuation.delimiter

;; Identifiers
(constant ((ucname) @variable (#is-not? local)))
(constant ([(uname) (freshuv)] @wildcard (#is-not? local)))
(constant ([(lcname) (qname) (bqname)] @constant (#is-not? local)))
(constant ((atname) @macro (#is-not? local)))

;; Literal constants
(string) @string
[(integer) (float)] @number

;; Operators
(vdash) @operator
(qdash) @operator
(or) @operator
(conj) @operator
(conj2) @operator
(arrow) @operator
(darrow) @operator
(eq) @operator
(eq2) @operator
(family_lt) @operator
(family_gt) @operator
(cons) @operator
(family_tick) @operator
(family_exp) @operator
(family_plus) @operator
(minus) @operator
(minusr) @operator
(minusi) @operator
(minuss) @operator
(family_times) @operator
(slash) @operator
(div) @operator
(mod) @operator
(family_minus) @operator
(family_btick) @operator
(family_eq) @operator
(family_or) @operator
(family_and) @operator
(family_sharp) @operator
(family_tilde) @operator

(as) @naming
(is) @evaluation

;; Modes
(io) @mode
(io_colon) @mode

;; Bound variables
(abs_term left: (constant (_) @variable.parameter))
(params (constant (_) @variable.parameter)+)
