;; Tree sitter grammar for the ELPI (https://github.com/LPCIC/elpi)
;; dialect of λProlog (https://www.lix.polytechnique.fr/~dale/lProlog/).
;;
;; File:            Syntax highlights specification.
;;
;; Original Author: Dominic Verity <dominic.verity@anu.edu.au>
;; License:         MIT
;;
;; Funding:         Partly funded under US Army ITC-IPAC R&D Project
;;                  contract FA520923C0004.
;; Project title:   "Towards a synthetic theory of Extended TQFTs"
;;
;; Copyright (c) 2023 Dominic Verity and The Australian National University

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
(typeid) @reserved
(pi) @reserved
(sigma) @reserved
(cut) @reserved

;; Brackets
(prog_begin) @bracket
(prog_end) @bracket
(lparen) @bracket
(rparen) @bracket
(lbracket) @bracket
(rbracket) @bracket
(lcurly) @bracket
(rcurly) @bracket

;; Delimiters
(comma) @delimiter
(pipe) @delimiter
(bind) @delimiter
(iff) @delimiter
(full_stop) @delimiter

;; Identifiers
((name (ucname)) @variable (#is-not? local))
((name [(uname) (freshuv)]) @wildcard (#is-not? local))
((name [(lcname) (qname) (bqname)]) @constant (#is-not? local))
((name (atname)) @macro (#is-not? local))

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
(abs_term (name) @parameter (bind))
(multi_bind (name) @parameter (bind))
