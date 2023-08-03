;; Tree sitter grammar for the ELPI (https://github.com/LPCIC/elpi)
;; dialect of λProlog (https://www.lix.polytechnique.fr/~dale/lProlog/).
;;
;; File:            Local scopes specification.
;;
;; Original Author: Dominic Verity <dominic.verity@anu.edu.au>
;; License:         MIT
;;
;; Funding:         Partly funded under US Army ITC-IPAC R&D Project
;;                  contract FA520923C0004.
;; Project title:   "Towards a synthetic theory of Extended TQFTs"
;;
;; Copyright (c) 2023 Dominic Verity and The Australian National University

;; Local scopes and parameters of bindings

(abs_term) @local.scope
(abs_term left: (constant (_) @local.definition))

(multi_bind) @local.scope
(params (constant (_) @local.definition)+)

(constant (_) @local.reference)
