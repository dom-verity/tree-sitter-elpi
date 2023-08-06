;; Tree sitter grammar for the ELPI (https://github.com/LPCIC/elpi)
;; dialect of λProlog (https://www.lix.polytechnique.fr/~dale/lProlog/).
;;
;; File:            Local scopes specification.
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

;; Local scopes and parameters of bindings

(abs_term) @local.scope
(abs_term left: (constant (_) @local.definition))

(multi_bind) @local.scope
(params (constant (_) @local.definition)+)

(constant (_) @local.reference)
