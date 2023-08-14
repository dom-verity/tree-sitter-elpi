/**
 * Tree sitter grammar for the ELPI (https://github.com/LPCIC/elpi)
 * dialect of λProlog (https://www.lix.polytechnique.fr/~dale/lProlog/).
 *
 * File:            Tree sitter grammar specification.
 *
 * Original Author: Dominic Verity (dominic.verity@anu.edu.au)
 * License:         GPL v3
 * Created:         August 2023
 *
 * Funding:         Partly funded under US Army ITC-IPAC R&D Project
 *                  contract FA520923C0004.
 * Project title:   "Towards a synthetic theory of Extended TQFTs"
 *
 * Copyright (c) 2023 Dominic Verity and The Australian National University
 *
 * This file is part of tree-sitter-elpi.
 *
 * tree-sitter-elpi is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * tree-sitter-elpi is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * tree-sitter-elpi. If not, see <https://www.gnu.org/licenses/>.
 */

const ucase = /[A-Z]/;
const lcase = /[a-z]/;
const digit = /[0-9]/;
const schar2 =  /[\+\*/\^<>`'\?@#~=&!]/;
const schar = choice(schar2, /[-\$_]/);
const idchar = choice(ucase, lcase, digit, schar);
const idcharns = choice(idchar, seq('.', choice(ucase, lcase)));
const symbchar = choice(idchar, ':');
const allnames = choice(
    repeat1(symbchar),
    seq('_', repeat(idchar)),
    seq(ucase, repeat(idchar)),
    seq(lcase, repeat(idcharns)),
    seq('@',repeat(idchar)));
const ws = /[^[^\s]\r\n]/;
const newline = /\r?\n|\r/;

const PREC = {
    AS: [0, prec],                      // as (non assoc, infix)
    BIND: [1, prec.right],              // \ (right assoc, infix)
    VDASH: [2, prec],                   // :- (non assoc, infix)
    QDASH: [2, prec],                   // ?- (non assoc, infix)
    OR: [3, prec.right],                // ; (right assoc, infix)
    CONJ: [4, prec.right],              // , (right assoc, infix)
    CONJ2: [4, prec.right],             // & (right assoc, infix)
    ARROW: [5, prec.right],             // -> (right assoc, infix)
    DARROW: [6, prec.right],            // => (right assoc, infix)
    EQ: [7, prec],                      // = (non assoc, infix)
    EQ2: [7, prec],                     // == (non assoc, infix)
    FAMILY_LT: [7, prec],               // /<[symbchar]*/, =<, r<, i<, s<, r=<, i=<, s=< (non assoc, infix)
    FAMILY_GT: [7, prec],               // />[symbchar]*/, r>, i>, s>, r>=, i>=, s>= (non assoc, infix)
    IS: [7, prec],                      // is (non assoc, infix)
    CONS: [8, prec.right],              // :: (right assoc, infix)
    FAMILY_TICK: [9, prec],             // /'[symbchar]*/ (non assoc, infix)
    FAMILY_EXP: [10, prec.left],        // /^[symbchar]*/ (left assoc, infix)
    FAMILY_PLUS: [10, prec.left],       // /+[symbchar]*/, r+, i+, s+ (left assoc, infix)
    MINUS: [10, prec.left],             // - (left assoc, infix)
    MINUSR: [10, prec.left],            // r- (left assoc, infix)
    MINUSI: [10, prec.left],            // i- (left assoc, infix)
    MINUSS: [10, prec.left],            // s- (left assoc, infix)
    FAMILY_TIMES: [11, prec.left],      // /*[symbchar]*/, r*, i*, s* (left assoc, infix)
    SLASH: [11, prec.left],             // / (left assoc, infix)
    DIV: [11, prec.left],               // div (left assoc, infix)
    MOD: [11, prec.left],               // mod (left assoc, infix)
    FAMILY_MINUS: [12, prec.right],     // /--[symbchar]*/ (right assoc, infix)
    FAMILY_BTICK: [13, prec],           // /`[symbchar]*/ (non assoc, infix)
    FAMILY_EQ: [14, prec.right],        // /==[symbchar]*/ (right assoc, infix)
    FAMILY_OR: [15, prec.right],        // /||[symbchar]*/ (right assoc, infix)
    FAMILY_AND: [16, prec.right],       // /&&[symbchar]*/ (right assoc, infix)
    FAMILY_SHARP: [17, prec.left],      // /#[symbchar]*/ (left assoc, infix)
    FAMILY_TILDE: [18, prec],           // /~[symbchar]*/, r~, i~ (non assoc, prefix)
    FAMILY_QMARK: [19, prec],           // /?[symbchar]*/ (non assoc, postfix)
    APP: [20, prec.left],               // application
    ATOMIC: [21, prec],                 // most tightly bound sequences etc
    COMMA: [22, prec],                  // comma separator in prolog list notation
    DEFTO: [22, prec]                   // turnstile in macro definition
};

function apply_prec(n, r) {
    p = PREC[n.toUpperCase()];
    return p[1](p[0], r);
};

function infix_rule(g, n, l, r) {
    p = PREC[n.toUpperCase()];
    return p[1](p[0], seq(
        field("left", l),
        field("op", g[n.toLowerCase()]),
        field("right", r)));
};

function postfix_rule(g, n, e) {
    p = PREC[n.toUpperCase()];
    return p[1](p[0], seq(
        field("exp", e),
        field("op", g[n.toLowerCase()])));
};

function prefix_rule(g, n, e) {
    p = PREC[n.toUpperCase()];
    return p[1](p[0], seq(
        field("op", g[n.toLowerCase()]),
        field("exp", e)));
};

function sep_list1(s, r) {
    return seq(r, repeat(seq(s, r)));
}

function sep_list(s, r) {
    return optional(sep_list1(s, r));
}

module.exports = grammar({
    name: 'elpi',

    externals: $ => [
        $.skip_comment_old,
        $.eof,
        $.block_comment_line
    ],

    extras: $ => [
        $._whitespace,
        $._newline,
        $.block_comment,
        $.line_comment,
        $.skip_comment
    ],

    conflicts: $ => [
        [$.infix_term, $.infix_term],
        [$.lcurly, $.prog_begin],
        [$.multi_bind, $._atomic_term]
    ],

    rules: {
        source_file: $ => repeat($._decl),

        word: $ => token(allnames),

        _whitespace: $ => token(repeat1(ws)),

        _newline: $ => token(newline),

        _decl: $ => choice(
            $.kind_decl,
            $.type_decl,
            $.abbrev_decl,
            $.fixity_decl,
            $.mode_decl,
            $.pred_decl,
            $.clause_decl,
            $.macro_decl,
            $._ignored_decl,
            $.namespace_section,
            $.program_section,
            $.constraint_section,
            $.shorten_decl,
            $.accumulate_decl,
            $.local_decl
        ),

        // Attributes
        attributes: $ => choice(
            $.external,
            repeat1($.attribute)
        ),

        attribute: $ => choice(
            seq(choice($.cif, $.cname, $.cafter, $.cbefore, $.creplace),
                $.string),
            $.cexternal,
            seq($.cindex, $.lparen, repeat1($.indexing), $.rparen)
        ),

        indexing: $ => choice(
            $.freshuv,
            $.integer
        ),

        // Kind declarations
        kind_term: $ => choice(
            $.typeid,
            infix_rule($, "arrow", $.typeid, $.kind_term)
        ),

        kind_decl: $ => seq($.kind, sep_list1($.comma, $.constant),
                            $.kind_term, $._terminator),

        // Type declarations
        type_term: $ => choice(
            $._ctype_term,
            infix_rule($, "arrow", $.type_term, $.type_term)
        ),

        _ctype_term: $ => choice(
            $.constant,
            $._atype_term,
            seq($.lparen, $.type_term, $.rparen)
        ),

        _atype_term: $ => apply_prec("atomic", seq(
            $.constant,
            repeat1($._atype_param)
        )),

        _atype_param: $ => choice(
            $.string,
            $.constant,
            seq($.lparen, $.type_term, $.rparen)
        ),

        type_decl: $ => seq(
            optional($.attributes), $.type, sep_list1($.comma, $.constant),
            $.type_term, $._terminator
        ),

        // Type abbreviation
        abbrev_form: $ => choice(
            $.constant,
            seq($.lparen, $.constant, repeat1($.constant), $.rparen)
            // seq($.lparen, $.abbrev_form, $.rparen)
        ),

        abbrev_decl: $ => seq($.typeabbrev, $.abbrev_form, $.type_term,
                              $._terminator),

        // Fixity declaration - compatible with Teyjus, parse error in ELPI
        fixity: $ => choice(
            $.infix, $.infixl, $.infixr,
            $.prefix, $.prefixr,
            $.postfix, $.postfixl
        ),

        fixity_decl: $ => seq($.fixity, sep_list1($.comma, $.constant),
                              $.integer, $._terminator),

        // Mode and predicate declarations
        mode_decl: $ => seq(
            $.mode, $.lparen, $.constant, repeat1($.io),
            $.rparen, $._terminator
        ),

        pred_decl: $ => seq(
            optional($.attributes), $.pred, $.constant,
            sep_list(optional($.comma), $.pred_item), $._terminator
        ),

        pred_item: $ => seq($.io_colon, $.type_term),

        // Clause declarations, the syntax of these is a subset of term syntax.
        clause_decl: $ => seq(
            optional($.attributes), $._term, $._terminator
        ),

        // Macro declarations
        macro_decl: $ => seq(
            $.macro, $.constant, repeat($.constant),
            $.defto, $._term, $._terminator
        ),

        // Namespaces
        namespace_section: $ => seq(
            $.namespace, $.constant, $.prog_begin, repeat($._decl), $.prog_end
        ),

        // Program
        program_section: $ => prec.dynamic(1, seq(
            $.prog_begin, repeat($._decl), $.prog_end
        )),

        // Shorten
        shorten_decl: $ => seq(
            $.shorten, $.trie, $._terminator
        ),

        trie: $ => seq(
            $.constant, $.full_stop, $.lcurly,
            sep_list1($.comma, $.subtrie), $.rcurly
        ),

        subtrie: $ => seq(
            $.constant, optional(seq($.full_stop, $.lcurly,
                                 sep_list1($.comma, $.subtrie),$.rcurly))
        ),

        // Local declaration
        local_decl: $ => seq(
            $.local, sep_list1($.comma, $.constant), optional($.type_term),
            $._terminator
        ),

        // Teyjus declarations which are ignored in ELPI
        _ignored_decl: $ => choice(
            $.module_decl,
            $.sig_decl,
            $.exportdef_decl,
            $.localkind_decl,
            $.useonly_decl,
            $.closed_decl
        ),

        module_decl: $ => seq($.module, $.constant, $._terminator),
        sig_decl: $ => seq($.sig, $.constant, $._terminator),
        exportdef_decl: $ => seq($.exportdef, sep_list1($.comma, $.constant),
                                 optional($.type_term), $._terminator),
        localkind_decl: $ => seq($.localkind, sep_list1($.comma, $.constant),
                                 $._terminator),
        useonly_decl: $ => seq($.useonly, sep_list1($.comma, $.constant),
                               optional($.type_term), $._terminator),
        closed_decl: $ => seq($.closed, sep_list1($.comma, $.constant),
                              $._terminator),

        // ELPI recognises these imports, they are all equivalent to "accumulate"
        accumulate_decl: $ => seq(
            choice($.accumulate, $.import, $.accum_sig, $.use_sig),
            sep_list1($.comma, $.filename)
        ),

        filename: $ => choice($.constant, $.string),

        // Constraint handling rules
        constraint_section: $=> seq(
            $.constraint, repeat($.constant), $.lcurly,
            repeat($.chr_rule), $.rcurly
        ),

        chr_rule: $ => seq(
            optional($.attributes), $.rule, repeat($.sequent), $.bind,
            repeat1($.sequent), $.pipe, $._term, $.iff, $.sequent
        ),

        sequent: $ => choice(
            $.closed_term,
            seq($.lparen, $.constant, $.colon, $._term, $.rparen)
        ),

        // Terms
        _term: $ => choice(
            $._atomic_term,
            $.infix_term,
            $.prefix_term,
            $.postfix_term,
            $.app_term,
            $.abs_term,
            $.multi_bind
        ),

        _atomic_term: $ => choice(
            $.list_term,
            $.spilled_term,
            $.cut, $.pi, $.sigma,
            $.constant,
            $.integer,
            $.float,
            $.string,
            $.paren_term
        ),

        closed_term: $ => $._atomic_term,

        /*
         * There doesn't appear to be an easy way to correctly implement
         * non-associative operators, so for the moment we simply put up
         * with the conflicts they introduce using the conflicts field.
         * This solution parses legal terms correctly, but it also parses
         * illegal expressions like "a == b == c" by picking an associativity.
         */

        infix_term: $ => {
            const table = [
                // Basic infix operators.
                "minus", "minusr", "minusi", "minuss",
                "or", "mod", "div", "arrow", "darrow",
                "slash", "conj2", "conj",
                // non-associative
                "as", "qdash", "vdash", "eq", "eq2", "is",
                // Extensible infix operator families
                "family_plus", "family_times", "family_minus",
                "family_exp", "family_eq", "family_and",
                "family_or", "family_sharp",
                // non-associative
                "family_lt", "family_gt", "family_tick",
                "family_btick"
            ];

            return choice(...table.map(
                (op) => infix_rule($, op, $._term, $._term)));
        },

        prefix_term: $ => prefix_rule($, "family_tilde", $._term),

        postfix_term: $ => postfix_rule($, "family_qmark", $._term),

        app_term: $ => prec.dynamic(0, apply_prec("app",
                                                  seq(field("left", $._term),
                                                      field("right", $._term)))),

        abs_term: $ => infix_rule($, "bind", $.constant, $._term),

        multi_bind: $ => prec.dynamic(1, seq(
            choice($.pi, $.sigma),
            infix_rule($, "bind", $.params, $._term))),

        params: $ => repeat1($.constant),

        spilled_term: $ => prec.dynamic(0, seq($.lcurly, $._term, $.rcurly)),

        paren_term: $ => seq($.lparen, field("term", $._term), $.rparen),

        list_term: $ => seq($.lbracket, optional($._list_items), $.rbracket),

        _list_items: $ => seq(
            $._list_items2,
            optional(seq($.pipe, $._term))),

        /*
         * In the following we create a second terminal $.comma which
         * matches "," and give it a higher precedence than $.conj. This
         * disambiguates the use of "," in a list of terms by preferring its
         * use as a list item separator.
         *
         * Note: this method doesn't work if we use sep_list1, so we hard
         * code the production to implement a comma separated list.
         */
        _list_items2: $ => choice(
            $._term,
            seq($._term, $.comma, $._list_items2)
        ),

        /* not sure if we are allowing typed identifiers...
           typedid: $ => choice(
           $.abstoken,
           seq($.abstoken, $.colon, $.type_term),
           seq($.lparen, $.typedid, $.type_term)
           ),

           typedname: $ => choice(
           $.name,
           seq($.name, $.colon, $.type_term),
           seq($.lparen, $.typedname, $.type_term)
           ),
        */

        // Keywords
        kind: $ => token("kind"),
        type: $ => token("type"),
        typeabbrev: $ => token("typeabbrev"),
        infix: $ => token("infix"),
        infixl: $ => token("infixl"),
        infixr: $ => token("infixr"),
        prefix: $ => token("prefix"),
        prefixr: $ => token("prefixr"),
        postfix: $ => token("postfix"),
        postfixl: $ => token("postfixl"),
        shorten: $ => token("shorten"),
        accumulate: $ => token("accumulate"),
        local: $=> token("local"),
        mode: $ => token("mode"),
        pred: $ => token("pred"),
        macro: $ => token("macro"),
        rule: $ => token("rule"),
        namespace: $ => token("namespace"),
        constraint: $ => token("constraint"),
        module: $ => token("module"),
        sig: $ => token("sig"),
        import: $ => token("import"),
        accum_sig: $ => token("accum_sig"),
        use_sig: $ => token("use_sig"),
        localkind: $ => token("localkind"),
        useonly: $ => token("useonly"),
        exportdef: $ => token("exportdef"),
        closed: $ => token("closed"),

        pi: $ => token("pi"),
        sigma: $ => token("sigma"),
        typeid: $ => token("type"),

        // Attributes
        external: $ => token("external"),
        cexternal: $ => token(":external"),
        cif: $ => token(":if"),
        cname: $ => token(":name"),
        cafter: $ => token(":after"),
        cbefore: $ => token(":before"),
        creplace: $ => token(":replace"),
        cindex: $ => token(":index"),

        // Modes
        io: $ => token(/i|o/),
        io_colon: $ => token(/(i|o):/),

        // Names
        ucname: $ => token(seq(ucase, repeat(idchar))),
        lcname: $ => token(seq(lcase, repeat(idchar))),
        uname: $ => token(seq('_', repeat1(idchar))),
        qname: $ => token(seq('\'',  repeat(symbchar), '\'')),
        bqname: $ => token(seq('`', repeat(symbchar), '`')),
        atname: $ => token(seq('@', repeat(idchar))),
        freshuv: $ => token('_'),

        _mixfix: $ => {
            const table = [
                "cons", "eq", "minus", "minusr", "minusi", "minuss",
                "eq2", "or", "is", "mod", "div", "arrow", "darrow",
                "qdash", "slash", "conj2", "conj", "vdash",
                "family_plus", "family_times", "family_minus",
                "family_exp", "family_lt", "family_gt", "family_eq",
                "family_and", "family_or", "family_sharp", "family_btick",
                "family_tick", "as", "family_tilde", "family_qmark"
            ];

            return choice(...table.map((op) => $[op]));
        },

        constant: $ => choice(
            field("id",
                  choice($.ucname, $.lcname, $.uname, $.atname,
                         $.qname, $.bqname, $.freshuv)),
            seq($.lparen, field("id", $._mixfix), $.rparen)
        ),

        full_stop: $ => token('.'),
        _terminator: $ => choice($.full_stop, $.eof),
        colon: $ => token(':'),
        lparen: $ => token("("),
        rparen: $ => token(")"),
        lbracket: $ => token("["),
        rbracket: $ => token("]"),
        lcurly: $ => token("{"),
        rcurly: $ => token("}"),
        tick: $ => token("\'"),
        btick: $ => token("`"),
        pipe: $ => token("|"),

        bind: $ => token('\\'),
        vdash: $ => token(":-"),
        qdash: $ => token("?-"),
        or: $ => token(';'),
        conj: $ => token(','),
        conj2: $ => token('&'),
        arrow: $ => token("->"),
        darrow: $ => token("=>"),
        iff: $ => token("<=>"),
        eq: $ => token("="),
        eq2: $ => token("=="),
        family_lt: $ => token(choice(
            seq('<', repeat(symbchar)),
            "=<", "r<", "i<", "s<", "r=<", "i=<", "s=<")),
        family_gt: $ => token(choice(
            seq('>', repeat(symbchar)),
            "r>", "i>", "s>", "r>=", "i>=", "s>=")),
        is: $ => token("is"),
        cons: $ => token("::"),
        family_tick: $ => token(seq('\'', repeat(symbchar))),
        family_exp: $ => token(seq('^', repeat(symbchar))),
        family_plus: $ => token(choice(
            seq('+', repeat(symbchar)),
            "r+", "i+", "s+")),
        minus: $ => token('-'),
        minusr: $ => token('r-'),
        minusi: $ => token('i-'),
        minuss: $ => token('s-'),
        family_times: $ => token(choice(
            seq('*', repeat(symbchar)),
            "r*", "i*", "s*")),
        slash: $ => token('/'),
        div: $ => token("div"),
        mod: $ => token("mod"),
        family_minus: $ => token(seq("--", repeat(symbchar))),
        family_btick: $ => token(seq('`', repeat(symbchar))),
        family_eq: $ => token(seq("==", repeat(symbchar))),
        family_or: $ => token(seq("||", repeat(symbchar))),
        family_and: $ => token(seq("&&", repeat(symbchar))),
        family_sharp: $ => token(seq('#', repeat(symbchar))),
        family_tilde: $ => token(seq('~', repeat(symbchar))),
        family_qmark: $ => token(seq('?', repeat(symbchar))),

        cut: $ => token('!'),

        defto: $ => apply_prec("defto", token(":-")),
        comma: $ => apply_prec("comma", token(",")),
        prog_begin: $ => token("{"),
        prog_end: $ => token("}"),

        integer: $ => token(/-?[0-9]+/),
        float: $ => token(/[0-9]*\.[0-9]+/),

        as: $ => token("as"),

        start_block_comment: $ => token('/*'),
        end_block_comment: $ => token("*/"),

        block_comment: $ => seq(
            $.start_block_comment,
            repeat($.block_comment_line),
            $.end_block_comment
        ),

        skip_comment: $ => token(
            prec(1, seq("%", repeat(ws), "elpi:skip", /[^\n\r]*/))),

        line_comment: $ => token(prec(0, /%[^\n\r]*/)),

        escape_sequence: $ => token.immediate(
            seq('\\', choice('n', 'b', 't', 'r', '\\', '"'))
        ),

        quote_escape: $ => token.immediate(
            seq('"', '"')
        ),

        string_content: $ => token.immediate(/[^\\"\"\n]+/),

        string_newline: $ => token.immediate(/\r?\n/),

        string: $ => seq(
            '"',
            repeat(choice(
                $.string_content,
                $.string_newline,
                $.escape_sequence,
                $.quote_escape
            )),
            token.immediate('"')
        ),
    }
});
