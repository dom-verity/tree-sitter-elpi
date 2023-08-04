/**
 * Tree sitter grammar for the ELPI (https://github.com/LPCIC/elpi)
 * dialect of λProlog (https://www.lix.polytechnique.fr/~dale/lProlog/).
 *
 * File:            C scanner, implements skip comments and EOF.
 *
 * Original Author: Dominic Verity <dominic.verity@anu.edu.au>
 * License:         MIT
 *
 * Funding:         Partly funded under US Army ITC-IPAC R&D Project
 *                  contract FA520923C0004.
 * Project title:   "Towards a synthetic theory of Extended TQFTs"
 *
 * Copyright (c) 2023 Dominic Verity and The Australian National University
 */

#include <tree_sitter/parser.h>
#include <wchar.h>

enum TokenType {
    SKIP_COMMENT,
    END_OF_FILE
};

void * tree_sitter_elpi_external_scanner_create() {
    // Nothing allocated
    return NULL;
}

void tree_sitter_elpi_external_scanner_destroy(void *payload) {
    // Nothing to destroy
}

unsigned tree_sitter_elpi_external_scanner_serialize(void *payload,
                                                     void *buffer) {
    // No state stored between calls
    return 0;
}

void tree_sitter_elpi_external_scanner_deserialize(void *payload,
                                                   const char *buffer,
                                                   unsigned length) {
    // Nothing to restore
}

const int START_STATE = 0;
const int ERROR_STATE = -1;
const int END_STATE = 100;

#define NEXT(ws, eof_state)\
    lexer->advance(lexer, ws);\
    if (lexer->eof(lexer)) { state = eof_state; }

bool tree_sitter_elpi_external_scanner_scan(void *payload,
                                            TSLexer *lexer,
                                            const bool *valid_symbols) {
    int blanks = 0;
    int skip_lines = 0;
    int state = START_STATE;

    while (state != END_STATE && state != ERROR_STATE) {
        switch (state) {
        case 0: // skip over whitespace paying attention to EOF.
            if (lexer->eof(lexer)) {
                if (valid_symbols[END_OF_FILE]) {
                    lexer->result_symbol = END_OF_FILE;
                    state = END_STATE;
                } else {
                    state = ERROR_STATE;
                }
            } else if (isspace(lexer->lookahead)) {
                lexer->advance(lexer,true);
            } else {
                state = 1;
            }
            break;
        case 1: // States to handle skip comments
            if (lexer->lookahead == '%' && valid_symbols[SKIP_COMMENT]) {
                NEXT(false, ERROR_STATE);
                state = 2;
            } else {
                state = ERROR_STATE;
            }
            break;
            // States to handle skip comments follow.
        case 2:
            if (isblank(lexer->lookahead)) {
                NEXT(false, ERROR_STATE);
            } else {
                state = 3;
            }
            break;
        case 3:
            if (lexer->lookahead == 'e') {
                state = 4;
                NEXT(false, ERROR_STATE);
            } else {
                state = ERROR_STATE;
            }
            break;
        case 4:
            if (lexer->lookahead == 'l') {
                state = 5;
                NEXT(false, ERROR_STATE);
            } else {
                state = ERROR_STATE;
            }
            break;
        case 5:
            if (lexer->lookahead == 'p') {
                state = 6;
                NEXT(false, ERROR_STATE);
            } else {
                state = ERROR_STATE;
            }
            break;
        case 6:
            if (lexer->lookahead == 'i') {
                state = 7;
                NEXT(false, ERROR_STATE);
            } else {
                state = ERROR_STATE;
            }
            break;
        case 7:
            if (lexer->lookahead == ':') {
                state = 8;
                NEXT(false, ERROR_STATE);
            } else {
                state = ERROR_STATE;
            }
            break;
        case 8:
            if (lexer->lookahead == 's') {
                state = 9;
                NEXT(false, ERROR_STATE);
            } else {
                state = ERROR_STATE;
            }
            break;
        case 9:
            if (lexer->lookahead == 'k') {
                state = 10;
                NEXT(false, ERROR_STATE);
            } else {
                state = ERROR_STATE;
            }
            break;
        case 10:
            if (lexer->lookahead == 'i') {
                state = 11;
                NEXT(false, ERROR_STATE);
            } else {
                state = ERROR_STATE;
            }
            break;
        case 11:
            if (lexer->lookahead == 'p') {
                state = 12;
                NEXT(false, END_STATE);
            } else {
                state = ERROR_STATE;
            }
            break;
        case 12:
            if (lexer->lookahead == '\n') {
                state = END_STATE;
            } else if (isblank(lexer->lookahead)) {
                blanks++;
                NEXT(false, END_STATE);
            } else if (blanks > 0) {
                state = 13;
            } else {
                state = ERROR_STATE;
            }
            break;
        case 13:
            if (isdigit(lexer->lookahead)) {
                skip_lines *= 10;
                skip_lines += lexer->lookahead - '0';
                NEXT(false, END_STATE);
            } else {
                lexer->result_symbol = SKIP_COMMENT;
                state = 14;
            }
            break;
        case 14:
            if (lexer->lookahead != '\n' || skip_lines-- > 0) {
                NEXT(false, END_STATE);
            } else {
                state = END_STATE;
            }
            break;
        }
    }

    return (state != ERROR_STATE);
}
