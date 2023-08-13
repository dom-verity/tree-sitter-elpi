/**
 * Tree sitter grammar for the ELPI (https://github.com/LPCIC/elpi)
 * dialect of λProlog (https://www.lix.polytechnique.fr/~dale/lProlog/).
 *
 * File:            C scanner, implements skip comments and EOF.
 *
 * Original Author: Dominic Verity <dominic.verity@anu.edu.au>
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

#include "tree_sitter/parser.h"
#include <wchar.h>

enum TokenType {
    SKIP_COMMENT,
    END_OF_FILE,
    BLOCK_COMMENT_LINE
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

const int START_STATE = 1;
const int ERROR_STATE = -1;
const int END_STATE = 100;

#define NEXT(ws, eof_state)\
    lexer->advance(lexer, ws);\
    if (lexer->eof(lexer)) { state = eof_state; }

bool tree_sitter_elpi_external_scanner_scan(void *payload,
                                            TSLexer *lexer,
                                            const bool *valid_symbols) {
    bool nonempty = false;
    int skip_lines = 0;
    int state = START_STATE;

    while (state != END_STATE && state != ERROR_STATE) {
        switch (state) {
        case 1:
            if (lexer->eof(lexer) && valid_symbols[END_OF_FILE]) {
                lexer->result_symbol = END_OF_FILE;
                state = END_STATE;
            } else if (lexer->lookahead == '%' && valid_symbols[SKIP_COMMENT]) {
                lexer->result_symbol = SKIP_COMMENT;
                NEXT(false, ERROR_STATE);
                state = 2;
            } else if (!lexer->eof(lexer) && !isspace(lexer->lookahead) &&
                       valid_symbols[BLOCK_COMMENT_LINE]) {
                lexer->result_symbol = BLOCK_COMMENT_LINE;
                lexer->mark_end(lexer); // Need to peek an extra character
                state = 15;
            } else {
                state = ERROR_STATE;
            }
            break;
        case 2: // States to handle skip comments follow.
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
                nonempty = true;
                NEXT(false, END_STATE);
            } else if (nonempty) {
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
        case 15: // States to handle block comment lines follow
            if (lexer->lookahead == '*') {
                lexer->advance(lexer, false);
                if (lexer->eof(lexer)) {
                    lexer->mark_end(lexer);
                    state = END_STATE;
                } else {
                    state = 16; // Do look-ahead to check for /
                }
            } else if (lexer->lookahead == '\n' || lexer->lookahead == '\r') {
                lexer->mark_end(lexer);
                state = nonempty ? END_STATE : ERROR_STATE;
            } else {
                nonempty = true;
                lexer->advance(lexer, false);
                lexer->mark_end(lexer);
                if (lexer->eof(lexer)) {
                    state = END_STATE;
                }
            }
            break;
        case 16:
            if (lexer->lookahead == '/') {
                state = nonempty ? END_STATE : ERROR_STATE;
            } else {
                lexer->mark_end(lexer);
                state = 15;
            }
            break;
        }
    }

    return (state != ERROR_STATE);
}
