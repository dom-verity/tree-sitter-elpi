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
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

enum TokenType {
    SKIP_COMMENT_HEAD,
    SKIP_COMMENT_LINE,
    BLOCK_COMMENT_LINE,
    END_OF_FILE,
    END_OF_LINE,
    WHITESPACE
};

#define NEXT(ws, eof_state)                             \
    lexer->advance(lexer, ws);                          \
    if (lexer->eof(lexer)) { state = eof_state; }

#define min(a,b)                                \
    ({ __auto_type _a = (a);                    \
        __auto_type _b = (b);                   \
        _a < _b ? _a : _b; })

bool eolp(int32_t ch) {
    return (ch == '\r' || ch == '\n');
}

typedef struct {
    unsigned line_no;
    unsigned skip_header_line;
    unsigned lines_to_skip;
} scanner_memo;

scanner_memo stored_state = {0,0,0};

void * tree_sitter_elpi_external_scanner_create() {
    // Return heap allocated block big enough to store a scanner_memo object.
    return malloc(sizeof(scanner_memo));
}

void tree_sitter_elpi_external_scanner_destroy(void *payload) {
    free(payload);
}

unsigned tree_sitter_elpi_external_scanner_serialize(void *payload,
                                                     void *buffer) {
    memcpy(buffer, &stored_state, sizeof(scanner_memo));
    return sizeof(scanner_memo);
}

void tree_sitter_elpi_external_scanner_deserialize(void *payload,
                                                   const char *buffer,
                                                   unsigned length) {
    memcpy(&stored_state, buffer, min(length, sizeof(scanner_memo)));
}

const int START_STATE = 1;
const int ERROR_STATE = -1;
const int END_STATE = 100;

// Maximum number of digits in the number specified in a skip comment
const int MAX_DIGITS = 5;

bool tree_sitter_elpi_external_scanner_scan(void *payload,
                                            TSLexer *lexer,
                                            const bool *valid_symbols) {
    bool nonempty = false;
    int digits = 0;
    int state = START_STATE;

    while (state != END_STATE && state != ERROR_STATE) {
        switch (state) {
        case 1:
            if (lexer->eof(lexer) && valid_symbols[END_OF_FILE]) {
                if (valid_symbols[END_OF_FILE]) {
                    lexer->result_symbol = END_OF_FILE;
                    state = END_STATE;
                } else {
                    state = ERROR_STATE;
                }
            } else if (lexer->lookahead == '\n') {
                if (valid_symbols[END_OF_LINE]) {
                    lexer->result_symbol = END_OF_LINE;
                    stored_state.line_no++;
                    NEXT(false, END_STATE);
                    state = END_STATE;
                } else {
                    state = ERROR_STATE;
                }
            } else if (isspace(lexer->lookahead)) {
                lexer->result_symbol = WHITESPACE;
                int last = lexer->lookahead;
                lexer->advance(lexer, false);
                if (lexer->eof(lexer)) {
                    state = valid_symbols[WHITESPACE] ? END_STATE : ERROR_STATE;
                } else if (last == '\r' && lexer->lookahead == '\n') {
                    if (valid_symbols[END_OF_LINE]) {
                        lexer->result_symbol = END_OF_LINE;
                        stored_state.line_no++;
                        NEXT(false, END_STATE);
                        state = END_STATE;
                    } else {
                        state = ERROR_STATE;
                    }
                } else if (valid_symbols[WHITESPACE]) {
                    lexer->mark_end(lexer);   // For extra char of lookahead.
                    state = 17;
                } else {
                    state = ERROR_STATE;
                }
            } else if (lexer->lookahead == '%' &&
                       valid_symbols[SKIP_COMMENT_HEAD]) {
                lexer->result_symbol = SKIP_COMMENT_HEAD;
                NEXT(false, ERROR_STATE);
                state = 2;
            } else if (stored_state.lines_to_skip > 0 &&
                       valid_symbols[SKIP_COMMENT_LINE]) {
                lexer->result_symbol = SKIP_COMMENT_LINE;
                state = 15;
            } else if (valid_symbols[BLOCK_COMMENT_LINE]) {
                lexer->result_symbol = BLOCK_COMMENT_LINE;
                lexer->mark_end(lexer); // Need to peek an extra character
                state = 16;
            } else {
                state = ERROR_STATE;
            }
            break;
        case 2: // States to handle skip comment headers follow.
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
                stored_state.lines_to_skip = 0;
                state = 12;
                NEXT(false, END_STATE);
            } else {
                state = ERROR_STATE;
            }
            break;
        case 12:
            if (eolp(lexer->lookahead)) {
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
            if (isdigit(lexer->lookahead) && digits < MAX_DIGITS) {
                stored_state.lines_to_skip *= 10;
                stored_state.lines_to_skip += lexer->lookahead - '0';
                digits++;
                NEXT(false, END_STATE);
            } else {
                state = 14;
            }
            break;
        case 14:
            if (eolp(lexer->lookahead)) {
                state = END_STATE;
            } else {
                NEXT(false, END_STATE);
            }
            break;
        case 15: // Process skip comment line
            if (eolp(lexer->lookahead)) {
                state = nonempty ? END_STATE : ERROR_STATE;
            } else if (!nonempty) {
                nonempty = true;
                stored_state.lines_to_skip--;
                NEXT(false, END_STATE);
            } else {
                NEXT(false, END_STATE);
            }
            break;
        case 16: // Process block comment line
            if (lexer->lookahead == '*') {
                lexer->advance(lexer, false);
                if (lexer->eof(lexer)) {
                    lexer->mark_end(lexer);
                    state = END_STATE;
                } else if (lexer->lookahead == '/') {
                    state = nonempty ? END_STATE : ERROR_STATE;
                } else {
                    nonempty = true;
                    lexer->mark_end(lexer);
                }
            } else if (eolp(lexer->lookahead)) {
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
        case 17:  // Process white space
            if (lexer->lookahead == '\r') {
                lexer->advance(lexer, false);
                if (lexer->eof(lexer)) {
                    lexer->mark_end(lexer);
                    state = END_STATE;
                } else if (lexer->lookahead == '\n'){
                    state = END_STATE;
                } else {
                    lexer->mark_end(lexer);
                }
            } else if (isspace(lexer->lookahead) && lexer->lookahead != '\n') {
                lexer->advance(lexer, false);
                lexer->mark_end(lexer);
            } else {
                state = END_STATE;
            }
            break;
    }

    return (state != ERROR_STATE);
}
