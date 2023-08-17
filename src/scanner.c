/**
 * Tree sitter grammar for the ELPI (https://github.com/LPCIC/elpi)
 * dialect of λProlog (https://www.lix.polytechnique.fr/~dale/lProlog/).
 *
 * File:            C scanner: Implements the tricky parts of ELPI syntax
 **                 like skip comments. Also implements an EOF token.
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

#define NEXT(commit, eof_state)                           \
    lexer->advance(lexer,false);                          \
    if (commit) {lexer->mark_end(lexer);}                 \
    if (lexer->eof(lexer)) { state = eof_state; break; };

#define min(a,b)                                \
    ({ __auto_type _a = (a);                    \
        __auto_type _b = (b);                   \
        _a < _b ? _a : _b; })

typedef struct {
    unsigned lines_to_skip;
} scanner_memo;

scanner_memo stored_state = {0,0};

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

const unsigned START_STATE = 1;
const unsigned ERROR_STATE = -1;
const unsigned END_STATE = 100;

// Maximum number of digits in the number specified in a skip comment
const int MAX_DIGITS = 5;

int current = 0;
bool eof = false;

void advance(TSLexer *lexer) {
    current = lexer->lookahead;
    eof = lexer->eof(lexer);
    if (!eof) {
        lexer->advance(lexer, false);
        if (current == '\r' && lexer->lookahead == '\n') {
            current = '\n';
            lexer->advance(lexer,false);
        }
    }
}

bool tree_sitter_elpi_external_scanner_scan(void *payload,
                                            TSLexer *lexer,
                                            const bool *valid_symbols) {
    unsigned digits = 0;
    unsigned state = START_STATE;
    bool nonempty = false;

    unsigned lines_to_skip = 0;

    lexer->mark_end(lexer);         // Manual end mark movement for look-ahead
    advance(lexer);                 // Get first character

    while (state != END_STATE && state != ERROR_STATE) {
        switch (state) {
        case 1:
            if (eof) {
                if (valid_symbols[END_OF_FILE]) {
                    lexer->result_symbol = END_OF_FILE;
                    state = END_STATE;
                } else {
                    state = ERROR_STATE;
                }
            } else if (current == '\n') {
                if (valid_symbols[END_OF_LINE]) {
                    lexer->result_symbol = END_OF_LINE;
                    if (stored_state.lines_to_skip > 0) {
                        stored_state.lines_to_skip--;
                    }
                    lexer->mark_end(lexer);
                    state = END_STATE;
                } else {
                    state = ERROR_STATE;
                }
            } else if (isspace(current)) {
                if (valid_symbols[WHITESPACE]) {
                    lexer->mark_end(lexer);
                    lexer->result_symbol = WHITESPACE;
                    state = 17;
                } else {
                    state = ERROR_STATE;
                }
            } else if (stored_state.lines_to_skip > 0 &&
                       valid_symbols[SKIP_COMMENT_LINE] &&
                       !eof && !isspace(current)) {
                lexer->mark_end(lexer);
                lexer->result_symbol = SKIP_COMMENT_LINE;
                state = 16;
            } else if (valid_symbols[BLOCK_COMMENT_LINE] &&
                       !eof && !isspace(current)) {
                lexer->result_symbol = BLOCK_COMMENT_LINE;
                if (current == '*') {
                    state = 19;
                } else {
                    lexer->mark_end(lexer);
                    state = 18;
                }
            } else if (current == '%' &&
                       valid_symbols[SKIP_COMMENT_HEAD]) {
                lexer->mark_end(lexer);
                lexer->result_symbol = SKIP_COMMENT_HEAD;
                state = 2;
            } else {
                state = ERROR_STATE;
            }
            break;
        case 2: // States to handle skip comment headers follow.
            advance(lexer);
            if (eof || current == '\n') {
                state = ERROR_STATE;
            } else if (current == 'e') {
                state = 4;
            } else if (!isspace(current)) {
                state = ERROR_STATE;
            }
            break;
        case 4:
            advance(lexer);
            if (current == 'l') {
                state = 5;
            } else {
                state = ERROR_STATE;
            }
            break;
        case 5:
            advance(lexer);
            if (current == 'p') {
                state = 6;
            } else {
                state = ERROR_STATE;
            }
            break;
        case 6:
            advance(lexer);
            if (current == 'i') {
                state = 7;
            } else {
                state = ERROR_STATE;
            }
            break;
        case 7:
            advance(lexer);
            if (current == ':') {
                state = 8;
            } else {
                state = ERROR_STATE;
            }
            break;
        case 8:
            advance(lexer);
            if (current == 's') {
                state = 9;
            } else {
                state = ERROR_STATE;
            }
            break;
        case 9:
            advance(lexer);
            if (current == 'k') {
                state = 10;
            } else {
                state = ERROR_STATE;
            }
            break;
        case 10:
            advance(lexer);
            if (current == 'i') {
                state = 11;
            } else {
                state = ERROR_STATE;
            }
            break;
        case 11:
            advance(lexer);
            if (current == 'p') {
                lines_to_skip = 0;
                lexer->mark_end(lexer);
                state = 12;
            } else {
                state = ERROR_STATE;
            }
            break;
        case 12:
            advance(lexer);
            if (eof || current == '\n') {
                stored_state.lines_to_skip = lines_to_skip + 1;
                state = END_STATE;
            } else if (isblank(current)) {
                lexer->mark_end(lexer);
                state = 13;
            } else {
                state = ERROR_STATE;
            }
            break;
        case 13:
            advance(lexer);
            if (eof || current == '\n') {
                state = END_STATE;
            } else if (isdigit(current)) {
                lexer->mark_end(lexer);
                lines_to_skip = current - '0';
                digits = 1;
                state = 14;
            } else if (isblank(current)) {
                lexer->mark_end(lexer);
            } else {
                state = ERROR_STATE;
            }
            break;
        case 14:
            advance(lexer);
            if (eof || current == '\n') {
                stored_state.lines_to_skip = lines_to_skip + 1;
                state = END_STATE;
            } else if (isdigit(current) && digits < MAX_DIGITS) {
                lexer->mark_end(lexer);
                lines_to_skip *= 10;
                lines_to_skip += current - '0';
                digits++;
            } else {
                state = 15;
            }
            break;
        case 15:
            advance(lexer);
            if (eof || current == '\n') {
                stored_state.lines_to_skip = lines_to_skip + 1;
                state = END_STATE;
            } else {
                lexer->mark_end(lexer);
            }
            break;
        case 16: // Process skip comment line
            advance(lexer);
            if (eof || current == '\n') {
                state = END_STATE;
            } else {
                lexer->mark_end(lexer);
            }
            break;
        case 17:  // Process white space
            advance(lexer);
            if (eof || current == '\n') {
                state = END_STATE;
            } else if (isspace(current)) {
                lexer->mark_end(lexer);
            } else {
                state = END_STATE;
            }
            break;
        case 18: // States to process block comment line.
            advance(lexer);
            if (eof || current == '\n') {
                state = nonempty ? END_STATE : ERROR_STATE;
            } else if (current == '*') {
                state = 19;
            } else {
                nonempty = true;
                lexer->mark_end(lexer);
            }
            break;
        case 19: // Current character is '*'
            if (lexer->lookahead == '/') { // peek ahead.
                state = nonempty ? END_STATE : ERROR_STATE;
            } else {
                nonempty = true;
                lexer->mark_end(lexer);
                state = 18;
            }
            break;
        }
    }
    return (state != ERROR_STATE);
}
