#include "tree_sitter/parser.h"

enum TokenType {
  // sequence of "normal" characters in single line string with one pound sign
  SL1_STRING_CHARS,
  // sequence of "normal" characters in single line string with two pound signs
  SL2_STRING_CHARS,
  // sequence of "normal" characters in single line string with three pound signs
  SL3_STRING_CHARS,
  // sequence of "normal" characters in single line string with four pound signs
  SL4_STRING_CHARS,
  // sequence of "normal" characters in single line string with five pound signs
  SL5_STRING_CHARS,
  // sequence of "normal" characters in single line string with six pound signs
  SL6_STRING_CHARS,
  // sequence of "normal" characters in multiline string without pound sign
  ML_STRING_CHARS,
  // sequence of "normal" characters in multiline string with one pound sign
  ML1_STRING_CHARS,
  // sequence of "normal" characters in multiline string with two pound signs
  ML2_STRING_CHARS,
  // sequence of "normal" characters in multiline string with three pound signs
  ML3_STRING_CHARS,
  // sequence of "normal" characters in multiline string with four pound signs
  ML4_STRING_CHARS,
  // sequence of "normal" characters in multiline string with five pound signs
  ML5_STRING_CHARS,
  // sequence of "normal" characters in multiline string with six pound signs
  ML6_STRING_CHARS,
  // `[` as used in subscript expressions
  OPEN_SQUARE_BRACKET,
  // `[` as used in object entries
  OPEN_ENTRY_BRACKET,
};

void *tree_sitter_pkl_external_scanner_create() { return NULL; }

void tree_sitter_pkl_external_scanner_destroy(void *p) {}

unsigned tree_sitter_pkl_external_scanner_serialize(void *p, char *buffer) { return 0; }

void tree_sitter_pkl_external_scanner_deserialize(void *p, const char *b, unsigned n) {}

static void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

static bool parse_slx_string_chars(TSLexer *lexer, int num_pounds) {
  bool has_content = false;
  switch(num_pounds) {
    case 1:
      lexer->result_symbol = SL1_STRING_CHARS;
      break;
    case 2:
      lexer->result_symbol = SL2_STRING_CHARS;
      break;
    case 3:
      lexer->result_symbol = SL3_STRING_CHARS;
      break;
    case 4:
      lexer->result_symbol = SL4_STRING_CHARS;
      break;
    case 5:
      lexer->result_symbol = SL5_STRING_CHARS;
      break;
    case 6:
    default:
      lexer->result_symbol = SL6_STRING_CHARS;
      break;
  }

  while (true) {
    next_iter:
    switch (lexer->lookahead) {
      case '"':
      case '\\':
        lexer->mark_end(lexer);
        advance(lexer);
        for (int i = 0; i < num_pounds; i++) {
          if (lexer->lookahead != '#') {
            has_content = true;
            goto next_iter;
          }
          advance(lexer);
        }
        return has_content;
      case '\n':
      case '\r':
      case 0:
        lexer->mark_end(lexer);
        return has_content;
      default:
        has_content = true;
        advance(lexer);
    }
  }
}

static bool parse_ml_string_chars(TSLexer *lexer) {
  bool has_content = false;
  lexer->result_symbol = ML_STRING_CHARS;

  while (true) {
    switch (lexer->lookahead) {
      case '"':
        lexer->mark_end(lexer);
        advance(lexer);
        if (lexer->lookahead == '"') {
          advance(lexer);
          if (lexer->lookahead == '"') {
            return has_content;
          }
        }
        has_content = true;
        break;
      case '\\':
      case 0:
        lexer->mark_end(lexer);
        return has_content;
      default:
        has_content = true;
        advance(lexer);
    }
  }
}

static bool parse_mlx_string_chars(TSLexer *lexer, int num_pounds) {
  bool has_content = false;
  switch(num_pounds) {
    case 1:
      lexer->result_symbol = ML1_STRING_CHARS;
      break;
    case 2:
      lexer->result_symbol = ML2_STRING_CHARS;
      break;
    case 3:
      lexer->result_symbol = ML3_STRING_CHARS;
      break;
    case 4:
      lexer->result_symbol = ML4_STRING_CHARS;
      break;
    case 5:
      lexer->result_symbol = ML5_STRING_CHARS;
      break;
    case 6:
    default:
      lexer->result_symbol = ML6_STRING_CHARS;
      break;
  }

  while (true) {
    next_iter:
    switch (lexer->lookahead) {
      case '"': {
        lexer->mark_end(lexer);
        int quote_count = 0;
        do {
          quote_count += 1;
          advance(lexer);
        } while (lexer->lookahead == '"');
        if (quote_count < 3) {
          has_content = true;
          break;
        }
        for (int i = 0; i < num_pounds; i++) {
          if (lexer->lookahead != '#') {
            has_content = true;
            goto next_iter;
          }
          advance(lexer);
        }
        return has_content;
      }
      case '\\':
        lexer->mark_end(lexer);
        advance(lexer);
        for (int i = 0; i < num_pounds; i++) {
          if (lexer->lookahead != '#') {
            has_content = true;
            goto next_iter;
          }
          advance(lexer);
        }
        return has_content;
      case 0:
        lexer->mark_end(lexer);
        return has_content;
      default:
        has_content = true;
        advance(lexer);
    }
  }
}

bool parse_square_bracket_variant(TSLexer *lexer, bool open_square_bracket, bool open_entry_bracket) {
  while (
    lexer->lookahead == ' ' ||
    lexer->lookahead == '\t' ||
    (open_entry_bracket && (
      lexer->lookahead == ';' ||
      lexer->lookahead == '\n'
    ))
  ) {
    open_square_bracket = open_square_bracket && (lexer->lookahead != '\n' && lexer->lookahead != ';');
    lexer->advance(lexer, true);
  }
  if (lexer->lookahead == '[') {
    lexer->result_symbol = open_square_bracket ? OPEN_SQUARE_BRACKET : OPEN_ENTRY_BRACKET;
    lexer->advance(lexer, false);
    if (lexer->lookahead != '[') {
        lexer->mark_end(lexer);
        return true;
    }
  }
  return false;
}

bool tree_sitter_pkl_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  bool sl1 = valid_symbols[SL1_STRING_CHARS];
  bool sl2 = valid_symbols[SL2_STRING_CHARS];
  bool sl3 = valid_symbols[SL3_STRING_CHARS];
  bool sl4 = valid_symbols[SL4_STRING_CHARS];
  bool sl5 = valid_symbols[SL5_STRING_CHARS];
  bool sl6 = valid_symbols[SL6_STRING_CHARS];
  bool ml = valid_symbols[ML_STRING_CHARS];
  bool ml1 = valid_symbols[ML1_STRING_CHARS];
  bool ml2 = valid_symbols[ML2_STRING_CHARS];
  bool ml3 = valid_symbols[ML3_STRING_CHARS];
  bool ml4 = valid_symbols[ML4_STRING_CHARS];
  bool ml5 = valid_symbols[ML5_STRING_CHARS];
  bool ml6 = valid_symbols[ML6_STRING_CHARS];
  bool osb = valid_symbols[OPEN_SQUARE_BRACKET];
  bool oeb = valid_symbols[OPEN_ENTRY_BRACKET];

  if (sl1 && sl2 && sl3 && sl4 && sl5 && sl6 && ml && ml1 && ml2 && ml3 && ml4 && ml5 && ml6 && osb && oeb) {
    // error recovery mode -> don't match any string chars
    return false;
  }

  if (ml) {
    return parse_ml_string_chars(lexer);
  }
  if (sl1) {
    return parse_slx_string_chars(lexer, 1);
  }
  if (ml1) {
    return parse_mlx_string_chars(lexer, 1);
  }
  if (sl2) {
    return parse_slx_string_chars(lexer, 2);
  }
  if (ml2) {
    return parse_mlx_string_chars(lexer, 2);
  }
  if (sl3) {
    return parse_slx_string_chars(lexer, 3);
  }
  if (ml3) {
    return parse_mlx_string_chars(lexer, 3);
  }
  if (sl4) {
    return parse_slx_string_chars(lexer, 4);
  }
  if (ml4) {
    return parse_mlx_string_chars(lexer, 4);
  }
  if (sl5) {
    return parse_slx_string_chars(lexer, 5);
  }
  if (ml5) {
    return parse_mlx_string_chars(lexer, 5);
  }
  if (sl6) {
    return parse_slx_string_chars(lexer, 6);
  }
  if (ml6) {
    return parse_mlx_string_chars(lexer, 6);
  }
  if (osb || oeb) {
    return parse_square_bracket_variant(lexer, osb, oeb);
  }
  return false;
}

