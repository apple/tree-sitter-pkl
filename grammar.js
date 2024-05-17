/**
 * Copyright © 2024 Apple Inc. and the Pkl project authors. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
/// <reference path="node_modules/tree-sitter-cli/dsl.d.ts" />

/**
 * Operator precedences
 */
const PREC = {
  NON_NULL: 21,
  NEG: 20,
  NOT: 19,
  EXP: 18,
  MUL: 17,
  ADD: 16,
  REL: 15,
  IS: 14,
  AS: 13,
  EQ: 12,
  AND: 11,
  OR: 10,
  PIPE: 9,
  COALESCE: 8,

  IF: -4,
  LET: -5,
  THROW: -6,
  TRACE: -7,
  READ: -8,
  READ_OR_NULL: -9,
  READ_GLOB: -10,
  FUN: -11,

  NULLABLE_TYPE: 5,
  FUN_TYPE: -5,
  UNION_TYPE: -6,
  DEFAULT_TYPE: -7,

  VAR_OBJ_LITERAL: 2,
  OBJ_LITERAL: 1,
  OBJ_MEMBER: -1
};

const decimalLiteral = seq(/\d/, /[\d_]*/);

module.exports = grammar({
  name: 'pkl',

  precedences: $ => [
    ["OBJ_ENTRY", "SUBSCRIPT"]
  ],

  externals: $ => [
    $._sl1_string_chars,
    $._sl2_string_chars,
    $._sl3_string_chars,
    $._sl4_string_chars,
    $._sl5_string_chars,
    $._sl6_string_chars,
    $._ml_string_chars,
    $._ml1_string_chars,
    $._ml2_string_chars,
    $._ml3_string_chars,
    $._ml4_string_chars,
    $._ml5_string_chars,
    $._ml6_string_chars,
    $._open_square_bracket,
    $._open_entry_bracket,
  ],

  extras: $ => [
    $.lineComment,
    $.docComment,
    $.blockComment,
    /[ \t\f\r\n;]/
  ],

  word: $ => $.identifier,

  conflicts: $ => [
    // in ANTLR we deal with these by not allowing a newline or semicolon before subscript
    [$.objectProperty, $.subscriptExpr],
    [$.objectMethod, $.subscriptExpr],
    [$.objectEntry, $.subscriptExpr],
    [$.objectPredicate, $.subscriptExpr],

    // these should be fixable in some other way (perhaps with prec)
    [$.propertyCallExpr, $.methodCallExpr],
    [$.variableExpr, $.methodCallExpr],
    [$.variableExpr, $.typedIdentifier],

    // not sure what this one is about
    [$.objectElement, $._expr],

    [$.qualifiedIdentifier],
    [$.type]
  ],

  rules: {
    module: $ => seq(
      optional($.moduleHeader),
      repeat(choice($.importClause, $.importGlobClause)),
      repeat($._moduleMember)
    ),

    moduleHeader: $ => seq(
      repeat($.annotation),
      choice(
        seq($.moduleClause, optional($.extendsOrAmendsClause)),
        $.extendsOrAmendsClause
      ),
    ),

    moduleClause: $ => seq(
      repeat($.modifier),
      "module",
      $.qualifiedIdentifier
    ),

    extendsOrAmendsClause: $ => seq(
      choice("extends", "amends"),
      $.stringConstant
    ),

    importClause: $ => seq(
      "import",
      $.stringConstant,
      optional(seq("as", $.identifier))
    ),

    importGlobClause: $ => seq(
      "import*",
      $.stringConstant,
      optional(seq("as", $.identifier))
    ),

    _moduleMember: $ => choice(
      $.clazz,
      $.typeAlias,
      $.classProperty,
      $.classMethod
    ),

    clazz: $ => seq(
      repeat($.annotation),
      repeat($.modifier),
      "class",
      $.identifier,
      optional($.typeParameterList),
      optional($.classExtendsClause),
      optional($.classBody)
    ),

    classExtendsClause: $ => seq(
      "extends",
      $.qualifiedIdentifier,
      optional($.typeArgumentList)
    ),

    classBody: $ => seq(
      "{",
      repeat(choice($.classProperty, $.classMethod)),
      "}"
    ),

    typeAlias: $ => seq(
      repeat($.annotation),
      repeat($.modifier),
      "typealias",
      $.identifier,
      optional($.typeParameterList),
      "=",
      $.type
    ),

    classProperty: $ => seq(
      repeat($.annotation),
      repeat($.modifier),
      $.identifier,
      choice(
        $.typeAnnotation,
        seq(
          optional($.typeAnnotation),
          choice(
            seq("=", $._expr),
            repeat1($.objectBody)
          )
        )
      )
    ),

    classMethod: $ => seq(
      repeat($.annotation),
      $.methodHeader,
      optional(seq("=", $._expr))
    ),

    methodHeader: $ => seq(
      repeat($.modifier),
      "function",
      $.identifier,
      optional($.typeParameterList),
      $.parameterList,
      optional($.typeAnnotation)
    ),

    annotation: $ => seq(
      "@",
      $.qualifiedIdentifier,
      optional($.objectBody)
    ),

    objectBody: $ => seq(
      "{",
      optional($.objectBodyParameters),
      repeat($._objectMember),
      "}"
    ),

    _objectMember: $ => prec(PREC.OBJ_MEMBER, choice(
      $.objectProperty,
      $.objectMethod,
      $.objectEntry,
      $.objectElement,
      $.objectPredicate,
      $.forGenerator,
      $.whenGenerator,
      $.objectSpread,
    )),

    objectProperty: $ => seq(
      repeat($.modifier),
      $.identifier,
      choice(
        seq(
          optional($.typeAnnotation),
          "=",
          $._expr
        ),
        repeat1($.objectBody)
      )
    ),

    objectMethod: $ => seq($.methodHeader, "=", $._expr),

    objectEntry: $ => seq(
      alias($._open_entry_bracket, "["),
      field("key", $._expr),
      "]",
      choice(
        seq("=", field("value", $._expr)),
        field("value", repeat1($.objectBody))
      )
    ),

    objectElement: $ => choice($.variableExpr, $._expr2),

    objectPredicate: $ => seq(
      "[[",
      field("condition", $._expr),
      "]]",
      choice(
        seq("=", field("consequence", $._expr)),
        field("consequence", repeat1($.objectBody))
      )
    ),

    forGenerator: $ => seq(
      "for",
      "(",
      $.typedIdentifier,
      optional(seq(
        ",",
        $.typedIdentifier
      )),
      "in",
      $._expr,
      ")",
      $.objectBody,
    ),

    whenGenerator: $ => seq(
      "when",
      "(",
      field("condition", $._expr),
      ")",
      field("consequence", $.objectBody),
      optional(
        seq("else", field("alternative", $.objectBody))
      )
    ),

    objectSpread: $ => seq(
      choice(
        "...",
        "...?"
      ),
      $._expr
    ),

    objectBodyParameters: $ => seq(
      commaSep1($.typedIdentifier),
      "->"
    ),

    typeAnnotation: $ => seq(":", $.type),

    type: $ => choice(
      "unknown",
      "nothing",
      $.stringConstant,
      seq($.qualifiedIdentifier, optional($.typeArgumentList)),
      seq("(", $.type, ")"),
      prec(PREC.NULLABLE_TYPE, seq($.type, "?")),
      seq($.type, "(", commaSep1($._expr), ")"),
      prec.left(PREC.UNION_TYPE, seq($.type, "|", $.type)),
      prec(PREC.DEFAULT_TYPE, seq("*",field("default", $.type))),
      prec(PREC.FUN_TYPE, seq("(", commaSep($.type), ")", "->", $.type))
    ),

    typeArgumentList: $ => seq(
      "<",
      commaSep1($.type),
      ">"
    ),

    typeParameterList: $ => seq(
      "<",
      commaSep1($.typeParameter),
      ">"
    ),

    typeParameter: $ => seq(
      optional(choice("in", "out")),
      $.identifier
    ),

    parameterList: $ => seq(
      '(',
      commaSep($.typedIdentifier),
      ')'
    ),

    argumentList: $ => seq(
      '(',
      commaSep($._expr),
      ')'
    ),

    modifier: $ => choice(
      "external",
      "abstract",
      "open",
      "local",
      "hidden",
      "fixed",
      "const"
    ),

    _expr: $ => choice(
      $.variableExpr,
      $.variableObjectLiteral,
      $._expr2
    ),

    variableObjectLiteral: $ => prec(PREC.VAR_OBJ_LITERAL, seq(
      $.identifier,
      $.objectBody
    )),

    _expr2: $ => choice(
      $.parenthesizedExpr,
      $.thisExpr,
      $.outerExpr,
      $.moduleExpr,
      $.nullLiteral,
      $.trueLiteral,
      $.falseLiteral,
      $.intLiteral,
      $.floatLiteral,
      $.slStringLiteral,
      $.mlStringLiteral,
      $.newExpr,
      $.objectLiteral,
      $.methodCallExpr,
      $.propertyCallExpr,
      $.subscriptExpr,
      $.unaryExpr,
      $.binaryExpr,
      $.binaryExprRightAssoc,
      $.isExpr,
      $.asExpr,
      $.ifExpr,
      $.letExpr,
      $.throwExpr,
      $.traceExpr,
      $.readExpr,
      $.readOrNullExpr,
      $.readGlobExpr,
      $.importExpr,
      $.importGlobExpr,
      $.functionLiteral
    ),

    parenthesizedExpr: $ => seq("(", $._expr, ")"),

    thisExpr: $ => "this",

    outerExpr: $ => "outer",

    moduleExpr: $ => "module",

    variableExpr: $ => $.identifier,

    nullLiteral: $ => "null",

    trueLiteral: $ => "true",

    falseLiteral: $ => "false",

    intLiteral: $ => {
      return token(choice(
        decimalLiteral,
        seq('0x', /[\da-fA-F]/, /[\da-fA-F_]*/),
        seq('0b', /[0-1]/, /[0-1_]*/),
        seq('0o', /[0-7]/, /[0-7_]*/)
      ))
    },

    floatLiteral: $ => {
      const exponent = seq(choice('e', 'E'), optional(choice('+', '-')), decimalLiteral)
      return token(choice(
        seq(optional(decimalLiteral), '.', decimalLiteral, optional(exponent)),
        seq(decimalLiteral, exponent)
      ))
    },

    stringFragment: $ => choice(
      token.immediate(/[^"\\\n\r]+/),
      $.escapeSequence
    ),

    stringConstant: $ => choice(
      seq(
        '"',
        repeat($.stringFragment),
        '"'
      ),
      seq(
        '#"',
        repeat(choice(
          alias($._sl1_string_chars, $.stringFragment),
          alias($.escapeSequence1, $.escapeSequence),
        )),
        '"#'
      )
    ),

    slStringLiteral: $ => choice(
      seq(
        '"',
        repeat(choice(
          $.stringFragment,
          $.interpolationExpr,
        )),
        '"'
      ),
      seq(
        '#"',
        repeat(choice(
          alias($._sl1_string_chars, $.stringFragment),
          alias($.escapeSequence1, $.escapeSequence),
          alias($.interpolationExpr1, $.interpolationExpr)
        )),
        '"#'
      ),
      seq(
        '##"',
        repeat(choice(
          alias($._sl2_string_chars, $.stringFragment),
          alias($.escapeSequence2, $.escapeSequence),
          alias($.interpolationExpr2, $.interpolationExpr)
        )),
        '"##'
      ),
      seq(
        '###"',
        repeat(choice(
          alias($._sl3_string_chars, $.stringFragment),
          alias($.escapeSequence3, $.escapeSequence),
          alias($.interpolationExpr3, $.interpolationExpr)
        )),
        '"###'
      ),
      seq(
        '####"',
        repeat(choice(
          alias($._sl4_string_chars, $.stringFragment),
          alias($.escapeSequence4, $.escapeSequence),
          alias($.interpolationExpr4, $.interpolationExpr)
        )),
        '"####'
      ),
      seq(
        '#####"',
        repeat(choice(
          alias($._sl5_string_chars, $.stringFragment),
          alias($.escapeSequence5, $.escapeSequence),
          alias($.interpolationExpr5, $.interpolationExpr)
        )),
        '"#####'
      ),
      seq(
        '######"',
        repeat(choice(
          alias($._sl6_string_chars, $.stringFragment),
          alias($.escapeSequence6, $.escapeSequence),
          alias($.interpolationExpr6, $.interpolationExpr)
        )),
        '"######'
      ),
    ),

    mlStringLiteral: $ => choice(
      seq(
        '"""',
        repeat(choice(
          alias($._ml_string_chars, $.stringFragment),
          $.escapeSequence,
          $.interpolationExpr
        )),
        '"""'
      ),
      seq(
        '#"""',
        repeat(choice(
          alias($._ml1_string_chars, $.stringFragment),
          alias($.escapeSequence1, $.escapeSequence),
          alias($.interpolationExpr1, $.interpolationExpr)
        )),
        '"""#'
      ),
      seq(
        '##"""',
        repeat(choice(
          alias($._ml2_string_chars, $.stringFragment),
          alias($.escapeSequence2, $.escapeSequence),
          alias($.interpolationExpr2, $.interpolationExpr)
        )),
        '"""##'
      ),
      seq(
        '###"""',
        repeat(choice(
          alias($._ml3_string_chars, $.stringFragment),
          alias($.escapeSequence3, $.escapeSequence),
          alias($.interpolationExpr3, $.interpolationExpr)
        )),
        '"""###'
      ),
      seq(
        '####"""',
        repeat(choice(
          alias($._ml4_string_chars, $.stringFragment),
          alias($.escapeSequence4, $.escapeSequence),
          alias($.interpolationExpr4, $.interpolationExpr)
        )),
        '"""####'
      ),
      seq(
        '#####"""',
        repeat(choice(
          alias($._ml5_string_chars, $.stringFragment),
          alias($.escapeSequence5, $.escapeSequence),
          alias($.interpolationExpr5, $.interpolationExpr)
        )),
        '"""#####'
      ),
      seq(
        '######"""',
        repeat(choice(
          alias($._ml6_string_chars, $.stringFragment),
          alias($.escapeSequence6, $.escapeSequence),
          alias($.interpolationExpr6, $.interpolationExpr)
        )),
        '"""######'
      ),
    ),

    escapeSequence: $ => token.immediate(seq(
      '\\',
      choice(
        /[tnr\\"]/,
        /u\{[0-9a-fA-F]+\}/
      )
    )),

    escapeSequence1: $ => token.immediate(seq(
      '\\#',
      choice(
        /[tnr\\"]/,
        /u\{[0-9a-fA-F]+\}/
      )
    )),

    escapeSequence2: $ => token.immediate(seq(
      '\\##',
      choice(
        /[tnr\\"]/,
        /u\{[0-9a-fA-F]+\}/
      )
    )),

    escapeSequence3: $ => token.immediate(seq(
      '\\###',
      choice(
        /[tnr\\"]/,
        /u\{[0-9a-fA-F]+\}/
      )
    )),

    escapeSequence4: $ => token.immediate(seq(
      '\\####',
      choice(
        /[tnr\\"]/,
        /u\{[0-9a-fA-F]+\}/
      )
    )),

    escapeSequence5: $ => token.immediate(seq(
      '\\#####',
      choice(
        /[tnr\\"]/,
        /u\{[0-9a-fA-F]+\}/
      )
    )),

    escapeSequence6: $ => token.immediate(seq(
      '\\######',
      choice(
        /[tnr\\"]/,
        /u\{[0-9a-fA-F]+\}/
      )
    )),

    interpolationExpr: $ => seq(token.immediate("\\("), $._expr, ")"),

    interpolationExpr1: $ => seq(token.immediate("\\#("), $._expr, ")"),

    interpolationExpr2: $ => seq(token.immediate("\\##("), $._expr, ")"),

    interpolationExpr3: $ => seq(token.immediate("\\###("), $._expr, ")"),

    interpolationExpr4: $ => seq(token.immediate("\\####("), $._expr, ")"),

    interpolationExpr5: $ => seq(token.immediate("\\#####("), $._expr, ")"),

    interpolationExpr6: $ => seq(token.immediate("\\######("), $._expr, ")"),

    newExpr: $ => seq("new", optional($.type), $.objectBody),

    objectLiteral: $ => prec(PREC.OBJ_LITERAL, seq($._expr2, $.objectBody)),

    methodCallExpr: $ => seq(optional(seq(choice("super", field("object", $._expr)), choice(".", "?."))), field("method", $.identifier), field("args", $.argumentList)),

    propertyCallExpr: $ => seq(choice("super", field("object", $._expr)), choice(".", "?."), field("property", $.identifier)),

    subscriptExpr: $ => seq(choice("super", $._expr), alias($._open_square_bracket, "["), $._expr, "]"),

    unaryExpr: $ => choice(
      prec.left(PREC.NEG, seq($._expr, field("operator", '!!'))),
      prec.left(PREC.NEG, seq(field("operator", '-'), $._expr)),
      prec.left(PREC.NOT, seq(field("operator", '!'), $._expr)),
    ),

    binaryExprRightAssoc: $ => choice(...[
      ['**', PREC.EXP],
      ['??', PREC.COALESCE]
    ].map(([operator, precedence]) =>
      prec.right(precedence, seq($._expr, field("operator", operator), $._expr))
    )),

    binaryExpr: $ => choice(...[
      ['*', PREC.MUL],
      ['/', PREC.MUL],
      ['~/', PREC.MUL],
      ['%', PREC.MUL],
      ['+', PREC.ADD],
      ['-', PREC.ADD],
      ['<', PREC.REL],
      ['<=', PREC.REL],
      ['>=', PREC.REL],
      ['>', PREC.REL],
      ['==', PREC.EQ],
      ['!=', PREC.EQ],
      ['&&', PREC.AND],
      ['||', PREC.OR],
      ['|>', PREC.PIPE]
    ].map(([operator, precedence]) =>
      prec.left(precedence, seq(field("left", $._expr), field("operator", operator), field("right", $._expr)))
    )),

    isExpr: $ => prec(PREC.IS, seq($._expr, "is", $.type)),

    asExpr: $ => prec(PREC.IS, seq($._expr, "as", $.type)),

    ifExpr: $ => prec(PREC.IF, seq("if", "(", field("condition", $._expr), ")", field("consequence", $._expr), "else", field("alternative", $._expr))),

    letExpr: $ => prec(PREC.LET, seq("let", "(", $.typedIdentifier, "=", $._expr, ")", $._expr)),

    throwExpr: $ => prec(PREC.THROW, seq("throw", $._expr)),

    traceExpr: $ => prec(PREC.TRACE, seq("trace", $._expr)),

    readExpr: $ => prec(PREC.READ, seq("read", $._expr)),

    readOrNullExpr: $ => prec(PREC.READ_OR_NULL, seq("read?", $._expr)),

    readGlobExpr: $ => prec(PREC.READ_GLOB, seq("read*", $._expr)),

    importExpr: $ => seq("import", seq('(', field("path", $.stringConstant), ')')),

    importGlobExpr: $ => seq("import*", seq('(', field("path", $.stringConstant), ')')),

    functionLiteral: $ => prec(PREC.FUN, seq(field("args", $.parameterList), "->", field("body", $._expr))),

    qualifiedIdentifier: $ => seq(
      $.identifier,
      repeat(seq(".", $.identifier)),
    ),

    typedIdentifier: $ => seq($.identifier, optional($.typeAnnotation)),

    // TODO: adapt to pkl
    identifier: $ => {
      const alpha = /[^\s0-9:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\uFEFF\u2060\u200B\u00A0]/
      const alpha_numeric = /[^\s:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\uFEFF\u2060\u200B\u00A0]/

      return token(choice(seq(alpha, repeat(alpha_numeric)), /`[^`]*`/))
    },

    lineComment: $ => token(seq('//', /.*/)),

    // TODO
    docComment: $ => token(seq('///', /.*/)),

    blockComment: $ => token(seq(
      '/*',
      /[^*]*\*+([^/*][^*]*\*+)*/,
      '/'
    ))
  }
});

function commaSep1 (rule) {
  return seq(rule, repeat(seq(',', rule)));
}

function commaSep (rule) {
  return optional(commaSep1(rule));
}

function sepBy (sep, rule) {
  return optional(sepBy1(sep, rule))
}

function sepBy1 (sep, rule) {
  return seq(rule, repeat(seq(sep, rule)));
}
