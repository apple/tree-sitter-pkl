/*
 * Copyright © 2024-2025 Apple Inc. and the Pkl project authors. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   https://www.apache.org/licenses/LICENSE-2.0
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
  ACCESS: 99,
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
  FUN: -11,

  NULLABLE_TYPE: 5,
  FUN_TYPE: -5,
  UNION_DEFAULT_TYPE: -6,
  UNION_TYPE: -7,

  VAR_OBJ_LITERAL: 2,
  AMEND_EXPR: 1,
  OBJ_MEMBER: -1
};

const decimalLiteral = seq(/\d/, /[\d_]*/);

module.exports = grammar({
  name: 'pkl',

  reserved: {
    global: $ => [
      'abstract',
      'amends',
      'as',
      'class',
      'const',
      'else',
      'extends',
      'external',
      'false',
      'fixed',
      'for',
      'function',
      'hidden',
      'if',
      'import',
      'import*',
      'in',
      'is',
      'let',
      'local',
      'module',
      'new',
      'nothing',
      'null',
      'open',
      'out',
      'outer',
      'read',
      'read*',
      'read?',
      'super',
      'this',
      'throw',
      'trace',
      'true',
      'typealias',
      'unknown',
      'when',

      // TODO support reserved keywords
      // 'protected',
      // 'override',
      // 'record',
      // 'delete',
      // 'case',
      // 'switch',
      // 'vararg',
    ]
  },

  externals: $ => [
    $._sl_string_chars,
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
    // '[' without preceding newline or semicolon
    $._open_subscript_bracket,
    // '(' without preceding newline or semicolon
    $._open_argument_paren,
    // '-' without preceding newline or semicolon
    $._binary_minus,
  ],

  extras: $ => [
    $.lineComment,
    $.blockComment,
    $.shebangComment,
    /[ \t\f\r\n;]/
  ],

  word: $ => $.identifier,

  conflicts: $ => [
    [$.unqualifiedAccessExpr, $.typedIdentifier],
    [$.declaredType]
  ],

  rules: {
    module: $ => seq(
      optional($.shebangComment),
      optional($.moduleHeader),
      repeat(choice($.importClause, $.importGlobClause)),
      repeat($._moduleMember)
    ),

    shebangComment: $ => seq(
      "#!",
      /.*/
    ),

    moduleHeader: $ => seq(
      optional($.docComment),
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
      optional($.docComment),
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
      optional($.docComment),
      repeat($.annotation),
      repeat($.modifier),
      "typealias",
      $.identifier,
      optional($.typeParameterList),
      "=",
      $.type
    ),

    classProperty: $ => seq(
      optional($.docComment),
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
      optional($.docComment),
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
      "[",
      field("key", $._expr),
      "]",
      choice(
        seq("=", field("valueExpr", $._expr)),
        repeat1($.objectBody)
      )
    ),

    objectElement: $ => $._expr,

    objectPredicate: $ => seq(
      "[[",
      field("conditionExpr", $._expr),
      "]]",
      choice(
        seq("=", field("valueExpr", $._expr)),
        repeat1($.objectBody)
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
      $.objectBody
    ),

    whenGenerator: $ => seq(
      "when",
      "(",
      field("conditionExpr", $._expr),
      ")",
      field("thenBody", $.objectBody),
      optional(
        seq(
          "else",
          field("elseBody", $.objectBody)
        )
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
      alias("unknown", $.unknownType),
      alias("nothing", $.nothingType),
      alias("module", $.moduleType),
      $.stringLiteralType,
      $.declaredType,
      $.parenthesizedType,
      $.nullableType,
      $.constrainedType,
      $.unionType,
      $.unionDefaultType,
      $.functionLiteralType
    ),

    stringLiteralType: $ => $.stringConstant,

    declaredType: $ => seq($.qualifiedIdentifier, optional($.typeArgumentList)),

    parenthesizedType: $ => seq("(", $.type, ")"),

    nullableType: $ => prec(PREC.NULLABLE_TYPE, seq($.type, "?")),

    constrainedType: $ => seq($.type, "(", commaSep1($._expr), ")"),

    unionType: $ => prec.left(PREC.UNION_TYPE, seq($.type, "|", $.type)),

    unionDefaultType: $ => prec(PREC.UNION_DEFAULT_TYPE, seq("*", $.type)),

    functionLiteralType: $ => prec(PREC.FUN_TYPE, seq("(", commaSep($.type), ")", "->", $.type)),

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
      alias($._open_argument_paren, '('),
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
      $.thisExpr,
      $.outerExpr,
      $.moduleExpr,
      $.nullLiteral,
      $.trueLiteral,
      $.falseLiteral,
      $.intLiteral,
      $.floatLiteral,
      $.throwExpr,
      $.traceExpr,
      $.importExpr,
      $.readExpr,
      $.unqualifiedAccessExpr,
      $.slStringLiteral,
      $.mlStringLiteral,
      $.newExpr,
      $.amendExpr,
      $.superAccessExpr,
      $.superSubscriptExpr,
      $.qualifiedAccessExpr,
      $.subscriptExpr,
      $.nonNullExpr,
      $.unaryMinusExpr,
      $.logicalNotExpr,
      $.exponentiationExpr,
      $.multiplicativeExpr,
      $.additiveExpr,
      $.comparisonExpr,
      $.typeTestExpr,
      $.equalityExpr,
      $.logicalAndExpr,
      $.logicalOrExpr,
      $.pipeExpr,
      $.nullCoalesceExpr,
      $.ifExpr,
      $.letExpr,
      $.functionLiteral,
      $.parenthesizedExpr,
    ),

    parenthesizedExpr: $ => seq("(", $._expr, ")"),

    thisExpr: $ => "this",

    outerExpr: $ => "outer",

    moduleExpr: $ => "module",

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

    stringConstant: $ => choice(
      seq(
        '"',
        repeat(choice(
          $.slStringLiteralPart,
          $.escapeSequence
        )),
        '"'
      ),
      seq(
        '#"',
        repeat(choice(
          alias($.slStringLiteralPart1, $.slStringLiteralPart),
          alias($.escapeSequence1, $.escapeSequence),
        )),
        '"#'
      )
    ),

    slStringLiteral: $ => choice(
      seq(
        '"',
        repeat(choice(
          $.slStringLiteralPart,
          $.escapeSequence,
          $.interpolationExpr,
        )),
        '"'
      ),
      seq(
        '#"',
        repeat(choice(
          alias($.slStringLiteralPart1, $.slStringLiteralPart),
          alias($.escapeSequence1, $.escapeSequence),
          alias($.interpolationExpr1, $.interpolationExpr)
        )),
        '"#'
      ),
      seq(
        '##"',
        repeat(choice(
          alias($.slStringLiteralPart2, $.slStringLiteralPart),
          alias($.escapeSequence2, $.escapeSequence),
          alias($.interpolationExpr2, $.interpolationExpr)
        )),
        '"##'
      ),
      seq(
        '###"',
        repeat(choice(
          alias($.slStringLiteralPart3, $.slStringLiteralPart),
          alias($.escapeSequence3, $.escapeSequence),
          alias($.interpolationExpr3, $.interpolationExpr)
        )),
        '"###'
      ),
      seq(
        '####"',
        repeat(choice(
          alias($.slStringLiteralPart4, $.slStringLiteralPart),
          alias($.escapeSequence4, $.escapeSequence),
          alias($.interpolationExpr4, $.interpolationExpr)
        )),
        '"####'
      ),
      seq(
        '#####"',
        repeat(choice(
          alias($.slStringLiteralPart5, $.slStringLiteralPart),
          alias($.escapeSequence5, $.escapeSequence),
          alias($.interpolationExpr5, $.interpolationExpr)
        )),
        '"#####'
      ),
      seq(
        '######"',
        repeat(choice(
          alias($.slStringLiteralPart6, $.slStringLiteralPart),
          alias($.escapeSequence6, $.escapeSequence),
          alias($.interpolationExpr6, $.interpolationExpr)
        )),
        '"######'
      ),
    ),

    slStringLiteralPart: $ => $._sl_string_chars,

    slStringLiteralPart1: $ => $._sl1_string_chars,

    slStringLiteralPart2: $ => $._sl2_string_chars,

    slStringLiteralPart3: $ => $._sl3_string_chars,

    slStringLiteralPart4: $ => $._sl4_string_chars,

    slStringLiteralPart5: $ => $._sl5_string_chars,

    slStringLiteralPart6: $ => $._sl6_string_chars,

    mlStringLiteral: $ => choice(
      seq(
        '"""',
        repeat(choice(
          $.mlStringLiteralPart,
          $.escapeSequence,
          $.interpolationExpr
        )),
        '"""'
      ),
      seq(
        '#"""',
        repeat(choice(
          alias($.mlStringLiteralPart1, $.mlStringLiteralPart),
          alias($.escapeSequence1, $.escapeSequence),
          alias($.interpolationExpr1, $.interpolationExpr)
        )),
        '"""#'
      ),
      seq(
        '##"""',
        repeat(choice(
          alias($.mlStringLiteralPart2, $.mlStringLiteralPart),
          alias($.escapeSequence2, $.escapeSequence),
          alias($.interpolationExpr2, $.interpolationExpr)
        )),
        '"""##'
      ),
      seq(
        '###"""',
        repeat(choice(
          alias($.mlStringLiteralPart3, $.mlStringLiteralPart),
          alias($.escapeSequence3, $.escapeSequence),
          alias($.interpolationExpr3, $.interpolationExpr)
        )),
        '"""###'
      ),
      seq(
        '####"""',
        repeat(choice(
          alias($.mlStringLiteralPart4, $.mlStringLiteralPart),
          alias($.escapeSequence4, $.escapeSequence),
          alias($.interpolationExpr4, $.interpolationExpr)
        )),
        '"""####'
      ),
      seq(
        '#####"""',
        repeat(choice(
          alias($.mlStringLiteralPart5, $.mlStringLiteralPart),
          alias($.escapeSequence5, $.escapeSequence),
          alias($.interpolationExpr5, $.interpolationExpr)
        )),
        '"""#####'
      ),
      seq(
        '######"""',
        repeat(choice(
          alias($.mlStringLiteralPart6, $.mlStringLiteralPart),
          alias($.escapeSequence6, $.escapeSequence),
          alias($.interpolationExpr6, $.interpolationExpr)
        )),
        '"""######'
      ),
    ),

    mlStringLiteralPart: $ => $._ml_string_chars,

    mlStringLiteralPart1: $ => $._ml1_string_chars,

    mlStringLiteralPart2: $ => $._ml2_string_chars,

    mlStringLiteralPart3: $ => $._ml3_string_chars,

    mlStringLiteralPart4: $ => $._ml4_string_chars,

    mlStringLiteralPart5: $ => $._ml5_string_chars,

    mlStringLiteralPart6: $ => $._ml6_string_chars,

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

    amendExpr: $ => prec(PREC.AMEND_EXPR, seq(choice($.newExpr, $.amendExpr, seq('(', $._expr, ')')), $.objectBody)),

    subscriptExpr: $ => seq(field("receiver", $._expr), alias($._open_subscript_bracket, "["), $._expr, "]"),

    unaryMinusExpr: $ => prec.left(PREC.NEG, seq('-', $._expr)),

    logicalNotExpr: $ => prec.left(PREC.NOT, seq('!', $._expr)),

    nonNullExpr: $ => prec.left(PREC.NON_NULL, seq($._expr, "!!")),

    nullCoalesceExpr: $ => prec.right(PREC.COALESCE, seq($._expr, field('operator', '??'), $._expr)),

    exponentiationExpr: $ => prec.right(PREC.EXP, seq($._expr, field('operator', '**'), $._expr)),

    multiplicativeExpr: $ => prec.left(PREC.MUL, seq($._expr, field('operator', choice("*", "/", "~/", "%")), $._expr)),

    additiveExpr: $ => prec.left(PREC.ADD, seq($._expr, field('operator', choice("+", $._binary_minus)), $._expr)),

    comparisonExpr: $ => prec.left(PREC.REL, seq($._expr, field('operator', choice("<", "<=", ">=", ">")), $._expr)),

    equalityExpr: $ => prec.left(PREC.EQ, seq($._expr, field('operator', choice("==", "!=")), $._expr)),

    logicalAndExpr: $ => prec.left(PREC.AND, seq($._expr, field('operator', "&&"), $._expr)),

    logicalOrExpr: $ => prec.left(PREC.OR, seq($._expr, field('operator', "||"), $._expr)),
    
    pipeExpr: $ => prec.left(PREC.PIPE, seq($._expr, field('operator', "|>"), $._expr)),

    typeTestExpr: $ => prec(PREC.IS, seq($._expr, field("operator", choice("is", "as")), $.type)),

    ifExpr: $ => prec(PREC.IF, seq("if", "(", $._expr, ")", $._expr, "else", $._expr)),

    letExpr: $ => prec(PREC.LET, seq("let", "(", $.typedIdentifier, "=", $._expr, ")", $._expr)),

    throwExpr: $ => prec(PREC.THROW, seq("throw", '(', $._expr, ')')),

    traceExpr: $ => prec(PREC.TRACE, seq("trace", '(', $._expr, ')')),

    readExpr: $ => prec(PREC.READ, seq(field("keyword", choice("read", "read?", "read*")), '(', $._expr, ')')),

    importExpr: $ => seq(choice("import", "import*"), seq('(', $.stringConstant, ')')),

    unqualifiedAccessExpr: $ => seq($.identifier, optional($.argumentList)),

    superAccessExpr: $ => prec.left(PREC.ACCESS, seq("super", ".", $.identifier, optional($.argumentList))),

    superSubscriptExpr: $ => prec.left(PREC.ACCESS, seq("super", alias($._open_subscript_bracket, "["), $._expr, "]")),

    qualifiedAccessExpr: $ => prec.left(
      PREC.ACCESS,
      seq(
        field("receiver", $._expr),
        choice(".", "?."),
        seq(
          $.identifier,
          optional($.argumentList)
        ),
      )
    ),

    functionLiteral: $ => prec(PREC.FUN, seq($.parameterList, "->", $._expr)),

    qualifiedIdentifier: $ => prec.left(seq(
      $.identifier,
      repeat(seq(".", $.identifier)),
    )),

    typedIdentifier: $ => seq($.identifier, optional($.typeAnnotation)),

    // TODO: adapt to pkl
    identifier: $ => {
      const alpha = /[^\s0-9:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\uFEFF\u2060\u200B\u00A0]/
      const alpha_numeric = /[^\s:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\uFEFF\u2060\u200B\u00A0]/

      return token(choice(seq(alpha, repeat(alpha_numeric)), /`[^`]*`/))
    },

    lineComment: $ => choice(
      token(seq(/\/\/[^\/]/, /.*/)),
      '//$',
    ),

    // TODO
    docComment: $ => seq(
      token(seq('///', /.*/)),
      repeat(token(seq('///', /.*/)))
    ),

    blockComment: $ => token(seq(
      '/*',
      /[^*]*\*+([^/*][^*]*\*+)*/,
      '/'
    )),
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
