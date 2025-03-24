/*
 * Copyright © 2025 Apple Inc. and the Pkl project authors. All rights reserved.
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
import org.gradle.api.GradleException
import org.pkl.core.parser.Parser
import org.pkl.core.parser.ParserError
import org.pkl.core.parser.syntax.*
import org.pkl.core.parser.syntax.Expr.*
import org.pkl.core.parser.syntax.ObjectMember.*
import org.pkl.core.parser.syntax.Parameter.*
import org.pkl.core.parser.syntax.Type.*

@Suppress("MemberVisibilityCanBePrivate", "DuplicatedCode")
object SExprRenderer {
  private var tab = ""
  private var buf = StringBuilder()
  private lateinit var sourceText: String
  private val parser = Parser()

  fun render(sourceText: String, filename: String): String {
    this.sourceText = sourceText
    // strip line comments because pkl-core parser doesn't emit them, so SExprRenderer can't
    // produce these tokens
    // whilst tree-sitter will.
    val module =
      try {
        parser.parseModule(sourceText)
      } catch (_: ParserError) {
        throw GradleException("Failed to parse file $filename")
      }
    renderModule(module)
    val res = buf.toString()
    reset()
    return res
  }

  fun renderModule(mod: Module) {
    buf.append(tab)
    buf.append("(module")
    val oldTab = increaseTab()
    if (mod.decl !== null) {
      buf.append('\n')
      renderModuleDeclaration(mod.decl!!)
    }
    for (imp in mod.imports) {
      buf.append('\n')
      renderImport(imp)
    }
    for (entry in sortModuleEntries(mod)) {
      buf.append('\n')
      when (entry) {
        is Class -> renderClass(entry)
        is TypeAlias -> renderTypeAlias(entry)
        is ClassProperty -> renderClassPropertyEntry(entry)
        is ClassMethod -> renderClassMethod(entry)
      }
    }
    tab = oldTab
    buf.append(')')
  }

  fun renderModuleDeclaration(decl: ModuleDecl) {
    buf.append(tab)
    buf.append("(moduleHeader")
    val oldTab = increaseTab()
    if (decl.docComment !== null) {
      buf.append('\n')
      renderDocComment()
    }
    for (ann in decl.annotations) {
      buf.append('\n')
      renderAnnotation(ann)
    }
    if (decl.modifiers.isNotEmpty() || decl.name !== null) {
      buf.append('\n')
      renderModuleClause(decl)
    }
    if (decl.extendsOrAmendsDecl !== null) {
      buf.append('\n')
      renderExtendsOrAmendsClause()
    }
    tab = oldTab
    buf.append(')')
  }

  fun renderModuleClause(node: ModuleDecl) {
    buf.append(tab)
    buf.append("(moduleClause\n")
    val oldTab = increaseTab()
    node.modifiers.forEach {
      buf.append('\n')
      renderModifier()
    }
    if (node.name !== null) {
      buf.append('\n')
      renderQualifiedIdent(node.name!!)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderExtendsOrAmendsClause() {
    val oldTab = increaseTab()
    buf.append(tab)
    buf.append("(extendsOrAmendsClause\n")
    renderStringConstant()
    buf.append(')')
    tab = oldTab
  }

  fun renderImport(imp: ImportClause) {
    buf.append(tab)
    if (imp.isGlob) {
      buf.append("(importGlobClause")
    } else {
      buf.append("(importClause")
    }
    val oldTab = increaseTab()
    buf.append('\n')
    renderStringConstant()
    if (imp.alias !== null) {
      buf.append('\n')
      buf.append(tab)
      buf.append("(identifier)")
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderStringConstant() {
    buf.append(tab)
    buf.append("(stringConstant")
    buf.append('\n')
    val oldTab = increaseTab()
    buf.append(tab)
    buf.append("(slStringLiteralPart))")
    tab = oldTab
  }

  fun renderClass(clazz: Class) {
    buf.append(tab)
    buf.append("(clazz")
    val oldTab = increaseTab()
    if (clazz.docComment !== null) {
      buf.append('\n')
      renderDocComment()
    }
    for (ann in clazz.annotations) {
      buf.append('\n')
      renderAnnotation(ann)
    }
    clazz.modifiers.forEach {
      buf.append('\n')
      renderModifier()
    }
    buf.append('\n')
    buf.append(tab)
    buf.append("(identifier)")
    val tparList = clazz.typeParameterList
    if (tparList !== null) {
      buf.append('\n')
      renderTypeParameterList(tparList)
    }
    if (clazz.superClass !== null) {
      renderClassExtendsClause(clazz)
    }
    if (clazz.body !== null) {
      buf.append('\n')
      renderClassBody(clazz.body!!)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderClassExtendsClause(clazz: Class) {
    val oldTab = increaseTab()
    buf.append(tab)
    buf.append("(classExtendsClause")
    buf.append('\n')
    renderQualifiedIdent((clazz.superClass as DeclaredType).name)
    buf.append(")")
    tab = oldTab
  }

  fun renderClassBody(classBody: ClassBody) {
    buf.append(tab)
    buf.append("(classBody")
    val oldTab = increaseTab()
    for (entry in sortClassEntries(classBody)) {
      buf.append('\n')
      when (entry) {
        is ClassProperty -> renderClassPropertyEntry(entry)
        is ClassMethod -> renderClassMethod(entry)
      }
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderTypeAlias(`typealias`: TypeAlias) {
    buf.append(tab)
    buf.append("(typeAlias")
    val oldTab = increaseTab()
    if (`typealias`.docComment !== null) {
      buf.append('\n')
      renderDocComment()
    }
    for (ann in `typealias`.annotations) {
      buf.append('\n')
      renderAnnotation(ann)
    }
    `typealias`.modifiers.forEach {
      buf.append('\n')
      renderModifier()
    }
    buf.append('\n')
    buf.append(tab)
    buf.append("(identifier)")
    val tparList = `typealias`.typeParameterList
    if (tparList !== null) {
      buf.append('\n')
      renderTypeParameterList(tparList)
    }
    buf.append('\n')
    renderType(`typealias`.type)
    buf.append(')')
    tab = oldTab
  }

  fun renderClassPropertyEntry(classEntry: ClassProperty) {
    buf.append(tab)
    buf.append("(classProperty")
    val oldTab = increaseTab()
    if (classEntry.docComment !== null) {
      buf.append('\n')
      renderDocComment()
    }
    for (ann in classEntry.annotations) {
      buf.append('\n')
      renderAnnotation(ann)
    }
    classEntry.modifiers.forEach {
      buf.append('\n')
      renderModifier()
    }
    buf.append('\n')
    buf.append(tab)
    buf.append("(identifier)")
    classEntry.typeAnnotation?.let { typeAnnotation ->
      buf.append('\n')
      renderTypeAnnotation(typeAnnotation)
    }
    classEntry.expr?.let { expr ->
      buf.append('\n')
      renderExpr(expr)
    }
    for (body in classEntry.bodyList) {
      buf.append('\n')
      renderObjectBody(body)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderMethodHeader(method: ClassMethod) {
    buf.append(tab)
    buf.append("(methodHeader")
    val oldTab = increaseTab()
    method.modifiers.forEach {
      buf.append('\n')
      renderModifier()
    }
    buf.append('\n')
    buf.append(tab)
    buf.append("(identifier)")
    method.typeParameterList?.let { tParList ->
      buf.append('\n')
      renderTypeParameterList(tParList)
    }
    buf.append('\n')
    renderParameterList(method.parameterList)
    if (method.typeAnnotation !== null) {
      buf.append('\n')
      renderTypeAnnotation(method.typeAnnotation!!)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderMethodHeader(method: ObjectMethod) {
    buf.append(tab)
    buf.append("(methodHeader")
    val oldTab = increaseTab()
    method.modifiers.forEach {
      buf.append('\n')
      renderModifier()
    }
    buf.append('\n')
    buf.append(tab)
    buf.append("(identifier)")
    method.typeParameterList?.let { tParList ->
      buf.append('\n')
      renderTypeParameterList(tParList)
    }
    buf.append('\n')
    renderParameterList(method.paramList)
    if (method.typeAnnotation !== null) {
      buf.append('\n')
      renderTypeAnnotation(method.typeAnnotation!!)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderClassMethod(classMethod: ClassMethod) {
    buf.append(tab)
    buf.append("(classMethod")
    val oldTab = increaseTab()
    if (classMethod.docComment !== null) {
      buf.append('\n')
      renderDocComment()
    }
    for (ann in classMethod.annotations) {
      buf.append('\n')
      renderAnnotation(ann)
    }
    buf.append('\n')
    renderMethodHeader(classMethod)
    if (classMethod.expr !== null) {
      buf.append('\n')
      renderExpr(classMethod.expr!!)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderDocComment() {
    buf.append(tab)
    buf.append("(docComment)")
  }

  fun renderAnnotation(
    @Suppress("RemoveRedundantQualifierName") ann: org.pkl.core.parser.syntax.Annotation
  ) {
    buf.append(tab)
    buf.append("(annotation")
    val oldTab = increaseTab()
    buf.append('\n')
    renderQualifiedIdent((ann.type as DeclaredType).name)
    if (ann.body !== null) {
      buf.append('\n')
      renderObjectBody(ann.body!!)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderQualifiedIdent(name: QualifiedIdentifier) {
    buf.append(tab)
    buf.append("(qualifiedIdentifier")
    val oldTab = increaseTab()
    name.identifiers.indices.forEach {
      buf.append('\n')
      buf.append(tab)
      buf.append("(identifier)")
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderObjectBody(body: ObjectBody) {
    buf.append(tab)
    buf.append("(objectBody")
    val oldTab = increaseTab()
    if (body.parameters.isNotEmpty()) {
      buf.append('\n')
      renderObjectBodyParameters(body.parameters)
    }
    for (member in body.members) {
      buf.append('\n')
      renderMember(member)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderObjectBodyParameters(params: List<Parameter>) {
    buf.append(tab)
    buf.append("(objectBodyParameters\n")
    val oldTab = increaseTab()
    for (par in params) {
      buf.append('\n')
      renderParameter(par)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderParameterList(parList: ParameterList) {
    buf.append(tab)
    buf.append("(parameterList")
    val oldTab = increaseTab()
    for (par in parList.parameters) {
      buf.append('\n')
      renderParameter(par)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderParameter(par: Parameter) {

    buf.append(tab)
    if (par is TypedIdentifier) {
      buf.append("(typedIdentifier")
      val oldTab = increaseTab()
      buf.append('\n')
      buf.append(tab)
      buf.append("(identifier)")
      if (par.typeAnnotation !== null) {
        buf.append('\n')
        renderTypeAnnotation(par.typeAnnotation!!)
      }
      buf.append(')')
      tab = oldTab
    } else {
      buf.append("(blankIdentifier)")
    }
  }

  fun renderArgumentList(argumentList: ArgumentList) {
    buf.append(tab)
    buf.append("(argumentList")
    val oldTab = increaseTab()
    for (arg in argumentList.arguments) {
      buf.append('\n')
      renderExpr(arg)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderExpr(expr: Expr) {
    when (expr) {
      is ThisExpr -> {
        buf.append(tab)
        buf.append("(thisExpr)")
      }
      is OuterExpr -> {
        buf.append(tab)
        buf.append("(outerExpr)")
      }
      is ModuleExpr -> {
        buf.append(tab)
        buf.append("(moduleExpr)")
      }
      is NullLiteralExpr -> {
        buf.append(tab)
        buf.append("(nullLiteralExpr)")
      }
      is BoolLiteralExpr -> {
        buf.append(tab)
        if (expr.isB) {
          buf.append("(trueLiteralExpr)")
        } else {
          buf.append("(falseLiteralExpr)")
        }
      }
      is IntLiteralExpr -> {
        buf.append(tab)
        buf.append("(intLiteralExpr)")
      }
      is FloatLiteralExpr -> {
        buf.append(tab)
        buf.append("(floatLiteralExpr)")
      }
      is SingleLineStringLiteralExpr -> renderSingleLineStringLiteral(expr)
      is MultiLineStringLiteralExpr -> renderMultiLineStringLiteral(expr)
      is ThrowExpr -> renderThrowExpr(expr)
      is TraceExpr -> renderTraceExpr(expr)
      is ImportExpr -> renderImportExpr()
      is ReadExpr -> renderReadExpr(expr)
      is UnqualifiedAccessExpr -> renderUnqualifiedAccessExpr(expr)
      is QualifiedAccessExpr -> renderQualifiedAccessExpr(expr)
      is SuperAccessExpr -> renderSuperAccessExpr(expr)
      is SuperSubscriptExpr -> renderSuperSubscriptExpr(expr)
      is SubscriptExpr -> renderSubscriptExpr(expr)
      is IfExpr -> renderIfExpr(expr)
      is LetExpr -> renderLetExpr(expr)
      is FunctionLiteralExpr -> renderFunctionLiteralExpr(expr)
      is ParenthesizedExpr -> renderParenthesisedExpr(expr)
      is NewExpr -> renderNewExpr(expr)
      is AmendsExpr -> renderAmendsExpr(expr)
      is NonNullExpr -> renderNonNullExpr(expr)
      is UnaryMinusExpr -> renderUnaryMinusExpr(expr)
      is LogicalNotExpr -> renderLogicalNotExpr(expr)
      is BinaryOperatorExpr -> renderBinaryOpExpr(expr)
      is TypeCheckExpr -> renderTypeCheckExpr(expr)
      is TypeCastExpr -> renderTypeCastExpr(expr)
      is OperatorExpr -> throw RuntimeException("Operator expr should not exist after parsing")
      is TypeExpr -> throw RuntimeException("Type expr should not exist after parsing")
    }
  }

  fun renderSingleLineStringLiteral(expr: SingleLineStringLiteralExpr) {
    buf.append(tab)
    buf.append("(slStringLiteralExpr")
    val oldTab = increaseTab()
    for (part in expr.parts) {
      if (part is StringPart.StringInterpolation) {
        buf.append('\n')
        renderInterpolation(part)
      } else {
        buf.append('\n').append(tab)
        buf.append("(slStringLiteralPart)")
      }
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderMultiLineStringLiteral(expr: MultiLineStringLiteralExpr) {
    buf.append(tab)
    buf.append("(mlStringLiteralExpr")
    val oldTab = increaseTab()
    // render only interpolated expressions because
    // the new parser parses string differently
    for (part in expr.parts) {
      if (part is StringPart.StringInterpolation) {
        buf.append('\n')
        renderInterpolation(part)
      } else {
        buf.append('\n').append(tab)
        buf.append("(mlStringLiteralPart)")
      }
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderInterpolation(node: StringPart.StringInterpolation) {
    buf.append(tab)
    buf.append("(stringInterpolation\n")
    val oldTab = increaseTab()
    renderExpr(node.expr)
    buf.append(")")
    tab = oldTab
  }

  fun renderThrowExpr(expr: ThrowExpr) {
    buf.append(tab)
    buf.append("(throwExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(expr.expr)
    buf.append(')')
    tab = oldTab
  }

  fun renderTraceExpr(expr: TraceExpr) {
    buf.append(tab)
    buf.append("(traceExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(expr.expr)
    buf.append(')')
    tab = oldTab
  }

  fun renderImportExpr() {
    val name = "(importExpr"
    buf.append(tab)
    buf.append(name)
    val oldTab = increaseTab()
    buf.append('\n')
    renderStringConstant()
    buf.append(')')
    tab = oldTab
  }

  fun renderReadExpr(expr: ReadExpr) {
    val name = "(readExpr"
    buf.append(tab)
    buf.append(name)
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(expr.expr)
    buf.append(')')
    tab = oldTab
  }

  fun renderUnqualifiedAccessExpr(expr: UnqualifiedAccessExpr) {
    buf.append(tab)
    buf.append("(unqualifiedAccessExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    buf.append(tab)
    buf.append("(identifier)")
    if (expr.argumentList !== null) {
      buf.append('\n')
      renderArgumentList(expr.argumentList!!)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderQualifiedAccessExpr(expr: QualifiedAccessExpr) {
    buf.append(tab)
    buf.append("(qualifiedAccessExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(expr.expr)
    buf.append('\n')
    buf.append(tab)
    buf.append("(identifier)")
    if (expr.argumentList !== null) {
      buf.append('\n')
      renderArgumentList(expr.argumentList!!)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderSuperAccessExpr(expr: SuperAccessExpr) {
    buf.append(tab)
    buf.append("(superAccessExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    buf.append(tab)
    buf.append("(identifier)")
    if (expr.argumentList !== null) {
      buf.append('\n')
      renderArgumentList(expr.argumentList!!)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderSuperSubscriptExpr(expr: SuperSubscriptExpr) {
    buf.append(tab)
    buf.append("(superSubscriptExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(expr.arg)
    buf.append(')')
    tab = oldTab
  }

  fun renderSubscriptExpr(expr: SubscriptExpr) {
    buf.append(tab)
    buf.append("(subscriptExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(expr.expr)
    buf.append('\n')
    renderExpr(expr.arg)
    buf.append(')')
    tab = oldTab
  }

  fun renderIfExpr(expr: IfExpr) {
    buf.append(tab)
    buf.append("(ifExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(expr.cond)
    buf.append('\n')
    renderExpr(expr.then)
    buf.append('\n')
    renderExpr(expr.els)
    buf.append(')')
    tab = oldTab
  }

  fun renderLetExpr(expr: LetExpr) {
    buf.append(tab)
    buf.append("(letExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    renderParameter(expr.parameter)
    buf.append('\n')
    renderExpr(expr.bindingExpr)
    buf.append('\n')
    renderExpr(expr.expr)
    buf.append(')')
    tab = oldTab
  }

  fun renderFunctionLiteralExpr(expr: FunctionLiteralExpr) {
    buf.append(tab)
    buf.append("(functionLiteralExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    renderParameterList(expr.parameterList)
    buf.append('\n')
    renderExpr(expr.expr)
    buf.append(')')
    tab = oldTab
  }

  fun renderParenthesisedExpr(expr: ParenthesizedExpr) {
    buf.append(tab)
    buf.append("(parenthesizedExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(expr.expr)
    buf.append(')')
    tab = oldTab
  }

  fun renderNewExpr(expr: NewExpr) {
    buf.append(tab)
    buf.append("(newExpr")
    val oldTab = increaseTab()
    if (expr.type !== null) {
      buf.append('\n')
      renderType(expr.type!!)
    }
    buf.append('\n')
    renderObjectBody(expr.body)
    buf.append(')')
    tab = oldTab
  }

  fun renderAmendsExpr(expr: AmendsExpr) {
    buf.append(tab)
    buf.append("(amendExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(expr.expr)
    buf.append('\n')
    renderObjectBody(expr.body)
    buf.append(')')
    tab = oldTab
  }

  fun renderNonNullExpr(expr: NonNullExpr) {
    buf.append(tab)
    buf.append("(nonNullExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(expr.expr)
    buf.append(')')
    tab = oldTab
  }

  fun renderUnaryMinusExpr(expr: UnaryMinusExpr) {
    buf.append(tab)
    buf.append("(unaryMinusExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(expr.expr)
    buf.append(')')
    tab = oldTab
  }

  fun renderLogicalNotExpr(expr: LogicalNotExpr) {
    buf.append(tab)
    buf.append("(logicalNotExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(expr.expr)
    buf.append(')')
    tab = oldTab
  }

  fun renderBinaryOpExpr(expr: BinaryOperatorExpr) {
    buf.append(tab)
    val name =
      when (expr.op) {
        Operator.POW -> "(exponentiationExpr"
        Operator.MULT,
        Operator.DIV,
        Operator.INT_DIV,
        Operator.MOD -> "(multiplicativeExpr"
        Operator.PLUS,
        Operator.MINUS -> "(additiveExpr"
        Operator.LT,
        Operator.GT,
        Operator.LTE,
        Operator.GTE -> "(comparisonExpr"
        Operator.IS,
        Operator.AS -> "(typeTestExpr"
        Operator.EQ_EQ,
        Operator.NOT_EQ -> "(equalityExpr"
        Operator.AND -> "(logicalAndExpr"
        Operator.OR -> "(logicalOrExpr"
        Operator.PIPE -> "(pipeExpr"
        Operator.NULL_COALESCE -> "(nullCoalesceExpr"
        else -> throw RuntimeException("Should never receive a dot operator here")
      }
    buf.append(name)
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(expr.left)
    buf.append('\n')
    renderExpr(expr.right)
    buf.append(')')
    tab = oldTab
  }

  fun renderTypeCheckExpr(expr: TypeCheckExpr) {
    buf.append(tab)
    buf.append("(typeTestExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(expr.expr)
    buf.append('\n')
    renderType(expr.type)
    buf.append(')')
    tab = oldTab
  }

  fun renderTypeCastExpr(expr: TypeCastExpr) {
    buf.append(tab)
    buf.append("(typeTestExpr")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(expr.expr)
    buf.append('\n')
    renderType(expr.type)
    buf.append(')')
    tab = oldTab
  }

  fun renderTypeAnnotation(typeAnnotation: TypeAnnotation) {
    buf.append(tab)
    buf.append("(typeAnnotation")
    val oldTab = increaseTab()
    buf.append('\n')
    renderType(typeAnnotation.type)
    buf.append(')')
    tab = oldTab
  }

  fun renderType(type: Type) {
    when (type) {
      is UnknownType -> {
        buf.append(tab)
        buf.append("(unknownType)")
      }
      is NothingType -> {
        buf.append(tab)
        buf.append("(nothingType)")
      }
      is ModuleType -> {
        buf.append(tab)
        buf.append("(moduleType)")
      }
      is StringConstantType -> {
        buf.append(tab)
        buf.append("(stringLiteralType")
        buf.append('\n')
        val oldTab = increaseTab()
        renderStringConstant()
        buf.append(')')
        tab = oldTab
      }
      is DeclaredType -> renderDeclaredType(type)
      is ParenthesizedType -> renderParenthesizedType(type)
      is NullableType -> renderNullableType(type)
      is ConstrainedType -> renderConstrainedType(type)
      is UnionType -> {
        val effectiveUnionType = nestUnionType(type)
        renderUnionType(effectiveUnionType)
      }
      is FunctionType -> renderFunctionType(type)
    }
  }

  fun renderDeclaredType(type: DeclaredType) {
    buf.append(tab)
    buf.append("(declaredType")
    val oldTab = increaseTab()
    buf.append('\n')
    renderQualifiedIdent(type.name)
    if (type.args !== null) {
      buf.append('\n')
      renderTypeArgumentList(type.args!!)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderTypeArgumentList(typeArgumentList: TypeArgumentList) {
    buf.append(tab)
    buf.append("(typeArgumentList")
    val oldTab = increaseTab()
    for (arg in typeArgumentList.types) {
      buf.append('\n')
      renderType(arg)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderParenthesizedType(type: ParenthesizedType) {
    buf.append(tab)
    buf.append("(parenthesizedType")
    val oldTab = increaseTab()
    buf.append('\n')
    renderType(type.type)
    buf.append(')')
    tab = oldTab
  }

  fun renderNullableType(type: NullableType) {
    buf.append(tab)
    buf.append("(nullableType")
    val oldTab = increaseTab()
    buf.append('\n')
    renderType(type.type)
    buf.append(')')
    tab = oldTab
  }

  fun renderConstrainedType(type: ConstrainedType) {
    buf.append(tab)
    buf.append("(constrainedType")
    val oldTab = increaseTab()
    buf.append('\n')
    renderType(type.type)
    for (expr in type.exprs) {
      buf.append('\n')
      renderExpr(expr)
    }
    buf.append(')')
    tab = oldTab
  }

  // tree-sitter expresses multiple alternatives as ((A | B) | C), rather than A | B | C
  fun nestUnionType(unionType: UnionType): UnionType {
    if (unionType.types.size == 2) {
      return unionType
    }
    val innerTypes = unionType.types.dropLast(1)
    val innerUnion =
      nestUnionType(
        UnionType(
          innerTypes,
          if (unionType.defaultIndex < innerTypes.size) unionType.defaultIndex else -1,
          unionType.span(),
        )
      )
    return UnionType(
      listOf(innerUnion, unionType.types.last()),
      if (unionType.defaultIndex == unionType.types.size - 1) 1 else -1,
      unionType.span(),
    )
  }

  fun renderUnionType(type: UnionType) {
    buf.append(tab)
    buf.append("(unionType")
    val oldTab = increaseTab()
    for (idx in type.types.indices) {
      val typ = type.types[idx]
      buf.append('\n')
      if (type.defaultIndex == idx) {
        buf.append(tab)
        buf.append("(defaultUnionType")
        val oldTab2 = increaseTab()
        buf.append('\n')
        renderType(typ)
        buf.append(')')
        tab = oldTab2
      } else {
        renderType(typ)
      }
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderFunctionType(type: FunctionType) {
    buf.append(tab)
    buf.append("(functionLiteralType")
    val oldTab = increaseTab()
    for (arg in type.args) {
      buf.append('\n')
      renderType(arg)
    }
    buf.append('\n')
    renderType(type.ret)
    buf.append(')')
    tab = oldTab
  }

  fun renderMember(member: ObjectMember) {
    when (member) {
      is ObjectElement -> renderObjectElement(member)
      is ObjectProperty -> renderObjectProperty(member)
      is ObjectMethod -> renderObjectMethod(member)
      is MemberPredicate -> renderMemberPredicate(member)
      is ObjectEntry -> renderObjectEntry(member)
      is ObjectSpread -> renderObjectSpread(member)
      is WhenGenerator -> renderWhenGenerator(member)
      is ForGenerator -> renderForGenerator(member)
    }
  }

  fun renderObjectElement(element: ObjectElement) {
    buf.append(tab)
    buf.append("(objectElement")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(element.expr)
    buf.append(')')
    tab = oldTab
  }

  fun renderObjectProperty(property: ObjectProperty) {
    buf.append(tab)
    buf.append("(objectProperty")
    val oldTab = increaseTab()
    property.modifiers.forEach {
      buf.append('\n')
      renderModifier()
    }
    buf.append('\n')
    buf.append(tab)
    buf.append("(identifier)")
    if (property.typeAnnotation !== null) {
      buf.append('\n')
      renderTypeAnnotation(property.typeAnnotation!!)
    }
    property.expr?.let {
      buf.append('\n')
      renderExpr(it)
    }
    for (body in property.bodyList) {
      buf.append('\n')
      renderObjectBody(body)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderObjectMethod(method: ObjectMethod) {
    buf.append(tab)
    buf.append("(objectMethod")
    val oldTab = increaseTab()
    buf.append('\n')
    renderMethodHeader(method)
    buf.append('\n')
    renderExpr(method.expr)
    buf.append(')')
    tab = oldTab
  }

  fun renderMemberPredicate(predicate: MemberPredicate) {
    buf.append(tab)
    buf.append("(memberPredicate")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(predicate.pred)
    predicate.expr?.let { expr ->
      buf.append('\n')
      renderExpr(expr)
    }
    for (body in predicate.bodyList) {
      buf.append('\n')
      renderObjectBody(body)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderObjectEntry(entry: ObjectEntry) {
    buf.append(tab)
    buf.append("(objectEntry")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(entry.key)
    entry.value?.let { value ->
      buf.append('\n')
      renderExpr(value)
    }
    for (body in entry.bodyList) {
      buf.append('\n')
      renderObjectBody(body)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderObjectSpread(spread: ObjectSpread) {
    buf.append(tab)
    buf.append("(objectSpread")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(spread.expr)
    buf.append(')')
    tab = oldTab
  }

  fun renderWhenGenerator(generator: WhenGenerator) {
    buf.append(tab)
    buf.append("(whenGenerator")
    val oldTab = increaseTab()
    buf.append('\n')
    renderExpr(generator.predicate)
    buf.append('\n')
    renderObjectBody(generator.thenClause)
    if (generator.elseClause !== null) {
      buf.append('\n')
      renderObjectBody(generator.elseClause!!)
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderForGenerator(generator: ForGenerator) {
    buf.append(tab)
    buf.append("(forGenerator")
    val oldTab = increaseTab()
    buf.append('\n')
    renderParameter(generator.p1)
    if (generator.p2 !== null) {
      buf.append('\n')
      renderParameter(generator.p2!!)
    }
    buf.append('\n')
    renderExpr(generator.expr)
    buf.append('\n')
    renderObjectBody(generator.body)
    buf.append(')')
    tab = oldTab
  }

  fun renderTypeParameterList(typeParameterList: TypeParameterList) {
    buf.append(tab)
    buf.append("(typeParameterList")
    val oldTab = increaseTab()
    typeParameterList.parameters.forEach {
      buf.append('\n')
      renderTypeParameter()
    }
    buf.append(')')
    tab = oldTab
  }

  fun renderTypeParameter() {
    buf.append(tab)
    buf.append("(typeParameter\n")
    val oldTab = increaseTab()
    buf.append(tab)
    buf.append("(identifier))")
    tab = oldTab
  }

  fun renderModifier() {
    buf.append(tab)
    buf.append("(modifier)")
  }

  fun reset() {
    tab = ""
    buf = StringBuilder()
  }

  private fun increaseTab(): String {
    val old = tab
    tab += "  "
    return old
  }

  private fun sortModuleEntries(mod: Module): List<Node> {
    val res = mutableListOf<Node>()
    res += mod.classes
    res += mod.typeAliases
    res += mod.properties
    res += mod.methods
    res.sortWith(compareBy { it.span().charIndex })
    return res
  }

  private fun sortClassEntries(body: ClassBody): List<Node> {
    val res = mutableListOf<Node>()
    res += body.properties
    res += body.methods
    res.sortWith(compareBy { it.span().charIndex })
    return res
  }
}
