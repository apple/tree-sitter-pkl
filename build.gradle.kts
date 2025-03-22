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
@file:Suppress("unused")

plugins {
  alias(libs.plugins.kotlin)
  alias(libs.plugins.spotless)
}

val createLanguageSnippetsCorpus by
  tasks.registering {
    val snippetTestDir = file("../pkl/pkl-core/src/test/files/LanguageSnippetTests/input")
    val snippetTestFiles =
      fileTree("../pkl/pkl-core/src/test/files/LanguageSnippetTests/input") {
        include("*/*.pkl")
        exclude(
          // these files have syntax errors, which are not meaningful to test.
          "**/stringError1.pkl",
          "**/annotationIsNotExpression2.pkl",
          "**/amendsRequiresParens.pkl",
          "**/errors/parser18.pkl",
          "**/errors/nested1.pkl",
          "**/errors/invalidCharacterEscape.pkl",
          "**/errors/invalidUnicodeEscape.pkl",
          "**/errors/unterminatedUnicodeEscape.pkl",
          "**/errors/keywordNotAllowedHere1.pkl",
          "**/errors/keywordNotAllowedHere2.pkl",
          "**/errors/keywordNotAllowedHere3.pkl",
          "**/errors/keywordNotAllowedHere4.pkl",
          "**/errors/moduleWithHighMinPklVersionAndParseErrors.pkl",
          "**/errors/underscore.pkl",
          "**/notAUnionDefault.pkl",
          "**/multipleDefaults.pkl",
          "**/errors/delimiters/*",
          "**/errors/parser*",
          "**/parser/*",

          // these files have extra line/block comments, which gets discarded by pkl's Parser but
          // emitted by tree-sitter.
          "**/objects/duplicateProperty.pkl",
          "**/modules/duplicateProperty1.pkl",
          "**/modules/duplicateProperty2.pkl",
          "**/modules/duplicateProperty3.pkl",
          "**/modules/duplicateFunction.pkl",
          "**/methods/methodParameterTypes1.pkl",
          "**/listings/numberLiterals.pkl",
          "**/listings/listing5.pkl",
          "**/listings2/numberLiterals.pkl",
          "**/generators/forGeneratorInFunctionBody.pkl",
          "**/errors/forGeneratorWrongVariableName.pkl",
          "**/api/dynamic.pkl",
          "**/api/jsonRenderer2.json.pkl",
          "**/classes/wrongType5.pkl",
          "**/classes/duplicateProperty.pkl",
          "**/classes/duplicateFunction.pkl",
          "**/api/pListRenderer2.plist.pkl",
          "**/api/pcfRenderer2.pkl",
          "**/api/propertiesRenderer2.properties.pkl",
          "**/api/propertiesRenderer3.properties.pkl",
          "**/api/protobuf.pkl",
          "**/api/typed.pkl",
          "**/api/xmlRenderer2.xml.pkl",
          "**/api/xmlRendererInline2.xml.pkl",
          "**/api/yamlParser1Compat.pkl",
          "**/api/api/yamlRenderer2.yml.pkl",
          "**/api/yamlRendererStringsCompat.pkl",
          "**/api/yamlRenderer2.yml.pkl",
          "**/basic/comments.pkl",
          "**/basic/floatLiteral.pkl",
          "**/basic/identifier.pkl",
          "**/basic/importGlob.pkl",
          "**/basic/int.pkl",
          "**/basic/float.pkl",
          "**/classes/inheritanceError1.pkl",

          // these files contain escape sequences in the string, which are merged into a single node
          // by pkl's parser
          // but emitted individually by tree-sitter.
          "**/api/benchmarkModule.pkl",
          "**/api/jsonnetRenderer1.jsonnet.pkl",
          "**/api/jsonnetRenderer2.jsonnet.pkl",
          "**/api/listing.pkl",
          "**/api/pcfRenderer7.pkl",
          "**/api/propertiesRenderer6.properties.pkl",
          "**/api/propertiesRenderer7.properties.pkl",
          "**/api/renderDirective.pkl",
          "**/api/string.pkl",
          "**/api/stringUnicode.pkl",
          "**/api/xmlRendererValidation10.pkl",
          "**/api/xmlRendererValidation11.pkl",
          "**/api/yamlRendererKeys.yml.pkl",
          "**/api/yamlRendererStream2.pkl",
          "**/api/yamlRendererStrings.yml.pkl",
          "**/api/shellModule.pkl",
          "**/errors/invalidGlobImport2.pkl",
          "**/errors/invalidGlobImport3.pkl",
          "**/basic/rawString.pkl",
          "**/basic/string.pkl",
          "**/basic/stringMultiline.pkl",
          "**/basic/stringUnicode.pkl",
          "**/classes/constraints10.pkl",
        )
      }
    inputs.files(snippetTestFiles)
    val outputDir = layout.projectDirectory.dir("test/corpus/snippetTests")
    outputs.files(
      snippetTestFiles.map {
        val relativePath = snippetTestDir.toPath().relativize(it.toPath()).toString()
        outputDir.asFile.resolve(relativePath).withReplacedExtension("pkl", "txt")
      }
    )
    doLast {
      for (file in snippetTestFiles) {
        val relativePath = snippetTestDir.toPath().relativize(file.toPath()).toString()
        val text = file.readText().stripLineComments()
        val rendered = SExprRenderer.render(text, file.name)
        val outputText = buildString {
          appendLine("===")
          appendLine(relativePath)
          appendLine("===")
          appendLine()
          appendLine(text)
          appendLine("---")
          appendLine()
          append(rendered)
        }
        val outputFile = outputDir.asFile.resolve(relativePath).withReplacedExtension("pkl", "txt")
        outputFile.writeText(outputText)
      }
    }
  }

repositories { mavenCentral() }

val licenseHeaderText =
  """/*
 * Copyright © ${'$'}YEAR Apple Inc. and the Pkl project authors. All rights reserved.
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
"""

val originalRemoteName = System.getenv("PKL_ORIGINAL_REMOTE_NAME") ?: "origin"

spotless {
  ratchetFrom = "$originalRemoteName/main"

  kotlinGradle {
    licenseHeader(licenseHeaderText, "([a-zA-Z]|@file|//)")
    ktfmt(libs.versions.ktfmt.get()).googleStyle()
    target("*.kts", "buildSrc/*.kts", "buildSrc/src/*/kotlin/**/*.kts")
  }

  kotlin {
    licenseHeader(licenseHeaderText)
    ktfmt(libs.versions.ktfmt.get()).googleStyle()
    target("buildSrc/src/*/kotlin/**/*.kt")
  }
}
