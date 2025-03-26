A tree-sitter grammar for Pkl.

    # install dependencies
    $ npm install

    # build parser
    $ npm run build

    # parse some code
    $ ./node_modules/.bin/tree-sitter parse test.pkl

# Tests

Tree sitter comes with its own test framework. Files in `test/corpus/`
describe one test each. All tests in `test/corpus/` are performed by the
command

    $ tree-sitter test

If the grammar has changed, run `tree-sitter test --update` to update the corpus.

## Snippet Tests

The tests within [test/corpus/snippetTests](test/corpus/snippetTests) are generated from the core Pkl parser, and are not updated by `tree-sitter test --update`.

These tests are generated by running `./gradlew createLanguageSnippetsCorpus`.
This task grabs all language snippet tests, and renders them into a format that can be processed by tree-sitter.

To fix any rendering issues, change [SExprRenderer](buildSrc/src/main/kotlin/SExprRenderer.kt).

# Upgrading tree-sitter

Upgrading tree-sitter involves upgrading the NPM package.

1.  Run `npm update tree-sitter` to install the newer version of tree-sitter.
2.  Commit to main, and push.

# Resources
-   [Tree-sitter docs](https://tree-sitter.github.io/tree-sitter/)
-   [Guide to your first Tree-sitter grammar](https://gist.github.com/Aerijo/df27228d70c633e088b0591b8857eeef)
