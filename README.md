A tree-sitter grammar for Pkl.

    # install dependencies
    $ npm install

    # build parser
    $ npm run build

    # parse some code
    $ ./node_modules/.bin/tree-sitter parse test.pkl

# Tests

Tree sitter comes with its own test framework. Files in `corpus/`
describe one test each. All tests in `corpus/` are performed by the
command

    $ tree-sitter test

The script `src/synctests.ts` creates `corpus/` test files from tests in
`pkl`'s `LanguageSnippetTests`. The script assumes that `../pkl` is a
checked out `pkl` repository (and that the tests are under
`pkl-core/src/test/files/LanguageSnippetFiles/input`). A pre-condition
of the script is that `tree-sitter test` currently passes. If broken
tests need replacing, the broken tests must first be deleted.

It can be executed by running

    $ npm run synctests

# Upgrading tree-sitter

Upgrading tree-sitter involves upgrading the NPM package.

1.  Run `npm update tree-sitter` to install the newer version of tree-sitter.
2.  Commit to main, and push.

# Releasing

1.  Run the build & test to make sure everything is up-to-date and passes (check 0 diff).
2.  Create a `Prepare 1.2.3 release` (with appropriate version number) commit where
  - Versions are bumped in `package.json`, `Cargo.toml`
  - Lockfiles are updated (`npm install`, `cargo check`)
  - You have checked the previous release PR for any changes in process not described in this `README.adoc`; if any
    - Adopt the changes accordingly
    - Update this description to capture the changed process
3. Merge into `main` & push
4. Check that CI release succeeded ([release pipeline](https://app.circleci.com/pipelines/github/apple/tree-sitter-pkl))
5. Check the publication is reachable, [on NPM](https://www.npmjs.com/package/@apple/tree-sitter-pkl)

# Resources
-   [Tree-sitter docs](https://tree-sitter.github.io/tree-sitter/)
-   [Guide to your first Tree-sitter grammar](https://gist.github.com/Aerijo/df27228d70c633e088b0591b8857eeef)
