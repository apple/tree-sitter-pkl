package tree_sitter_pkl_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_pkl "github.com/apple/tree-sitter-pkl/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_pkl.Language())
	if language == nil {
		t.Errorf("Error loading Pkl grammar")
	}
}
