package tree_sitter_pkl_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-pkl"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_pkl.Language())
	if language == nil {
		t.Errorf("Error loading Pkl grammar")
	}
}
