import XCTest
import SwiftTreeSitter
import TreeSitterPkl

final class TreeSitterPklTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_pkl())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Pkl grammar")
    }
}
