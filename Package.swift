// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "TreeSitterPkl",
    products: [
        .library(name: "TreeSitterPkl", targets: ["TreeSitterPkl"]),
    ],
    dependencies: [],
    targets: [
        .target(name: "TreeSitterPkl",
                path: ".",
                exclude: [
                    "binding.gyp",
                    "bindings",
                    "Cargo.lock",
                    "Cargo.toml",
                    "corpus",
                    "grammar.js",
                    "LICENSE.txt",
                    "NOTICE.txt",
                    "package.json",
                    "package-lock.json",
                    "tsconfig.json",
                    "README.adoc",
                    "SECURITY.adoc",
                    "MAINTAINERS.adoc",
                    "CODE_OF_CONDUCT.adoc",
                    "CONTRIBUTING.adoc",
                    "src/grammar.json",
                    "src/node-types.json"
                ],
                sources: [
                    "src/parser.c",
                    "src/scanner.c",
                ],
                resources: [
                    .copy("queries"),
                ],
                publicHeadersPath: "bindings/swift",
                cSettings: [.headerSearchPath("src")]
        )
    ]
)
