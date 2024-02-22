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
