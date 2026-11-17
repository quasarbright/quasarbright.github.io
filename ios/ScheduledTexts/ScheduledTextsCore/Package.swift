// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "ScheduledTextsCore",
    platforms: [.iOS(.v17), .macOS(.v14)],
    products: [
        .library(name: "ScheduledTextsCore", targets: ["ScheduledTextsCore"]),
    ],
    targets: [
        .target(name: "ScheduledTextsCore"),
        .testTarget(name: "ScheduledTextsCoreTests", dependencies: ["ScheduledTextsCore"]),
    ]
)
