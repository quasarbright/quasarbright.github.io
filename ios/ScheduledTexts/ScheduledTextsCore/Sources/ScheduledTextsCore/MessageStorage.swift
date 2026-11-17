import Foundation

/// Reads and writes the queue as a small JSON file.
public enum MessageStorage {
    public static let currentVersion = 1

    struct File: Codable {
        var version: Int
        var messages: [ScheduledMessage]
    }

    public enum StorageError: Error, Equatable {
        case unsupportedVersion(Int)
    }

    public static func encode(_ queue: MessageQueue) throws -> Data {
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
        return try encoder.encode(File(version: currentVersion, messages: queue.messages))
    }

    public static func decode(_ data: Data) throws -> MessageQueue {
        let file = try JSONDecoder().decode(File.self, from: data)
        guard file.version <= currentVersion else {
            throw StorageError.unsupportedVersion(file.version)
        }
        return MessageQueue(file.messages)
    }

    /// An empty queue if the file doesn't exist yet. Throws if it exists but
    /// can't be read, so the caller can keep the file instead of
    /// overwriting the person's messages.
    public static func load(from url: URL) throws -> MessageQueue {
        guard FileManager.default.fileExists(atPath: url.path) else { return MessageQueue() }
        return try decode(Data(contentsOf: url))
    }

    public static func save(_ queue: MessageQueue, to url: URL) throws {
        try FileManager.default.createDirectory(
            at: url.deletingLastPathComponent(),
            withIntermediateDirectories: true
        )
        try encode(queue).write(to: url, options: .atomic)
    }
}
