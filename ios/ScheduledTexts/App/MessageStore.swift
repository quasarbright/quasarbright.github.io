import Foundation
import ScheduledTextsCore

/// The app's copy of the queue. Saves to disk and updates notifications
/// after every change.
@MainActor
final class MessageStore: ObservableObject {
    static let shared = MessageStore()

    @Published private(set) var queue: MessageQueue

    private let fileURL: URL
    private var syncTask: Task<Void, Never>?

    init(fileURL: URL = MessageStore.defaultFileURL) {
        self.fileURL = fileURL
        do {
            queue = try MessageStorage.load(from: fileURL)
        } catch {
            // Keep the unreadable file around rather than overwrite it on
            // the next save.
            print("Couldn't read \(fileURL.path): \(error)")
            let backup = fileURL.deletingPathExtension()
                .appendingPathExtension("unreadable-\(Int(Date().timeIntervalSince1970)).json")
            try? FileManager.default.moveItem(at: fileURL, to: backup)
            queue = MessageQueue()
        }
    }

    nonisolated static var defaultFileURL: URL {
        FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("messages.json")
    }

    func message(id: UUID) -> ScheduledMessage? {
        queue.message(id: id)
    }

    /// Adds a new message or saves changes to an existing one.
    func save(_ message: ScheduledMessage) {
        queue.upsert(message.normalized())
        persist()
        resync()
    }

    func delete(id: UUID) {
        guard queue.remove(id: id) != nil else { return }
        persist()
        resync()
    }

    /// The person tapped Send in the compose sheet, so the text is done.
    func markSent(id: UUID) {
        delete(id: id)
    }

    /// Brings notifications and the badge up to date. Syncs run one at a
    /// time and each one reads the queue when it starts, so the last one
    /// always reflects the latest state.
    @discardableResult
    func resync() -> Task<Void, Never> {
        let previous = syncTask
        let task = Task {
            await previous?.value
            await NotificationScheduler.sync(with: self.queue)
        }
        syncTask = task
        return task
    }

    func waitForSync() async {
        await syncTask?.value
    }

    private func persist() {
        do {
            try MessageStorage.save(queue, to: fileURL)
        } catch {
            print("Couldn't save messages: \(error)")
        }
    }
}
