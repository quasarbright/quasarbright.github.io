import Foundation

/// Every message that hasn't been sent or deleted yet, ordered by send time.
public struct MessageQueue: Equatable {
    public private(set) var messages: [ScheduledMessage]

    public init(_ messages: [ScheduledMessage] = []) {
        self.messages = messages.sorted(by: Self.sendOrder)
    }

    /// Earliest first. Ties are broken by id so the order is stable.
    static func sendOrder(_ a: ScheduledMessage, _ b: ScheduledMessage) -> Bool {
        if a.sendAt != b.sendAt { return a.sendAt < b.sendAt }
        return a.id.uuidString < b.id.uuidString
    }

    public var isEmpty: Bool { messages.isEmpty }

    public func message(id: UUID) -> ScheduledMessage? {
        messages.first { $0.id == id }
    }

    /// Adds the message, or replaces the one with the same id.
    public mutating func upsert(_ message: ScheduledMessage) {
        messages.removeAll { $0.id == message.id }
        messages.append(message)
        messages.sort(by: Self.sendOrder)
    }

    @discardableResult
    public mutating func remove(id: UUID) -> ScheduledMessage? {
        guard let index = messages.firstIndex(where: { $0.id == id }) else { return nil }
        return messages.remove(at: index)
    }

    /// Messages whose time has come and are waiting to be sent, oldest first.
    public func due(at now: Date) -> [ScheduledMessage] {
        messages.filter { $0.isDue(at: now) }
    }

    /// Messages whose time hasn't come yet, soonest first.
    public func upcoming(at now: Date) -> [ScheduledMessage] {
        messages.filter { !$0.isDue(at: now) }
    }

    /// The app icon badge: how many texts are waiting to be sent.
    public func badgeCount(at now: Date) -> Int {
        due(at: now).count
    }
}
