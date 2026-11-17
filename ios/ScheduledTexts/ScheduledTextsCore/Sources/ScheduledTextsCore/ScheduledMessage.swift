import Foundation

/// A text the person wants to send later.
///
/// The app never sends on its own: iOS only lets an app open a prefilled
/// compose sheet, and the person has to tap Send there. So a message stays
/// in the queue until it's sent or deleted.
public struct ScheduledMessage: Codable, Identifiable, Equatable, Hashable {
    public var id: UUID
    /// Display name, for example from Contacts. Nil when the person typed a number.
    public var recipientName: String?
    /// The phone number or email address the compose sheet sends to.
    /// iMessage and SMS both go through the same compose sheet.
    public var recipientAddress: String
    public var body: String
    public var sendAt: Date

    public init(
        id: UUID = UUID(),
        recipientName: String? = nil,
        recipientAddress: String,
        body: String,
        sendAt: Date
    ) {
        self.id = id
        self.recipientName = recipientName
        self.recipientAddress = recipientAddress
        self.body = body
        self.sendAt = sendAt
    }

    /// The name if there is one, otherwise the number or address.
    public var displayRecipient: String {
        if let name = recipientName?.trimmingCharacters(in: .whitespacesAndNewlines), !name.isEmpty {
            return name
        }
        return recipientAddress
    }

    /// True once the send time has arrived. A due message waits for the
    /// person to send it; it doesn't expire.
    public func isDue(at now: Date) -> Bool {
        sendAt <= now
    }

    /// A message needs someone to send to and something to say.
    public var isValid: Bool {
        !recipientAddress.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
            && !body.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
    }

    /// A copy with surrounding whitespace removed from the address and body,
    /// and the send time rounded down to the minute (the date picker has
    /// minute precision, so stray seconds would only be confusing).
    public func normalized(calendar: Calendar = .current) -> ScheduledMessage {
        var copy = self
        copy.recipientAddress = recipientAddress.trimmingCharacters(in: .whitespacesAndNewlines)
        copy.body = body.trimmingCharacters(in: .whitespacesAndNewlines)
        copy.sendAt = sendAt.truncatedToMinute(calendar: calendar)
        if let name = recipientName?.trimmingCharacters(in: .whitespacesAndNewlines) {
            copy.recipientName = name.isEmpty ? nil : name
        }
        return copy
    }
}

extension Date {
    /// This date with its seconds and fractional seconds dropped.
    public func truncatedToMinute(calendar: Calendar = .current) -> Date {
        let parts = calendar.dateComponents([.year, .month, .day, .hour, .minute], from: self)
        return calendar.date(from: parts) ?? self
    }
}
