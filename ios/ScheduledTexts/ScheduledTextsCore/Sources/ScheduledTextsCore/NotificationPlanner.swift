import Foundation

/// One local notification the app should have scheduled.
public struct PlannedNotification: Equatable {
    /// The notification identifier. It is the message id, so a tap on the
    /// notification leads back to its message.
    public let id: String
    public let fireDate: Date
    public let title: String
    public let body: String
    /// The badge to show when this notification arrives.
    public let badge: Int
}

/// Decides which notifications should exist for a queue. Kept free of
/// UserNotifications so it can be unit tested anywhere.
public enum NotificationPlanner {
    /// iOS keeps at most 64 pending local notifications per app and drops
    /// the rest, so only the soonest 64 are scheduled. The app re-plans
    /// every time it opens or the queue changes, so later ones get their
    /// turn.
    public static let maxPending = 64

    public static func plan(for queue: MessageQueue, now: Date) -> [PlannedNotification] {
        var planned: [PlannedNotification] = []
        // `queue.messages` is in send order, so when a message fires, every
        // message before it is already due. Unless the person sends some in
        // the meantime, that makes the badge its position plus one.
        for (index, message) in queue.messages.enumerated() where !message.isDue(at: now) {
            planned.append(PlannedNotification(
                id: notificationID(for: message.id),
                fireDate: message.sendAt,
                title: title(for: message),
                body: message.body,
                badge: index + 1
            ))
            if planned.count == maxPending { break }
        }
        return planned
    }

    public static func title(for message: ScheduledMessage) -> String {
        "Send text to \(message.displayRecipient)?"
    }

    public static func notificationID(for messageID: UUID) -> String {
        messageID.uuidString
    }

    public static func messageID(fromNotificationID id: String) -> UUID? {
        UUID(uuidString: id)
    }
}
