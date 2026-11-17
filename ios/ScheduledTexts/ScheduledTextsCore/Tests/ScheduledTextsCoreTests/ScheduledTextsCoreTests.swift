import Foundation
import XCTest
@testable import ScheduledTextsCore

final class ScheduledMessageTests: XCTestCase {
    func testDisplayRecipientPrefersName() {
        let named = ScheduledMessage(recipientName: "Alex", recipientAddress: "+15551234567", body: "hi", sendAt: Date())
        XCTAssertEqual(named.displayRecipient, "Alex")

        let blankName = ScheduledMessage(recipientName: "  ", recipientAddress: "+15551234567", body: "hi", sendAt: Date())
        XCTAssertEqual(blankName.displayRecipient, "+15551234567")

        let noName = ScheduledMessage(recipientAddress: "alex@example.com", body: "hi", sendAt: Date())
        XCTAssertEqual(noName.displayRecipient, "alex@example.com")
    }

    func testIsDue() {
        let now = Date(timeIntervalSince1970: 1_000_000)
        let message = ScheduledMessage(recipientAddress: "1", body: "hi", sendAt: now)
        XCTAssertTrue(message.isDue(at: now))
        XCTAssertTrue(message.isDue(at: now.addingTimeInterval(1)))
        XCTAssertFalse(message.isDue(at: now.addingTimeInterval(-1)))
    }

    func testIsValid() {
        let date = Date()
        XCTAssertTrue(ScheduledMessage(recipientAddress: "1", body: "hi", sendAt: date).isValid)
        XCTAssertFalse(ScheduledMessage(recipientAddress: " ", body: "hi", sendAt: date).isValid)
        XCTAssertFalse(ScheduledMessage(recipientAddress: "1", body: "\n ", sendAt: date).isValid)
    }

    func testNormalized() {
        var calendar = Calendar(identifier: .gregorian)
        calendar.timeZone = TimeZone(identifier: "UTC")!
        // 1970-01-12 13:46:40 UTC
        let date = Date(timeIntervalSince1970: 1_000_000.75)
        let message = ScheduledMessage(recipientName: " ", recipientAddress: " +1 555 ", body: " hey \n", sendAt: date)
        let normalized = message.normalized(calendar: calendar)

        XCTAssertEqual(normalized.recipientAddress, "+1 555")
        XCTAssertEqual(normalized.body, "hey")
        XCTAssertNil(normalized.recipientName)
        XCTAssertEqual(normalized.sendAt, Date(timeIntervalSince1970: 999_960))
        XCTAssertEqual(normalized.id, message.id)
    }
}

final class MessageQueueTests: XCTestCase {
    let now = Date(timeIntervalSince1970: 1_000_000)

    func message(_ offset: TimeInterval, _ body: String = "hi") -> ScheduledMessage {
        ScheduledMessage(recipientAddress: "1", body: body, sendAt: now.addingTimeInterval(offset))
    }

    func testSortsBySendTime() {
        let later = message(60)
        let earlier = message(-60)
        let middle = message(0)
        let queue = MessageQueue([later, earlier, middle])
        XCTAssertEqual(queue.messages, [earlier, middle, later])
    }

    func testUpsertAddsAndReplaces() {
        var queue = MessageQueue()
        var first = message(60, "first")
        queue.upsert(first)
        queue.upsert(message(30, "second"))
        XCTAssertEqual(queue.messages.map(\.body), ["second", "first"])

        first.body = "edited"
        first.sendAt = now.addingTimeInterval(10)
        queue.upsert(first)
        XCTAssertEqual(queue.messages.map(\.body), ["edited", "second"])
        XCTAssertEqual(queue.messages.count, 2)
    }

    func testRemove() {
        let keep = message(10)
        let drop = message(20)
        var queue = MessageQueue([keep, drop])
        XCTAssertEqual(queue.remove(id: drop.id), drop)
        XCTAssertNil(queue.remove(id: drop.id))
        XCTAssertEqual(queue.messages, [keep])
    }

    func testDueAndUpcoming() {
        let past = message(-3600)
        let exactlyNow = message(0)
        let future = message(3600)
        let queue = MessageQueue([future, past, exactlyNow])

        XCTAssertEqual(queue.due(at: now), [past, exactlyNow])
        XCTAssertEqual(queue.upcoming(at: now), [future])
        XCTAssertEqual(queue.badgeCount(at: now), 2)
        XCTAssertEqual(queue.message(id: future.id), future)
    }
}

final class NotificationPlannerTests: XCTestCase {
    let now = Date(timeIntervalSince1970: 1_000_000)

    func testSkipsDueMessagesAndCountsThemInBadges() {
        let due = ScheduledMessage(recipientAddress: "1", body: "old", sendAt: now.addingTimeInterval(-60))
        let soon = ScheduledMessage(recipientName: "Alex", recipientAddress: "1", body: "soon", sendAt: now.addingTimeInterval(60))
        let later = ScheduledMessage(recipientAddress: "+15550000", body: "later", sendAt: now.addingTimeInterval(120))
        let plan = NotificationPlanner.plan(for: MessageQueue([later, due, soon]), now: now)

        XCTAssertEqual(plan, [
            PlannedNotification(id: soon.id.uuidString, fireDate: soon.sendAt, title: "Send text to Alex?", body: "soon", badge: 2),
            PlannedNotification(id: later.id.uuidString, fireDate: later.sendAt, title: "Send text to +15550000?", body: "later", badge: 3),
        ])
    }

    func testCapsAtPendingLimit() {
        let messages = (1...100).map {
            ScheduledMessage(recipientAddress: "1", body: "\($0)", sendAt: now.addingTimeInterval(TimeInterval($0)))
        }
        let plan = NotificationPlanner.plan(for: MessageQueue(messages), now: now)
        XCTAssertEqual(plan.count, NotificationPlanner.maxPending)
        XCTAssertEqual(plan.first?.body, "1")
        XCTAssertEqual(plan.last?.body, "64")
    }

    func testNotificationIDRoundTrip() {
        let id = UUID()
        XCTAssertEqual(NotificationPlanner.messageID(fromNotificationID: NotificationPlanner.notificationID(for: id)), id)
        XCTAssertNil(NotificationPlanner.messageID(fromNotificationID: "not-a-uuid"))
    }
}

final class MessageStorageTests: XCTestCase {
    func testRoundTrip() throws {
        let queue = MessageQueue([
            ScheduledMessage(recipientName: "Alex", recipientAddress: "+15551234567", body: "hello", sendAt: Date(timeIntervalSince1970: 1_000_000.5)),
            ScheduledMessage(recipientAddress: "sam@example.com", body: "hi\nthere", sendAt: Date(timeIntervalSince1970: 2_000_000)),
        ])
        XCTAssertEqual(try MessageStorage.decode(MessageStorage.encode(queue)), queue)
    }

    func testFileRoundTripAndMissingFile() throws {
        let dir = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        defer { try? FileManager.default.removeItem(at: dir) }
        let url = dir.appendingPathComponent("nested/messages.json")

        XCTAssertEqual(try MessageStorage.load(from: url), MessageQueue())

        let queue = MessageQueue([ScheduledMessage(recipientAddress: "1", body: "hi", sendAt: Date(timeIntervalSince1970: 5))])
        try MessageStorage.save(queue, to: url)
        XCTAssertEqual(try MessageStorage.load(from: url), queue)
    }

    func testRejectsNewerVersionsAndGarbage() {
        let newer = Data(#"{"version": 99, "messages": []}"#.utf8)
        XCTAssertThrowsError(try MessageStorage.decode(newer)) { error in
            XCTAssertEqual(error as? MessageStorage.StorageError, .unsupportedVersion(99))
        }
        XCTAssertThrowsError(try MessageStorage.decode(Data("nope".utf8)))
    }
}
