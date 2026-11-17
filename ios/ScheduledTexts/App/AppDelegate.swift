import ScheduledTextsCore
import UIKit
import UserNotifications

/// Receives taps and long-press actions on the "Send text?" notifications.
final class AppDelegate: NSObject, UIApplicationDelegate, UNUserNotificationCenterDelegate {
    func application(
        _ application: UIApplication,
        didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]? = nil
    ) -> Bool {
        // The delegate has to be set before launch finishes, or a tap that
        // launched the app is lost.
        UNUserNotificationCenter.current().delegate = self
        NotificationScheduler.registerCategories()
        return true
    }

    /// Show the notification even when the app is open.
    func userNotificationCenter(
        _ center: UNUserNotificationCenter,
        willPresent notification: UNNotification,
        withCompletionHandler completionHandler: @escaping (UNNotificationPresentationOptions) -> Void
    ) {
        completionHandler([.banner, .list, .sound, .badge])
    }

    func userNotificationCenter(
        _ center: UNUserNotificationCenter,
        didReceive response: UNNotificationResponse,
        withCompletionHandler completionHandler: @escaping () -> Void
    ) {
        let identifier = response.notification.request.identifier
        let action = response.actionIdentifier
        guard let id = NotificationPlanner.messageID(fromNotificationID: identifier) else {
            completionHandler()
            return
        }

        Task { @MainActor in
            switch action {
            case NotificationScheduler.Action.send:
                // Opens the app straight into the prefilled compose sheet.
                Router.shared.open(.send(id))
            case NotificationScheduler.Action.edit:
                Router.shared.open(.edit(id))
            case NotificationScheduler.Action.cancel:
                // Runs in the background without opening the app. Wait for
                // the notification cleanup before telling iOS we're done.
                MessageStore.shared.delete(id: id)
                await MessageStore.shared.waitForSync()
            case UNNotificationDefaultActionIdentifier:
                // A plain tap opens the app on a "Send this text?" screen.
                Router.shared.open(.review(id))
            default:
                // Swiped away: the message stays in the app's list.
                break
            }
            completionHandler()
        }
    }
}
