import MessageUI
import ScheduledTextsCore
import UIKit

/// Opens the system compose sheet with the recipient and text filled in.
///
/// This is as far as iOS lets an app go: the person still taps Send. The
/// same sheet sends iMessage to Apple devices and SMS to everyone else.
@MainActor
final class MessageSender: NSObject, MFMessageComposeViewControllerDelegate {
    static let shared = MessageSender()

    enum Outcome {
        case sent
        case cancelled
        case failed
    }

    /// False on the simulator and on devices with Messages turned off.
    static var canSendText: Bool {
        MFMessageComposeViewController.canSendText()
    }

    private var continuation: CheckedContinuation<Outcome, Never>?

    func compose(_ message: ScheduledMessage) async -> Outcome {
        guard Self.canSendText, continuation == nil else { return .failed }

        let composer = MFMessageComposeViewController()
        composer.recipients = [message.recipientAddress]
        composer.body = message.body
        composer.messageComposeDelegate = self

        return await withCheckedContinuation { continuation in
            self.continuation = continuation
            Task {
                if await !UIApplication.shared.presentWhenReady(composer) {
                    self.finish(.failed)
                }
            }
        }
    }

    nonisolated func messageComposeViewController(
        _ controller: MFMessageComposeViewController,
        didFinishWith result: MessageComposeResult
    ) {
        let outcome: Outcome
        switch result {
        case .sent: outcome = .sent
        case .cancelled: outcome = .cancelled
        default: outcome = .failed
        }
        MainActor.assumeIsolated {
            controller.dismiss(animated: true) {
                Task { @MainActor in self.finish(outcome) }
            }
        }
    }

    private func finish(_ outcome: Outcome) {
        continuation?.resume(returning: outcome)
        continuation = nil
    }
}
