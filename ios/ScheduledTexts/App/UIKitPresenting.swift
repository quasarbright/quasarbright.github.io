import UIKit

extension UIApplication {
    /// The view controller on top of the key window, including any sheet.
    @MainActor
    var topViewController: UIViewController? {
        let scenes = connectedScenes.compactMap { $0 as? UIWindowScene }
        let windows = scenes.flatMap(\.windows)
        var top = (windows.first { $0.isKeyWindow } ?? windows.first)?.rootViewController
        while let presented = top?.presentedViewController {
            top = presented
        }
        return top
    }

    /// Presents a UIKit screen over whatever is showing.
    ///
    /// SwiftUI sheets take a moment to appear or go away, and UIKit refuses
    /// to present during that. A tap that launches the app also lands
    /// before the window exists. So this waits up to two seconds for a
    /// settled screen to present from.
    @MainActor
    func presentWhenReady(_ viewController: UIViewController) async -> Bool {
        for _ in 0..<20 {
            if let top = topViewController, !top.isBeingPresented, !top.isBeingDismissed {
                top.present(viewController, animated: true)
                return true
            }
            try? await Task.sleep(nanoseconds: 100_000_000)
        }
        return false
    }
}
