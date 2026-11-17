import SwiftUI

@main
struct ScheduledTextsApp: App {
    @UIApplicationDelegateAdaptor(AppDelegate.self) private var appDelegate
    @Environment(\.scenePhase) private var scenePhase

    var body: some Scene {
        WindowGroup {
            ContentView()
                .environmentObject(MessageStore.shared)
                .environmentObject(Router.shared)
        }
        .onChange(of: scenePhase) { _, phase in
            // Catch up on anything that changed while the app was closed:
            // the badge, cleared notifications, and the 64-notification cap.
            if phase == .active {
                MessageStore.shared.resync()
            }
        }
    }
}
