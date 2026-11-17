import ScheduledTextsCore
import SwiftUI
import UIKit

/// The main screen: texts waiting to be sent, then texts scheduled for later.
struct ContentView: View {
    @EnvironmentObject private var store: MessageStore
    @EnvironmentObject private var router: Router
    @Environment(\.scenePhase) private var scenePhase

    @State private var notificationsAllowed = true
    @State private var unsendable: ScheduledMessage?

    var body: some View {
        NavigationStack {
            // Re-render every 15 seconds so texts move from Scheduled to
            // Ready to Send when their time comes.
            TimelineView(.periodic(from: .now, by: 15)) { context in
                list(now: context.date)
            }
            .navigationTitle("Scheduled Texts")
            .toolbar {
                ToolbarItem(placement: .primaryAction) {
                    Button {
                        router.open(.new)
                    } label: {
                        Label("New Text", systemImage: "plus")
                    }
                }
            }
        }
        .sheet(item: $router.sheet) { route in
            sheet(for: route)
        }
        .onReceive(router.$pendingSend.compactMap { $0 }) { id in
            router.pendingSend = nil
            send(id)
        }
        .task {
            await NotificationScheduler.requestAuthorization()
            notificationsAllowed = await NotificationScheduler.isAuthorized()
        }
        .onChange(of: scenePhase) { _, phase in
            guard phase == .active else { return }
            Task { notificationsAllowed = await NotificationScheduler.isAuthorized() }
        }
        .alert(
            "This iPhone can't send texts",
            isPresented: Binding(get: { unsendable != nil }, set: { if !$0 { unsendable = nil } }),
            presenting: unsendable
        ) { message in
            Button("Copy Message") { UIPasteboard.general.string = message.body }
            Button("OK", role: .cancel) {}
        } message: { message in
            Text("Messages isn't available here. You can copy the text and send it to \(message.displayRecipient) yourself.")
        }
    }

    @ViewBuilder
    private func list(now: Date) -> some View {
        let due = store.queue.due(at: now)
        let upcoming = store.queue.upcoming(at: now)

        List {
            if !notificationsAllowed {
                Section {
                    NotificationsOffNotice()
                }
            }

            if !due.isEmpty {
                Section("Ready to Send") {
                    ForEach(due) { message in
                        row(message, now: now, route: .review(message.id))
                    }
                }
            }

            if !upcoming.isEmpty {
                Section("Scheduled") {
                    ForEach(upcoming) { message in
                        row(message, now: now, route: .edit(message.id))
                    }
                }
            }
        }
        .overlay {
            if store.queue.isEmpty && notificationsAllowed {
                ContentUnavailableView {
                    Label("No Scheduled Texts", systemImage: "clock")
                } description: {
                    Text("Write a text now and get a reminder to send it later.")
                } actions: {
                    Button("New Text") { router.open(.new) }
                        .buttonStyle(.borderedProminent)
                }
            }
        }
    }

    private func row(_ message: ScheduledMessage, now: Date, route: Route) -> some View {
        Button {
            router.open(route)
        } label: {
            MessageRow(message: message, now: now)
        }
        .buttonStyle(.plain)
        .swipeActions(edge: .trailing) {
            Button(role: .destructive) {
                store.delete(id: message.id)
            } label: {
                Label("Don't Send", systemImage: "trash")
            }
        }
        .swipeActions(edge: .leading) {
            Button {
                send(message.id)
            } label: {
                Label(message.isDue(at: now) ? "Send" : "Send Now", systemImage: "paperplane")
            }
            .tint(.blue)
        }
    }

    @ViewBuilder
    private func sheet(for route: Route) -> some View {
        switch route {
        case .new:
            MessageEditorView(existing: nil)
        case .edit(let id):
            if let message = store.message(id: id) {
                MessageEditorView(existing: message)
            } else {
                MissingMessageView()
            }
        case .review(let id):
            if let message = store.message(id: id) {
                ReviewView(message: message)
            } else {
                MissingMessageView()
            }
        case .send:
            // Never shown as a sheet; see Router.open.
            EmptyView()
        }
    }

    /// Opens the compose sheet and removes the text once it's sent. If the
    /// person backs out, the text stays in the list.
    private func send(_ id: UUID) {
        guard let message = store.message(id: id) else { return }
        guard MessageSender.canSendText else {
            unsendable = message
            return
        }
        Task {
            if await MessageSender.shared.compose(message) == .sent {
                store.markSent(id: id)
            }
        }
    }
}

struct MessageRow: View {
    let message: ScheduledMessage
    let now: Date

    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            HStack(alignment: .firstTextBaseline) {
                Text(message.displayRecipient)
                    .font(.headline)
                    .lineLimit(1)
                Spacer()
                Text(message.sendAt, format: .dateTime.month(.abbreviated).day().hour().minute())
                    .font(.subheadline)
                    .foregroundStyle(message.isDue(at: now) ? Color.orange : Color.secondary)
            }
            Text(message.body)
                .font(.subheadline)
                .foregroundStyle(.secondary)
                .lineLimit(2)
        }
        .padding(.vertical, 2)
        .frame(maxWidth: .infinity, alignment: .leading)
        .contentShape(Rectangle())
    }
}

private struct NotificationsOffNotice: View {
    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            Label("Notifications are off", systemImage: "bell.slash")
                .font(.headline)
            Text("You won't be reminded when it's time to send. Texts still show up here.")
                .font(.subheadline)
                .foregroundStyle(.secondary)
            Button("Turn On in Settings") {
                if let url = URL(string: UIApplication.openNotificationSettingsURLString) {
                    UIApplication.shared.open(url)
                }
            }
        }
        .padding(.vertical, 4)
    }
}

struct MissingMessageView: View {
    @EnvironmentObject private var router: Router

    var body: some View {
        NavigationStack {
            ContentUnavailableView(
                "Already Handled",
                systemImage: "checkmark.circle",
                description: Text("This text was already sent or deleted.")
            )
            .toolbar {
                ToolbarItem(placement: .confirmationAction) {
                    Button("Done") { router.close() }
                }
            }
        }
    }
}
