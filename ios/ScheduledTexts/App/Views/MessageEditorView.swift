import ScheduledTextsCore
import SwiftUI

/// Create a new scheduled text, or change one.
struct MessageEditorView: View {
    let existing: ScheduledMessage?

    @EnvironmentObject private var store: MessageStore
    @EnvironmentObject private var router: Router

    @State private var recipientName: String?
    @State private var recipientAddress: String
    @State private var text: String
    @State private var sendAt: Date
    /// The address that came with `recipientName`. Typing a different one
    /// drops the name, so the list never shows the wrong person.
    @State private var pickedAddress: String?

    private let id: UUID

    init(existing: ScheduledMessage?) {
        self.existing = existing
        id = existing?.id ?? UUID()
        _recipientName = State(initialValue: existing?.recipientName)
        _recipientAddress = State(initialValue: existing?.recipientAddress ?? "")
        _pickedAddress = State(initialValue: existing?.recipientName == nil ? nil : existing?.recipientAddress)
        _text = State(initialValue: existing?.body ?? "")
        _sendAt = State(initialValue: existing?.sendAt ?? Self.defaultSendTime())
    }

    /// An hour from now, on the minute.
    private static func defaultSendTime() -> Date {
        Date().addingTimeInterval(60 * 60).truncatedToMinute()
    }

    private var draft: ScheduledMessage {
        ScheduledMessage(
            id: id,
            recipientName: recipientName,
            recipientAddress: recipientAddress,
            body: text,
            sendAt: sendAt
        )
    }

    var body: some View {
        NavigationStack {
            Form {
                Section("To") {
                    if let recipientName {
                        HStack {
                            Image(systemName: "person.crop.circle.fill")
                                .foregroundStyle(.secondary)
                            Text(recipientName)
                        }
                    }
                    TextField("Phone number or email", text: $recipientAddress)
                        .textContentType(.telephoneNumber)
                        .textInputAutocapitalization(.never)
                        .autocorrectionDisabled()
                    Button {
                        Task {
                            if let pick = await ContactPicker.shared.pick() {
                                recipientName = pick.name
                                recipientAddress = pick.address
                                pickedAddress = pick.address
                            }
                        }
                    } label: {
                        Label("Choose from Contacts", systemImage: "person.crop.circle.badge.plus")
                    }
                }

                Section("Message") {
                    TextField("Message", text: $text, axis: .vertical)
                        .lineLimit(3...12)
                }

                Section {
                    DatePicker("Remind me at", selection: $sendAt)
                } header: {
                    Text("When")
                } footer: {
                    if sendAt <= Date() {
                        Text("That time has passed, so this text will be ready to send right away.")
                    } else {
                        Text("You'll get a notification asking whether to send it.")
                    }
                }

                if let existing {
                    Section {
                        Button("Delete Text", role: .destructive) {
                            store.delete(id: existing.id)
                            router.close()
                        }
                    }
                }
            }
            .onChange(of: recipientAddress) { _, newValue in
                if newValue != pickedAddress {
                    recipientName = nil
                    pickedAddress = nil
                }
            }
            .navigationTitle(existing == nil ? "New Text" : "Edit Text")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .cancellationAction) {
                    Button("Cancel") { router.close() }
                }
                ToolbarItem(placement: .confirmationAction) {
                    Button("Save") {
                        store.save(draft)
                        router.close()
                    }
                    .disabled(!draft.isValid)
                }
            }
        }
        .interactiveDismissDisabled(hasChanges)
    }

    private var hasChanges: Bool {
        guard let existing else { return !text.isEmpty || !recipientAddress.isEmpty }
        return draft.normalized() != existing
    }
}
