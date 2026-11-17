import ScheduledTextsCore
import SwiftUI

/// "Send this text?" Shown when the person taps a notification or a text
/// that's ready to send.
struct ReviewView: View {
    let message: ScheduledMessage

    @EnvironmentObject private var store: MessageStore
    @EnvironmentObject private var router: Router

    var body: some View {
        NavigationStack {
            VStack(alignment: .leading, spacing: 20) {
                VStack(alignment: .leading, spacing: 2) {
                    Text("To \(message.displayRecipient)")
                        .font(.headline)
                    if message.recipientName != nil {
                        Text(message.recipientAddress)
                            .font(.subheadline)
                            .foregroundStyle(.secondary)
                    }
                }

                Text(message.body)
                    .padding(.horizontal, 14)
                    .padding(.vertical, 10)
                    .foregroundStyle(.white)
                    .background(Color.blue, in: RoundedRectangle(cornerRadius: 18, style: .continuous))
                    .frame(maxWidth: .infinity, alignment: .trailing)

                Text("Scheduled for \(message.sendAt.formatted(date: .abbreviated, time: .shortened))")
                    .font(.footnote)
                    .foregroundStyle(.secondary)

                Spacer(minLength: 0)

                VStack(spacing: 12) {
                    Button {
                        router.open(.send(message.id))
                    } label: {
                        Label("Send", systemImage: "paperplane.fill")
                            .frame(maxWidth: .infinity)
                    }
                    .buttonStyle(.borderedProminent)

                    Button {
                        router.open(.edit(message.id))
                    } label: {
                        Label("Edit", systemImage: "pencil")
                            .frame(maxWidth: .infinity)
                    }
                    .buttonStyle(.bordered)

                    Button(role: .destructive) {
                        store.delete(id: message.id)
                        router.close()
                    } label: {
                        Label("Don't Send", systemImage: "xmark")
                            .frame(maxWidth: .infinity)
                    }
                    .buttonStyle(.bordered)
                }
                .controlSize(.large)
            }
            .padding()
            .navigationTitle("Send this text?")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .cancellationAction) {
                    Button("Later") { router.close() }
                }
            }
        }
        .presentationDetents([.medium, .large])
    }
}
