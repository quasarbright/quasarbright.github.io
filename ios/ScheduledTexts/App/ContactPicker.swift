import ContactsUI
import UIKit

/// Lets the person pick a phone number or email from Contacts.
///
/// The system contact picker runs outside the app, so it needs no Contacts
/// permission and the app never sees the rest of the address book.
@MainActor
final class ContactPicker: NSObject, CNContactPickerDelegate {
    static let shared = ContactPicker()

    struct Pick {
        var name: String?
        var address: String
    }

    private var continuation: CheckedContinuation<Pick?, Never>?

    func pick() async -> Pick? {
        guard continuation == nil else { return nil }

        let picker = CNContactPickerViewController()
        picker.delegate = self
        picker.displayedPropertyKeys = [CNContactPhoneNumbersKey, CNContactEmailAddressesKey]
        picker.predicateForEnablingContact = NSPredicate(
            format: "phoneNumbers.@count > 0 OR emailAddresses.@count > 0"
        )
        // Tapping a contact shows their numbers so the person picks one,
        // rather than us guessing which number to use.
        picker.predicateForSelectionOfContact = NSPredicate(value: false)

        return await withCheckedContinuation { continuation in
            self.continuation = continuation
            Task {
                if await !UIApplication.shared.presentWhenReady(picker) {
                    self.finish(nil)
                }
            }
        }
    }

    nonisolated func contactPicker(_ picker: CNContactPickerViewController, didSelect contactProperty: CNContactProperty) {
        let contact = contactProperty.contact
        let address: String?
        if let phone = contactProperty.value as? CNPhoneNumber {
            address = phone.stringValue
        } else {
            address = contactProperty.value as? String
        }
        let formatted = CNContactFormatter.string(from: contact, style: .fullName)
        let fallback = [contact.givenName, contact.familyName]
            .filter { !$0.isEmpty }
            .joined(separator: " ")
        let name = formatted ?? (fallback.isEmpty ? nil : fallback)

        MainActor.assumeIsolated {
            self.finish(address.map { Pick(name: name, address: $0) })
        }
    }

    nonisolated func contactPickerDidCancel(_ picker: CNContactPickerViewController) {
        MainActor.assumeIsolated {
            self.finish(nil)
        }
    }

    private func finish(_ pick: Pick?) {
        continuation?.resume(returning: pick)
        continuation = nil
    }
}
