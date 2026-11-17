import Foundation

/// Where the app should go next: a sheet, or the system compose screen.
enum Route: Identifiable, Equatable {
    case new
    /// "Send this text?" with Send, Edit and Don't Send.
    case review(UUID)
    case edit(UUID)
    /// The prefilled system compose sheet.
    case send(UUID)

    var id: String {
        switch self {
        case .new: return "new"
        case .review(let id): return "review-\(id)"
        case .edit(let id): return "edit-\(id)"
        case .send(let id): return "send-\(id)"
        }
    }
}

/// Notifications can arrive before any view exists (a tap that launches the
/// app), so they go through this shared object and the views pick it up.
@MainActor
final class Router: ObservableObject {
    static let shared = Router()

    /// The sheet over the main list.
    @Published var sheet: Route?
    /// A message waiting to be opened in the compose sheet. The compose
    /// sheet is UIKit, so the main view presents it instead of `sheet`.
    @Published var pendingSend: UUID?

    func open(_ route: Route) {
        switch route {
        case .send(let id):
            sheet = nil
            pendingSend = id
        default:
            sheet = route
        }
    }

    func close() {
        sheet = nil
    }
}
