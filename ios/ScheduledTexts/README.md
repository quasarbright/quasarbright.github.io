# Scheduled Texts

An iPhone app for texts you want to send later, but only if you still want to send them when the time comes. You write the text now. At the time you pick, a notification asks "Send text to Alex?", and you decide then. ([Issue #268](https://github.com/quasarbright/quasarbright.github.io/issues/268))

## How it works

- **Tap the notification** to open a "Send this text?" screen with Send, Edit and Don't Send.
- **Press and hold the notification** for the same three choices without opening the app first:
  - **Send** opens the Messages compose sheet with the recipient and text filled in. You tap the send arrow.
  - **Edit** opens the text in the app so you can change the wording, recipient or time.
  - **Don't Send** deletes it without opening the app.
- **Missed notifications stay** in Notification Center until you deal with them, and the app icon badge counts texts that are waiting.
- **The app lists everything pending**: "Ready to Send" for texts whose time has come, and "Scheduled" for later ones. Swipe right to send now, swipe left to delete.
- **iMessage and SMS** both work. The system compose sheet picks the right one, so Android contacts are fine.

## What iOS doesn't allow

- **No sending without a tap.** Apps can only open a prefilled compose sheet. Sending still takes one tap on the send arrow.
- **No "only if we haven't talked" check.** Apps can't read your messages or call history, so the app can't tell whether you've been in touch since you scheduled the text. That's why every text asks first.
- **64 notifications at a time.** iOS only keeps an app's next 64 scheduled notifications. The app schedules the soonest 64 and tops up every time you open it, so this only matters if you have more than 64 texts queued and don't open the app for a long time.

## Run it on your iPhone

You need a Mac with Xcode 16 or newer, and [XcodeGen](https://github.com/yonaskolb/XcodeGen) to create the Xcode project.

```sh
brew install xcodegen
cd ios/ScheduledTexts
xcodegen
open ScheduledTexts.xcodeproj
```

In Xcode, select the ScheduledTexts target, open Signing & Capabilities, and pick your team (a free personal team works). Then plug in your iPhone and press Run. The simulator can run the app, but it can't send texts, so the app offers to copy the message instead.

The `.xcodeproj` is generated, so it isn't checked in. Run `xcodegen` again after adding or removing files.

## Code layout

- `ScheduledTextsCore/` is a Swift package with the logic that doesn't need iOS: the message model, the queue, which notifications to schedule, and saving to disk. Its tests run anywhere with `swift test`.
- `App/` is the iPhone app: SwiftUI screens, notification handling, the Messages compose sheet and the contact picker.
- `project.yml` describes the Xcode project for XcodeGen.

Texts are saved as JSON in the app's Application Support folder. Nothing leaves the phone.

## Before the App Store

- An app icon (the project has none yet).
- A privacy policy URL. The app collects nothing, so it can say exactly that.
- Your bundle id and team in `project.yml`.
