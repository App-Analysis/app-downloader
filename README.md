# App Downloader

This is the tooling we use to acquire current (free) app charts, the apps, and the corresponding privacy labels
from the Google Play Store. For the moment we do not support iOS but plan to at least implement iOS Privacy Label support
again.

## Requirements

- sbt
- scala 3
- JRE 11+

## Installation

Simply `cd` into the root folder (the same as of this README) and run `sbt stage`.

## Configuration

Rename the `config.example.json` to `config.json` and adapt the `downloadFolderRoot` entry to determine the folder
in which the to be downloaded content is supposed to reside. If you only want to download a selection of categories adapt the `android.categories` 
list accordingly.

### Charts & Labels

Downloading charts and Privacy Labels should work out of the box.

### Apps

### Telegram Support

If you want to have telegram support, i.e., stay informed on the progress of ongoing actions you need to configure
the corresponding part of the `config.json`.
The `apiKey` is the bot key which you are told on creation by the [BotFather](https://core.telegram.org/bots/tutorial#obtain-your-bot-token).
Acquiring the `chatId` is a bit more tricky. The easiest way is to send a message in a chat in which the bot resides and then
use `https://api.telegram.org/bot<apiKey>/getUpdates` to get the latest messages. The entry for the messages
also contains the chat id in which the message was sent.