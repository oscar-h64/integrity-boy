# Integrity Boy

Integrity Boy is a bot which mutes Discord the specified channels when an exam is happening (eg a module specific channel). It is designed with the University of Warwick exam process in mind, and supports 24-hour window and fixed time exams (by locking for (length of exam+50%) + 45 minutes, to account for extra time and the upload window). Currently the locks have to be added to the database manually, but the ability to import from the exam timetables automatically will be added if this is still useful for 2021/22 exams.

The bot uses [discord-haskell](https://hackage.haskell.org/package/discord-haskell) and [persistent](https://hackage.haskell.org/package/persistent). It uses a Postgres database, so if the bot stops it will perform any missed tasks when it restarts, and will remember tasks across reboots.

## Building

The recommended way to build the bot is using the [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) tool, simply using the `stack build` command to build the bot.

## Usage

The bot must be configured with your Discord bot token, the database connection settings and the mapping of exam codes to channels (this can be whatever you want, it just has the match what you use in the database. Paper code is a safe option as that is likely what the bot is used if the ability to sync from Warwick systems is added). The best option for this is to copy the example config file and adjust it to your needs.

To run the bot just use `stack run -- path/to/config/file` (if you just use `stack run` then `conf/config.yaml` will be used). If you have built an executable instead of using `stack` then use `./bot-exe path/to/config/file` (the same default is used if the path is omitted).

The bot needs "SEND_MESSAGES" and "MANAGE_ROLES" permissions.

## Apology/Contributions

The code was written in relatively short time, so the formatting isn't perfect and it may not always be idiomatic. Issues and PRs about this or anything else are very welcome.
