![](https://i.imgur.com/mpQcvX8.png)

Clear recent activity on a given steam account. [How it will look like.](https://i.imgur.com/YyDJzvA.png)

You may input the accounts through a file, either `~./steam.json` or the ACCOUNTS environment variable. 
Dotenv support is provided, meaning you can simply create a `.env` file with `ACCOUNTS=/your/path` as the only content in the project root.

This will attempt to clear the activity every 15 minutes, with additional delays if the account is currently in-game or an error occurs. 
