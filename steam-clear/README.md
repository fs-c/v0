# steam-clear

Clear recent activity on a given steam account. [How it will look like.](https://i.imgur.com/YyDJzvA.png)

Accounts are read from `~/.steam.json`, you may customise this by providing a path with the `-a` or `--accounts` options. Expected format is as follows:
```
{
  "<alias>": {
    "accountName": "<name>",
    "password": "<password>"
  }, { ... } ...
}
```
You may also provide an array of account objects, where the alias will be set to the accountName.

Enable logging with the `-v`/`--verbose` option.

By default, this will attempt to clear the activity every 15 minutes, with additional delays if the account is currently in-game or an error occurs. 
