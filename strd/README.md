`strd` is a simple command line utility which allows accepting trades on the Steam platform without using the official Steam app.

```
$ npm i -g @sturmwalzer/strd
$ strd -h

 :::::::: ::::::::::: :::::::::  :::::::::
:+:    :+:    :+:     :+:    :+: :+:    :+:
+:+           +:+     +:+    +:+ +:+    +:+
+#++:++#++    +#+     +#++:++#:  +#+    +:+
       +#+    +#+     +#+    +#+ +#+    +#+
#+#    #+#    #+#     #+#    #+# #+#    #+#
 ########     ###     ###    ### #########

           steam trade assistant

Usage: strd [-hcd]
    -h Displays usage information and exits.
    -c Location of the config file, can be absolute or relative. [~/.strd.json]
    -d Enables verbose (debug) logging.

Config format:
    {
        "name": "unique steam account name",
        "password": "corresponding password",
        "shasec": "shared secret",
        "idsec": "identity secret",
    }

```
