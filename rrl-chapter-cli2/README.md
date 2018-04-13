```
$ npm i -g rrl-chapter-cli
$ rrl-chapter < chapter1.md
Posted chapter.
```

__Simple CLI tool for the [royalroadl.com](https://royalroadl.com) website__, to publish chapters formatted in markdown.

This expects a config file to be located either in `./.rrl.json` or `~/.rrl.json`, with the following format: 

```
{
  "username": "",
  "password": ""
}
```

Note that headers are not supported, and that you may only use `*/_` and `**/__`. This also expects some meta information to be passed along in every chapter file - this is stripped from the markdown parsing.

```
<$META
{
  "fictionID": 0000,
  "title": "Chapter Title",
  ["preNote"]: "The note that appears above your content.",
  ["postNote"]: "The note that appears below your content."
}
$>

Your chapter content, you may use _markdown_ here.
```

The `fictionID` can for example be found in the fiction URL `https://royalroadl.com/fiction/<ID>/...`. Note that you may also use markdown in `preNote` and `postNote`.