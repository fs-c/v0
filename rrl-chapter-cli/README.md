## royalroadl CLI

A simple command line tool to publish chapters formatted in markdown.

```
npm i -g rrl-chapter-cli
rrl-chapter -h
```

Optionally reads from stdin and writes to stdout.

```
rrl-chapter publish < chapter.md
rrl-chapter parse --file chapter.md > parsed.html
```