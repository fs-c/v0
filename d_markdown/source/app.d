import std.file;
import std.stdio;

import markdown;

void main()
{
	string md = readText("../../words/README.md");

	stdout.rawWrite(filterMarkdown(md, MarkdownFlags.backtickCodeBlocks));
}
