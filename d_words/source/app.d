import std.file;
import std.path;
import std.exception;

import item;
import mustache;

alias MustacheEngine!(string) Mustache;

void main()
{
	immutable publicPath = absolutePath("public");

	if (publicPath.exists)
		publicPath.rmdirRecurse;

	publicPath.mkdir;

	immutable staticPath = absolutePath("static");
	copyDir(staticPath, buildPath(publicPath, "static"));

	immutable templatesPath = absolutePath("templates");
	immutable itemTemplate = buildPath(templatesPath, "item");
	immutable frontTemplate = buildPath(templatesPath, "front");

	Mustache mustache;
	auto context = new Mustache.Context;

	immutable contentPath = absolutePath("content");
	foreach(string e; dirEntries(contentPath, SpanMode.shallow)) {
		Item i = parseItem(e);

		immutable itemFolder = buildPath(publicPath, i.stub);
		immutable itemPath = buildPath(itemFolder, "index.html");
		mkdir(itemFolder);

		context["title"] = i.title;
		context["content"] = i.content;

		// stdout.rawWrite(mustache.render(itemTemplate, context));

		write(itemPath, mustache.render(itemTemplate, context));
	}
}

void copyDir(const string from, const string to)
{
	if (to.exists && to.isDir)
		to.rmdirRecurse;

	to.mkdir();

	foreach(DirEntry e; dirEntries(from, SpanMode.breadth)) {
		immutable newPath = absolutePath(e.name[from.length + 1 .. $],
			to);

		if (e.isDir())
			mkdir(newPath);
		else
			copy(e.name, newPath);
	}
}
