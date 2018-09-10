import std.file;
import std.path;
import std.json;
import std.string;
import std.datetime.date;

import dmarkdown;

struct Item {
	string path;
	string stub;
	string title;
	DateTime date;
	string content;
}

Item parseItem(const string itemPath)
{
	immutable content = readText(itemPath);
	JSONValue j = parseJSON(content[0 .. indexOf(content, '\n')]);

	Item i;
	i.path = itemPath;
	i.title = j["title"].str;
	i.stub = baseName(stripExtension(itemPath));
	i.date = DateTime.fromISOExtString(j["date"].str);
	i.content = filterMarkdown(content[indexOf(content, '\n') .. $]);

	return i;
}