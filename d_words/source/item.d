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
	immutable content = itemPath.readText;
	JSONValue j = content[0 .. indexOf(content, '\n')].parseJSON;

	Item i;
	i.path = itemPath;
	i.title = j["title"].str;
	i.stub = itemPath.stripExtension.baseName;
	i.date = DateTime.fromISOExtString(j["date"].str);
	i.content = content[indexOf(content, '\n') .. $].filterMarkdown;

	return i;
}