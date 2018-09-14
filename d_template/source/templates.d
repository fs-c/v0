import std.file : readText;
import std.stdio;

import item;

string renderItemTemplate(const string templatePath, Item item)
{
	string[string] context;
	immutable templateFile = readText(templatePath);

	context["title"] = item.title;
	context["content"] = item.content;

	string buffer;

	string name;
	bool inName = false;

	for (int i = 0; i < templateFile.length; i++) {		
		immutable curChar = templateFile[i];
		immutable lastChar = templateFile[i == 0 ? i : i - 1];
		immutable nextChar = templateFile[i == templateFile.length - 1 ? i : i + 1];		

		if (curChar == '{' && nextChar == '{')
			inName = true;
		if (curChar == '}' && nextChar == '}') {
			inName = false;

			writefln("found full name: %s", name);

			buffer ~= context[name];

			name = "";
		}

		if (inName && curChar != '{') {
			name ~= curChar;
		} else if (curChar != '{' && curChar != '}') buffer ~= curChar;
	}

	writefln("buffer: %s", buffer);

	return buffer;
}