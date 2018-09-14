import std.path;
import std.file;

import item;
import templates;

void main()
{
	Item item;

	item.title = "example title";
	item.content = "<div class=\"whatever\">example content</div>";

	immutable rendered = renderItemTemplate(absolutePath("templates/item.html"), item);

	write(absolutePath("public/item.html"), rendered);
}
