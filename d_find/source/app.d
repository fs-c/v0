import std.stdio;
import std.algorithm.searching;

void main()
{
	string parsed;
	string buffer = "lorem {{ipsum}} dolor {{sit}} amet, whatever {{keyword}} whatever";

	string key;
	auto res = findSplit(buffer, "{{");
	while (res) {
		int i = 0;
		while (res[2][i] != '}')
			i++;

		parsed ~= res[0][key.length .. $];

		key = res[2][0 .. i];

		buffer = buffer[res[0].length + 2 .. $];

		key.length += 3;
		
		res = findSplit(buffer, "{{");
	}

	parsed ~= res[0][key.length .. $];

	writeln;
	writeln(parsed);
}
