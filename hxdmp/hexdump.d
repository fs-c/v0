import std.file;
import std.stdio;

int main()
{
	int i;
	immutable cols = 8;
	immutable col_size = 2;
	immutable chunk_size = cols * col_size;

	foreach (ubyte[] buffer; stdin.byChunk(chunk_size)) {
		writef("%08x: ", i);
		i += chunk_size;

		handle_chunk(buffer, chunk_size, col_size);
	}

	return 0;
}

void handle_chunk(const ubyte[] chunk, const int chunk_size, const int col_size)
{
	string text;

	foreach (i, b; chunk) {
		char c = cast(char)b;

		if (c == '\n' || c == '\t' || c == ' ')
			c = '.';

		text ~= c;

		writef("%02x", b);

		if (!((i + 1) % col_size)) {
			write(" ");			
		}
	}

	writef(" %s\n", text);
}