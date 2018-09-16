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

		handle_chunk(buffer, col_size);

		// writefln(buffer[0]);
	}

	return 0;
}

void handle_chunk(const ubyte[] chunk, const int col_size)
{
	int col_i;
	string s;

	foreach (b; chunk) {
		char c = cast(char)b;

		if (c == '\n' || c == '\t' || c == ' ')
			c = '.';
		
		s ~= c;

		writef("%02x", b);

		if (++col_i == col_size) {
			col_i = 0;
			write(" ");			
		}
	}

	writef(" %s\n", s);
}