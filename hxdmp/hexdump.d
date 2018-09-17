import std.file;
import std.math;
import std.stdio;

int main()
{
	int i;
	immutable cols = 8;
	immutable col_size = 2;
	immutable chunk_size = cols * col_size;

	foreach (ubyte[] buffer; stdin.byChunk(chunk_size)) {
		writef("%08x: ", i++ * chunk_size);

		handle_chunk(buffer, chunk_size, col_size);
	}

	return 0;
}

void handle_chunk(const ubyte[] chunk, const int chunk_size, const int col_size)
{
	int col_i;
	string text;

	for (int i = 0; i < chunk_size; i++) {
		// The last chunk will most likely be smaller than the max size
		if (i < chunk.length) {
			ubyte b = chunk[i];
			char c = cast(char)b;

			// TODO: Implement a more general approach to parsing
			// 	 out control sequences
			if (c == '\n' || c == '\t' || c == ' ')
				c = '.';

			text ~= c;

			writef("%02x", b);
		// If it is, pad out the remaining space
		} else {
			write("  ");
		}

		// Prefer this over modulo for performance
		if (++col_i >= col_size) {
			col_i = 0;

			write(" ");
		}
	}

	writef(" %s\n", text);
}