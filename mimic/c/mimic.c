#include "read.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "easylzma/decompress.h"

int basic_decompress(elzma_file_format format, BYTE *in_data,
	size_t in_len, BYTE **out_data, size_t *out_len);
int input_callback(void *ctx, void *buf, size_t * size);
size_t output_callback(void *ctx, const void *buf, size_t size);

int main(int argc, char *argv[])
{
	setbuf(stdout, NULL);

	if (argc < 2) {
		printf("usage: %s <path to .osr file> [<path to out file>]\n", argv[0]);

		return 1;
	}

	FILE *stream = NULL;

	if (!(stream = fopen(argv[1], "rb"))) {
		printf("couldn't open file %s for reading\n", argv[1]);

		return 1;
	} else printf("opened file %s for reading\n", argv[1]);

	set_stream(stream);

	int test_endian = 1;
	if (*(char *)&test_endian == 1) {
		printf("on a little-endian system\n");
	} else printf("on a big-endian system\n");

	/* ruleset */
	BYTE mode = read_byte();
	printf("game mode: %d\n", (int)mode);

	/* game version */
	int32_t version = read_int32();
	printf("game version: %d\n", version);

	char string_buffer[2048];

	/* md5 hash of beatmap */
	read_osu_string(string_buffer);
	printf("beatmap hash: '%s'\n", string_buffer);

	/* player name */
	read_osu_string(string_buffer);
	printf("player name: '%s'\n", string_buffer);
	
	/* md5 hash of the replay */
	read_osu_string(string_buffer);
	printf("replay hash: '%s'\n", string_buffer);

	/* Discard the next 6 shorts */
	sread(string_buffer, sizeof(int16_t), 6);

	int32_t score = read_int32();
	printf("score: %d\n", score);

	int16_t max_combo = read_int16();
	printf("max_combo: %d\n", max_combo);

	BYTE perfect = read_byte();
	printf("perfect: %d\n", perfect);

	int32_t mods_used = read_int32();
	printf("mods used: %d\n", mods_used);

	read_osu_string(string_buffer);
	printf("life bar: '%s'\n", string_buffer);

	int64_t timestamp = read_int64();
	printf("timestamp: %ld\n", timestamp);

	/* length in bytes of the LZMA compressed replay data */
	int32_t data_len = read_int32();
	printf("data_len: %d\n", data_len);

	BYTE *data = malloc(data_len);

	if (!data) {
		printf("failed allocating %d bytes for compressed data\n",
			data_len);
		
		return 1;
	}

	for (int32_t i = 0; i < data_len; i++) {
		int c = fgetc(stream);

		if (c == EOF) {
			printf("reached EOF early, bailing out (%d)\n", i);

			break;
		}

		data[i] = c;
	}

	BYTE *decomp = NULL;
	size_t decomp_len = 0;

	int rc = basic_decompress(ELZMA_lzma, data, data_len, &decomp,
		&decomp_len);

	if (rc != ELZMA_E_OK) {
		printf("decompression failed\n");

		return 1;
	}

	if (argc < 3) {
		printf("no outfile given, stopping\n");

		return 0;
	}

	FILE *out_stream = NULL;

	if(!(out_stream = fopen(argv[2], "wb"))) {
		printf("couldn't open file %s for writing\n", argv[2]);

		return 1;
	} else printf("opened file %s for writing\n", argv[2]);

	for (size_t i = 0; i < decomp_len; i++) {
		putc(decomp[i], out_stream);
	}

	return 0;
}

struct data_stream 
{
    BYTE *in_data;
    size_t in_len;

    BYTE *out_data;
    size_t out_len;
};

int basic_decompress(elzma_file_format format, BYTE *in_data,
	size_t in_len, BYTE **out_data, size_t *out_len)
{
    int rc;
    elzma_decompress_handle hand;
    
    hand = elzma_decompress_alloc();
    
    /* now run the compression */
    {
        struct data_stream ds;
        ds.in_data = in_data;
        ds.in_len = in_len;
        ds.out_data = NULL;
        ds.out_len = 0;

        rc = elzma_decompress_run(hand, input_callback, (void *)&ds,
		output_callback, (void *)&ds, format);
        
        if (rc != ELZMA_E_OK) {
            if (ds.out_data != NULL)
	    	free(ds.out_data);
            elzma_decompress_free(&hand);
            return rc;
        }
        
        *out_data = ds.out_data;
        *out_len = ds.out_len;
    }

    return rc;
}

int input_callback(void *ctx, void *buf, size_t * size)
{
    size_t rd = 0;
    struct data_stream *ds = (struct data_stream *)ctx;

    rd = (ds->in_len < *size) ? ds->in_len : *size;

    if (rd > 0) {
        memcpy(buf, (void *) ds->in_data, rd);
        ds->in_data += rd;
        ds->in_len -= rd;
    }

    *size = rd;

    return 0;
}

size_t output_callback(void *ctx, const void *buf, size_t size)
{
    struct data_stream *ds = (struct data_stream *)ctx;

    if (size > 0) {
        ds->out_data = realloc(ds->out_data, ds->out_len + size);
        memcpy((void *) (ds->out_data + ds->out_len), buf, size);
        ds->out_len += size;
    }

    return size;
}
