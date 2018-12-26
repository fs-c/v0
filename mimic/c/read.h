#include <stdio.h>
#include <inttypes.h>

#define sread(dest, size, count)					\
	rd = fread(dest, size, count, stream);				\
	if (rd != count)						\
		printf("fread failed (%d, %d)\n", (int)rd, (int)count);	\

#define type_read(type)			\
	type val;			\
	sread(&val, sizeof(val), 1);	\
	return val;			\

typedef unsigned char BYTE;

/* Used by sread */
size_t rd = 0;
FILE *stream = NULL;

void set_stream(FILE *new_stream)
{
	stream = new_stream;
}

BYTE read_byte()
{
	// BYTE by;
	// sread(&by, sizeof(by), 1);
	// return by;

	type_read(BYTE);
}

int16_t read_int16()
{
	int16_t val;
	sread(&val, sizeof(val), 1);
	return val;
}

int32_t read_int32()
{
	int32_t val;
	sread(&val, sizeof(val), 1);
	return val;
}

int64_t read_int64()
{
	int64_t val;
	sread(&val, sizeof(val), 1);
	return val;
}

/* Taken from Section 7.6 of the DWARF 3 spec, Appendix C */
uint64_t read_uleb128()
{
	BYTE by = 0;
	int shift = 0;
	uint64_t result = 0;

	while (1) {
		sread(&by, sizeof(by), 1);

		result |= ((by & 0x7f) << shift);

		if (!(by & 0x80))
			break;

		shift += 7;
	}

	return result;
}

/* These are really just C# strings */
size_t read_osu_string(char *out_buffer)
{
	BYTE indicator;
	sread(&indicator, sizeof(BYTE), 1);

	if (indicator != 0x0b) {
		printf("string indicator set to invalid value\n");

		return 0;
	}

	uint64_t len = read_uleb128();

	printf("  len: %d\n", (int)len);

	for (size_t i = 0; i < len; i++) {
		sread(out_buffer++, sizeof(char), 1);
	}

	*out_buffer = '\0';

	return len;
}

/* Just keeping these around in case they are ever needed again */
#ifdef NULL

void u16_switch_endian(uint16_t *x)
{
	*x = (*x >> 8) | (*x << 8);
}

void u32_switch_endian(uint32_t *x)
{
	*x = (*x >> 24) | 
		((*x << 8) & 0x00FF0000) |
		((*x >> 8) & 0x0000FF00) |
		(*x << 24);
}

#endif
