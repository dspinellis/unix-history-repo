#include "stdioprivate.h"

extern "C" size_t fread(void *buf, size_t size, size_t count, FILE* fp)
{
    streambuf* sb = FILE_to_streambuf(fp);
    if (!sb)
	return 0;
    size_t bytes_requested = size*count;
    size_t bytes_read = sb->sgetn((char *)buf, bytes_requested);
    return bytes_requested == bytes_read ? count : bytes_read / size;
}
