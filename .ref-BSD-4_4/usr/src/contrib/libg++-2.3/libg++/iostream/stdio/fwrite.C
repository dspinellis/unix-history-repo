#include <stdioprivate.h>

extern "C" size_t fwrite(const void *buf, size_t size, size_t count, FILE *fp)
{
    streambuf* sb = FILE_to_streambuf(fp);
    if (!sb)
	return 0;
    size_t request = size*count;
    size_t written = sb->sputn((const char *)buf, request);
    if (written == request)
	return count;
    else
	return written / size;
}
