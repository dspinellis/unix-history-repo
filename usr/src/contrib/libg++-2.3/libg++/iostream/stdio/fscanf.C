#include <stdioprivate.h>

int fscanf(FILE *fp, const char* format, ...)
{
    streambuf* sb = FILE_to_streambuf(fp);
    if (!sb)
	return EOF;
    va_list args;
    va_start(args, format);
    int ret = sb->vscan(format, args);
    va_end(args);
    return ret;
}
