#include <stdarg.h>
#include <stdioprivate.h>

int scanf(const char* format, ...)
{
    streambuf* sb = FILE_to_streambuf(stdin);
    if (!sb)
	return EOF;
    va_list args;
    va_start(args, format);
    int ret = sb->vscan(format, args);
    va_end(args);
    return ret;
}
