#include <stdioprivate.h>
#include <stdarg.h>

#undef stdout
// To force linking in the non-macro definitions of stdin/stdout/stderr.

int printf(const char* format, ...)
{
    streambuf* sb = FILE_to_streambuf(stdout);
    if (!sb)
	return EOF;
    va_list args;
    va_start(args, format);
    int ret = sb->vform(format, args);
    va_end(args);
    return ret;
}
