#include <stdarg.h>
#include <stdioprivate.h>

int vprintf(const char* format, _G_va_list args)
{
    streambuf* sb = FILE_to_streambuf(stdout);
    if (!sb)
	return EOF;
    return sb->vform(format, args);
}
