#include <stdioprivate.h>
extern "C" int vscanf(const char *format, _G_va_list args)
{
    streambuf* sb = FILE_to_streambuf(stdin);
    if (!sb)
	return EOF;
    return sb->vscan(format, args);
}
