#include <stdioprivate.h>

int vfprintf(FILE *fp, const char* format, _G_va_list args)
{
    streambuf* sb = FILE_to_streambuf(fp);
    if (!sb)
	return EOF;
    return sb->vform(format, args);
}
