#include "stdioprivate.h"

extern "C" int fputs(const char *str, FILE *fp)
{
    _G_size_t len = strlen(str);
    if (((streambuf*)fp)->sputn(str, len) != len)
	return EOF;
    return len;
}
