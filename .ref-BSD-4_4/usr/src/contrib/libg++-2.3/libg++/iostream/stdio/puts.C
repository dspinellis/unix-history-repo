#include "stdioprivate.h"

extern "C" int puts(const char *str)
{
    _G_size_t len = strlen(str);
    if (((streambuf*)stdout)->sputn(str, len) != len)
	return EOF;
    if (putchar('\n') == EOF)
	return EOF;
    return len+1;
}
