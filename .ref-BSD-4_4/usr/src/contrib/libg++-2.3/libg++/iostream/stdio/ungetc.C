#include <stdioprivate.h>

extern "C" int ungetc(int c, FILE* fp)
{
    if (!__validfp(fp))
	return EOF;
    if (c == EOF)
	return EOF;
    return ((streambuf*)fp)->sputbackc(c);
}
