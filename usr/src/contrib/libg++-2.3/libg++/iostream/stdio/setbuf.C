#include <stdioprivate.h>

extern "C" void setbuf(FILE* stream, char* buf)
{
    streambuf* sb = FILE_to_streambuf(stream);
    if (!sb)
	return;
    sb->linebuffered(0);
    if (buf) 
	(void)sb->setbuf(buf, BUFSIZ);
    else
	(void)sb->setbuf(NULL, 0);
}
