#include <stdioprivate.h>

extern "C" void setbuffer(FILE* stream, char* buf, int size)
{
    if (!__validfp(stream))
	return;
    if (buf) 
	(void)((streambuf*)stream)->setbuf(buf, size);
    else
	(void)((streambuf*)stream)->setbuf(NULL, 0);
}
