#include <stdioprivate.h>

extern "C" void setlinebuf(FILE* stream)
{
    if (!__validfp(stream))
	return;
    ((streambuf*)stream)->linebuffered(1);
    // FIXME: If currently unbuffered(), set base() to NULL,
    // to force allocation later on next under/overflow.
}

