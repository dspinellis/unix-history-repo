#include <stdioprivate.h>
#include <errno.h>
// ANSI explicily requires setting errno to a positive value on failure.

extern "C" long int ftell(FILE* fp)
{
    if (!__validfp(fp)) {
#ifdef EBADF
	errno = EBADF;
#endif
	return EOF;
    }
    streampos pos = ((streambuf*)fp)->seekoff(0, ios::cur);
    if (pos == (streampos)(EOF)) {
#ifdef EIO
	if (errno == 0)
	    errno = EIO;
#endif
	return -1L;
    }
    return pos;
}
