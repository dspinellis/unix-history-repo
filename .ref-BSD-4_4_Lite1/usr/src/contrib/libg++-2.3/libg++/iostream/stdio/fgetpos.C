#include <stdioprivate.h>
#include <errno.h>
// ANSI explicily requires setting errno to a positive value on failure.

int fgetpos(FILE* fp, fpos_t *pos)
{
    if (!__validfp(fp)) {
#ifdef EBADF
	errno = EBADF;
#endif
	return EOF;
    }
    streampos spos = ((streambuf*)fp)->seekoff(0, ios::cur);
    if (spos == (streampos)(EOF)) {
#ifdef EIO
	if (errno == 0)
	    errno = EIO;
#endif
	return EOF;
    }
    *pos = spos;
    return 0;
}
