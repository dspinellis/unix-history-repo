#include <stdioprivate.h>
#include <errno.h>
// ANSI explicily requires setting errno to a positive value on failure.

int fsetpos(FILE* fp, const fpos_t *pos)
{
    if (!__validfp(fp)) {
#ifdef EBADF
	errno = EBADF;
#endif
	return EOF;
    }
    if (((streambuf*)fp)->seekpos((streampos)(*pos)) == (streampos)(EOF)) {
#ifdef EIO
	if (errno == 0)
	    errno = EIO;
#endif
	return EOF;
    }
    return 0;
}
