#include "stdioprivate.h"

extern "C" int fileno(FILE* fp)
{
    if (!__validfp(fp))
	return EOF;
    if (!(fp->_flags & _S_IS_FILEBUF))
	return EOF;
    return ((filebuf*)fp)->fd();
}
