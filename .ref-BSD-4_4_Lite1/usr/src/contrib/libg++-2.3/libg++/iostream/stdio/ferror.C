#include "stdioprivate.h"

extern "C" int ferror(FILE* fp)
{
    if (!__validfp(fp))
	return EOF;
    return (fp->_flags & _S_ERR_SEEN) != 0;
}
