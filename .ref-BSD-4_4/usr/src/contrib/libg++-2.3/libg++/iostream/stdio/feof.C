#include "stdioprivate.h"

extern "C" int feof(FILE* fp)
{
    if (!__validfp(fp))
	return EOF;
    return (fp->_flags & _S_EOF_SEEN) != 0;
}
