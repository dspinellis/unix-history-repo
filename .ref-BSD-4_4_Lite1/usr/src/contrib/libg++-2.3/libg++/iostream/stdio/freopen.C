#include "stdioprivate.h"

FILE* freopen(const char* filename, const char* mode, FILE* stream)
{
    if (!__validfp(stream) || !(stream->_flags & _S_IS_FILEBUF))
	return NULL;
    filebuf* fb = (filebuf*)stream;
    fb->close();
    return (FILE*)fb->open(filename, mode);
}
