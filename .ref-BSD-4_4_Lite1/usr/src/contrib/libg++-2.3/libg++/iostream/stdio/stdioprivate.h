#include <stdio.h>
#include <ioprivate.h>

#define __validfp(fp) \
  (((fp->_flags & _IO_MAGIC_MASK) == _OLD_STDIO_MAGIC && \
    (fp = *(FILE**)&((int*)fp)[1])), \
   __checkfp(fp))
#define __checkfp(fp) (fp->_flags & _IO_MAGIC_MASK) == _IO_MAGIC

inline streambuf* FILE_to_streambuf(FILE* fp)
{
    if ((fp->_flags & _IO_MAGIC_MASK) == _OLD_STDIO_MAGIC)
	return *(streambuf**)&((int*)fp)[1];
    if ((fp->_flags & _IO_MAGIC_MASK) == _IO_MAGIC)
	return (streambuf*)fp;
    return NULL;
}
