#include "stdioprivate.h"
#include <errno.h>

extern "C" int fflush (register FILE *fp)
{
  if (fp == NULL)
      return streambuf::flush_all();

  streambuf* sb = FILE_to_streambuf(fp);
  if (!sb)
    {
      errno = EINVAL;
      return EOF;
    }

  return sb->sync() == EOF ? EOF : 0;
}
