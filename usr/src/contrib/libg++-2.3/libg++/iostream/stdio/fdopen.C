#include "stdioprivate.h"
#include "errno.h"

extern "C" FILE *fdopen(int fd, const char *mode)
{
  if (fd < 0)
    {
      errno = EBADF;
      return NULL;
    }
  filebuf* fbuf = new filebuf();
  if (fbuf->attach(fd) == NULL) {
      delete fbuf;
      return NULL;
  }
  fbuf->_flags &= ~_S_DELETE_DONT_CLOSE;
  return (FILE*)fbuf;
}
