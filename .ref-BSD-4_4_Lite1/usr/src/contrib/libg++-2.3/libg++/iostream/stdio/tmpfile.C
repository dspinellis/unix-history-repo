#include <stdioprivate.h>
#include <errno.h>
#include <string.h>
#ifndef errno
extern int errno;
#endif

// Maybe define a new C++ streambuf sub-class:  new tmpstreambuf() ???

FILE *tmpfile(void)
{
  FILE *fp;
  int e;
  char *f;
  char  buf[L_tmpnam];

  if ((f = tmpnam(buf)) == NULL)
   return (NULL);
  fp = fopen(f, "wb+");
  e = errno;
  (void) remove(f);
  errno = e;
  return (fp);
}
