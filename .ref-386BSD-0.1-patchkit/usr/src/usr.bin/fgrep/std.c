/* std.c - compensate for a few missing library functions.
   In the Public Domain; written by Mike Haertel. */

#include "std.h"
#include "unix.h"

#ifdef X_memmove
PTR
DEFUN(memmove, (d, s, n), PTR d AND PTRCONST s AND size_t n)
{
  char *dd;
  const char *ss;

  dd = d;
  ss = s;
  if (dd > ss && dd < ss + n)
    {
      dd += n;
      ss += n;
      while (n--)
	*--dd = *--ss;
    }
  else
    while (n--)
      *dd++ = *ss++;
  return d;
}
#endif /* X_memmove */

#ifdef X_remove
#if defined(unix) || defined(__unix__)
int
DEFUN(remove, (filename), const char *filename)
{
  extern int EXFUN(unlink, (const char *));

  return unlink(filename);
}
#endif /* unix */
#endif /* X_strerror */

#ifdef X_strerror
#if defined(unix) || defined(__unix__)
char *
DEFUN(strerror, (errnum), int errnum)
{
  extern int sys_nerr;
  extern char *sys_errlist[];

  if (errnum > 0 && errnum < sys_nerr)
    return sys_errlist[errnum];
  return "";
}
#endif /* unix */
#endif /* X_strerror */
