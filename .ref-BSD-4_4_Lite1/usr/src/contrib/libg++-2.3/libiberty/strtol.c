/*
 * strtol : convert a string to long.
 *
 * Andy Wilson, 2-Oct-89.
 */

#include <errno.h>
#include <ctype.h>
#include <stdio.h>
#include "ansidecl.h"

/* FIXME: It'd be nice to configure around these, but the include files are too
   painful.  These macros should at least be more portable than hardwired hex
   constants. */

#define	ULONG_MAX	((unsigned long)(~0L))		/* 0xFFFFFFFF */
#define	LONG_MAX	((long)(ULONG_MAX >> 1))	/* 0x7FFFFFFF */
#define	LONG_MIN	((long)(~LONG_MAX))		/* 0x80000000 */

extern int errno;

long
strtol(s, ptr, base)
     CONST char *s; char **ptr; int base;
{
  extern unsigned long  strtoul();
  int minus=0;
  unsigned long tmp;
  CONST char *start=s, *eptr;

  if (s==NULL)
    {
      errno = ERANGE;
      if (!ptr)
	*ptr = (char *)start;
      return 0L;
    }
  while (isspace(*s))
	s++;
  if (*s == '-') {
	s++;
	minus = 1;
      }
  else if (*s == '+')
    s++;

  /*
   * let strtoul do the hard work.
   */
  tmp = strtoul(s, &eptr, base);
  if (ptr != NULL)
    *ptr = (char *)((eptr==s) ? (char *)start : eptr);
  if (errno==ERANGE && tmp==ULONG_MAX)
    return (minus ? LONG_MIN : LONG_MAX);
  else
    return (minus ? (long) -tmp : (long) tmp);
}
