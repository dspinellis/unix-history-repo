/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)moddi3.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include "longlong.h"

long long
__moddi3 (u, v)
     long long u, v;
{
  if (u < 0)
    if (v < 0)
      return - ((unsigned long long) -u % (unsigned long long) -v);
    else
      return - ((unsigned long long) -u % (unsigned long long) v);
  else
    if (v < 0)
      return (unsigned long long) u % (unsigned long long) -v;
    else
      return (unsigned long long) u % (unsigned long long) v;
}
