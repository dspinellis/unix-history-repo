/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fixdfdi.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include "longlong.h"

long long
__fixdfdi (a)
     double a;
{
  long long __fixunsdfdi (double a);

  if (a < 0)
    return - __fixunsdfdi (-a);
  return __fixunsdfdi (a);
}
