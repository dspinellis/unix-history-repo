/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)notdi2.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include "longlong.h"

long long
__one_cmpldi2 (u)
     long long u;
{
  long_long w;
  long_long uu;

  uu.ll = u;

  w.s.high = ~uu.s.high;
  w.s.low = ~uu.s.low;

  return w.ll;
}
