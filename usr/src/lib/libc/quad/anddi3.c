/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)anddi3.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include "longlong.h"

long long 
__anddi3 (u, v)
     long long u, v;
{
  long_long w;
  long_long uu, vv;

  uu.ll = u;
  vv.ll = v;

  w.s.high = uu.s.high & vv.s.high;
  w.s.low = uu.s.low & vv.s.low;

  return w.ll;
}
