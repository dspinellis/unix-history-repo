/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ucmpdi2.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include "longlong.h"

SItype
__ucmpdi2 (a, b)
     long long a, b;
{
  long_long au, bu;

  au.ll = a, bu.ll = b;

  if ((unsigned) au.s.high < (unsigned) bu.s.high)
    return 0;
  else if ((unsigned) au.s.high > (unsigned) bu.s.high)
    return 2;
  if ((unsigned) au.s.low < (unsigned) bu.s.low)
    return 0;
  else if ((unsigned) au.s.low > (unsigned) bu.s.low)
    return 2;
  return 1;
}
