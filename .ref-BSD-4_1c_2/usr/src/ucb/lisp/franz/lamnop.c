#ifndef lint
static char *rcsid =
   "$Header: /na/franz/franz/RCS/lamnop.c,v 1.1 83/01/29 13:11:07 jkf Exp $";
#endif

/*					-[Sat Jan 29 13:08:17 1983 by jkf]-
 * 	lamnop.c			$Locker:  $
 * file used when profiling is not being done
 *
 * (c) copyright 1982, Regents of the University of California
 */

#include "global.h"

short	pbuf[8];

/* data space for fasl to put counters */
int mcounts[1];
int mcountp = (int) mcounts;
int doprof = FALSE;

Lmonitor()
{
	error("Profiling not enabled",FALSE);
}
