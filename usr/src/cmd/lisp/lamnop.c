static char *sccsid = "@(#)lamnop.c	34.1 10/3/80";

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
