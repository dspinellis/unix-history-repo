static char *sccsid = "@(#)lamnop.c	35.1 5/6/81";

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
