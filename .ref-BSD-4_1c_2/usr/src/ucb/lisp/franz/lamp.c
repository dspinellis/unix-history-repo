#ifndef lint
static char *rcsid =
   "$Header: /na/franz/franz/RCS/lamp.c,v 1.1 83/01/29 13:11:25 jkf Exp $";
#endif

/*					-[Sat Jan 29 13:08:54 1983 by jkf]-
 * 	lamp.c				$Locker:  $
 * interface with unix profiling
 *
 * (c) copyright 1982, Regents of the University of California
 */

#include "global.h"

#define PBUFSZ 500000
short pbuf[PBUFSZ];

/* data space for fasl to put counters */
int mcounts[NMCOUNT];
int mcountp = (int) mcounts;
int doprof = TRUE;

lispval
Lmonitor()
{
	extern etext, countbase;

	if (np==lbot) { monitor((int(*)())0); countbase = 0; }
	else if (TYPE(lbot->val)==INT) 
	 { monitor((int (*)())2, (int (*)())lbot->val->i, pbuf,
	 				PBUFSZ*(sizeof(short)), 7000); 
	   countbase = ((int)pbuf) +12; 
	}
	else {
	   monitor((int (*)())2, (int (*)())sbrk(0), pbuf,
	   				PBUFSZ*(sizeof(short)), 7000); 
	   countbase = ((int)pbuf) + 12; }
	return(tatom);
}
