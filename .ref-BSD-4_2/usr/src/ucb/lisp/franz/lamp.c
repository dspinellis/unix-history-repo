#ifndef lint
static char *rcsid =
   "$Header: /na/franz/franz/RCS/lamp.c,v 1.2 83/03/23 22:17:52 jkf Exp $";
#endif

/*					-[Tue Mar 22 15:17:09 1983 by jkf]-
 * 	lamp.c				$Locker:  $
 * interface with unix profiling
 *
 * (c) copyright 1982, Regents of the University of California
 */

#include "global.h"

#ifdef PROF

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


#else

/* if prof is not defined, create a dummy Lmonitor */

short	pbuf[8];

/* data space for fasl to put counters */
int mcounts[1];
int mcountp = (int) mcounts;
int doprof = FALSE;

Lmonitor()
{
	error("Profiling not enabled",FALSE);
}


#endif
