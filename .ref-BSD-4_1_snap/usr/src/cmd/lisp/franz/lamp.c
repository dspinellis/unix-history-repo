static char *sccsid = "@(#)lamp.c	35.1 5/6/81";

#include "global.h"

#define PBUFSZ 100000
short pbuf[PBUFSZ];

/* data space for fasl to put counters */
int mcounts[NMCOUNT];
int mcountp = (int) mcounts;
int doprof = TRUE;

lispval
Lmonitor()
{
	extern etext, countbase;

	if (np==lbot) { monitor(0); countbase = 0; }
	else if (TYPE(lbot->val)==INT) 
	 { monitor(2, lbot->val->i, pbuf, 200000, 7000); 
	   countbase = ((int)pbuf) +12; 
	}
	else {
	   monitor(2, sbrk(0), pbuf, 200000, 7000); 
	   countbase = ((int)pbuf) + 12; }
	return(tatom);
}
