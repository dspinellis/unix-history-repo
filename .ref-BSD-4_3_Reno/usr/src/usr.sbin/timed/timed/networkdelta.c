/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)networkdelta.c	2.4 (Berkeley) 6/1/90";
#endif /* not lint */

#include "globals.h"
#include <protocols/timed.h>

extern int machup;

/*
 * `networkdelta' selects the largest set of deltas that fall within the
 * interval RANGE, and uses them to compute the network average delta 
 */

long networkdelta()
{
	int i, j, maxind, minind;
	int ext;
	int tempind;
	long tempdata;
	long x[NHOSTS];
	long average;

	for (i=0; i<slvcount; i++)
		x[i] = hp[i].delta;
	for (i=0; i<slvcount-1; i++) {
		tempdata = x[i];
		tempind = i;
		for (j=i+1; j<slvcount; j++) {
			if (x[j] < tempdata) {
				tempdata = x[j];
				tempind = j;
			}
		}
		x[tempind] = x[i];
		x[i] = tempdata;
	}

	/* this piece of code is critical: DO NOT TOUCH IT! */
/****/
	i=0; j=1; minind=0; maxind=1;
	if (machup == 2)
		goto compute;
	do {
		if (x[j]-x[i] <= RANGE)
			j++;
		else {
			if (j > i+1) 
 				j--; 
			if ((x[j]-x[i] <= RANGE) && (j-i >= maxind-minind)) {
				minind=i;
				maxind=j;
			}	
			i++;
			if(i = j)
				j++;
		}
	} while (j < machup);
	if ((x[machup-1] - x[i] <= RANGE) && (machup-i-1 >= maxind-minind)) {
		minind=i; maxind=machup-1;
	}
/****/
compute:
	ext = maxind - minind + 1;
	average = 0;
	for (i=minind; i<=maxind; i++)
		average += x[i];
	average /= ext;
	return(average);
}
