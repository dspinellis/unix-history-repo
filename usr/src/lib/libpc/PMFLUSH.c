/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)PMFLUSH.c	1.4 (Berkeley) 4/9/90";
#endif /* not lint */

#include "h00vars.h"

PMFLUSH(cntrs, rtns, bufaddr)

	long cntrs;	/* total number of counters (stmt + routine) */
	long rtns;	/* number of func and proc counters */
	long *bufaddr;	/* address of count buffers */
{
	register FILE	*filep;

	bufaddr[0] = 0426;
	time(&bufaddr[1]);
	bufaddr[2] = cntrs;
	bufaddr[3] = rtns;
	filep = fopen(PXPFILE, "w");
	if (filep == NULL)
		goto ioerr;
	fwrite(bufaddr, (int)(cntrs + 1), sizeof(long), filep);
	if (ferror(filep))
		goto ioerr;
	fclose(filep);
	if (!ferror(filep))
		return;
ioerr:
	perror(PXPFILE);
}
