/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)docopy.c	7.3 (Berkeley) 6/28/90
 */

#define	SIZE	10240

docopy(from, to, nrecs)
	register int from, to, nrecs;
{
	register int record, rcc, wcc;
	char buf[SIZE];

	for (record = 0;;) {
		if (!(rcc = read(from, buffer, SIZE)))
			break;
		if (rcc < 0) {
			printf("Record %d: read error, errno=%d\n",
			    record, errno);
			break;
		}
		if (rcc < SIZE)
			printf("Record %d: read short; expected %d, got %d\n",
			    record, SIZE, rcc);
#ifdef vax
		/* For bug in ht driver. */
		if (rcc > SIZE)
			rcc = SIZE;
#endif
		if ((wcc = write(to, buffer, rcc)) < 0) {
			printf("Record %d: write error: errno=%d\n",
			    record, errno);
			break;
		}
		if (wcc < rcc) {
			printf("Record %d: write short; expected %d, got %d\n",
			    record, rcc, wcc);
			break;
		}
		if (nrecs > 0 && ++record == nrecs)
			break;
	}
	printf("copy completed: %d records copied\n", record);
}
