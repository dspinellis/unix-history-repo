/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)docopy.c	7.1 (Berkeley) %G%
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
