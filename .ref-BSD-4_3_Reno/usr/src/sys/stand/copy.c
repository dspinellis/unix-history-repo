/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)copy.c	7.5 (Berkeley) 5/24/88
 */

#define	BSIZE	10240

/*
 * Copy from from to to.  Intended for use in system installation.
 */
main()
{
	extern int errno;
	register int from, to, record, rcc, wcc;
	char buf[BSIZE];

	from = getfile("From", 0);
	to = getfile("To", 1);
	for (record = 0;; ++record) {
		if (!(rcc = read(from, buf, BSIZE)))
			break;
		if (rcc < 0) {
			printf("Record %d: read error, errno=%d\n",
			    record, errno);
			break;
		}
		if (!record && rcc != BSIZE) {
			rcc = BSIZE;
			printf("Block size set from input; %d bytes\n", BSIZE);
		}
		if (rcc < BSIZE)
			printf("Record %d: read short; expected %d, got %d\n",
			    record, BSIZE, rcc);
#ifdef vax
		/* For bug in ht driver. */
		if (rcc > BSIZE)
			rcc = BSIZE;
#endif
		if ((wcc = write(to, buf, rcc)) < 0) {
			printf("Record %d: write error: errno=%d\n",
			    record, errno);
			break;
		}
		if (wcc < rcc) {
			printf("Record %d: write short; expected %d, got %d\n",
			    record, rcc, wcc);
			break;
		}
	}
	printf("copy completed: %d records copied\n", record);
}
