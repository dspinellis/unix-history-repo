/*-
 * Copyright (c) 1982, 1986, 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)copy.c	7.7 (Berkeley) %G%
 */

#define	BSIZE	10240

/*
 * Copy from from to to.  Intended for use in system installation.
 */
main()
{
	extern int errno;
	register int from, to, record, rcc, wcc, bsize = BSIZE;
	char buf[BSIZE];

	from = getfile("From", 0);
	to = getfile("To", 1);
	for (record = 0;; ++record) {
		if (!(rcc = read(from, buf, bsize)))
			break;
		if (rcc < 0) {
			printf("Record %d: read error, errno=%d\n",
			    record, errno);
			break;
		}
		if (rcc != bsize) {
			if (record == 0) {
				bsize = rcc;
				printf("Block size set from input; %d bytes\n",
				    bsize);
			} else
				printf("Record %d: read short; expected %d, got %d\n",
				    record, bsize, rcc);
		}
#ifdef vax
		/* For bug in ht driver. */
		if (rcc > bsize)
			rcc = bsize;
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
