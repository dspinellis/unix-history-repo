/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)copy.c	1.1 (Berkeley) %G%
 */

/*
 * Copy from to in 10K units.
 * Intended for use in system
 * installation.
 */
char	buffer[32*1024];

main()
{
	int from, to, firstrecord = 1;
	char fbuf[50], tbuf[50];
	register int record, recsize = sizeof (buffer);
	extern int errno;

	from = getdev("From", fbuf, 0);
	to = getdev("To", tbuf, 1);
	for (record = 0; ; record++) {
		int rcc, wcc;

/* printf("read(%d, %x, %d)\n", from, buffer, recsize); */
		rcc = read(from, buffer, recsize);
/* printf("rcc %d\n", rcc); */
		if (rcc == 0)
			break;
		if (rcc < 0) {
			printf("Record %d: read error, errno=%d\n",
				record, errno);
			break;
		}
		if (firstrecord) {
			if (rcc != recsize)
				recsize = rcc;
			firstrecord = !firstrecord;
			printf("%d Kbyte records\n", recsize/1024);
		}
		if (rcc < recsize)
			printf("Record %d: read short; expected %d, got %d\n",
				record, sizeof (buffer), rcc);
		wcc = write(to, buffer, rcc);
		if (wcc < 0) {
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
	printf("%d records copied\n", record);
	/* can't call exit here */
}

getdev(prompt, buf, mode)
	char *prompt, *buf;
	int mode;
{
	register int i;

	do {
		printf("%s: ", prompt);
		gets(buf);
		i = open(buf, mode);
	} while (i <= 0);
	return (i);
}
