/*
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)prf.c	7.4 (Berkeley) %G%
 */

scankbd()
{
	register int c;

	c = cngetc();
	if (c == ('c'&037)) {
		printf("^C");
		_stop("");
		/* NOTREACHED */
	}
	return(c);
}

getchar()
{
	register int c;

	while((c = cngetc()) == 0)
		;
	if (c == '\r')
		c = '\n';
	else if (c == ('c'&037)) {
		printf("^C");
		_stop("");
		/* NOTREACHED */
	}
	putchar(c);
	return(c);
}

putchar(c)
	register int c;
{
	cnputc(c);
	if (c == '\n')
		cnputc('\r');
}
