/*
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)prf.c	7.5 (Berkeley) %G%
 */

/*
 * XXX we know that scankbd is only called from read/write to interrupt
 * a boot program.  Since we restart only on ^C and we do that here, we
 * always return 0 to avoid a longjmp in the caller.
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
	return(0);
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
	if (c != '\b' && c != '\177')
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
