/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)getline.c	7.1 (Berkeley) %G%
 */

/*
 * getline -- simple getline function
 * 	by A.Fujita, Dec-11-1992
 */

int
getline(prompt, buff)
	char *prompt, *buff;
{
	register int c;
	register char *p = buff;

	printf("%s", prompt);

	for(;;) {
		c = cngetc() & 0x7F;

		switch (c) {
		case 0x0a:
		case 0x0d:
			cnputc('\r');
			cnputc('\n');
			*p = '\0';
			goto outloop;

		case 0x08:
		case 0x7f:
			if (p > buff) {
				cnputc(0x08);
				cnputc(' ');
				cnputc(0x08);
				p--;
			}
			break;

		default:
			*p++ = c;
			cnputc(c);
			break;
		}
	}

 outloop:
	return(strlen(buff));
}
