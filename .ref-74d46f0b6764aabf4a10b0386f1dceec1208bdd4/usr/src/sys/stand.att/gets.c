/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)gets.c	7.5 (Berkeley) %G%
 */

gets(buf)
	char *buf;
{
	register int c;
	register char *lp;

	for (lp = buf;;)
		switch(c = getchar() & 0177) {
		case '\n':
		case '\r':
			*lp = '\0';
			return;
		case '\b':
		case '\177':
			if (lp > buf) {
				lp--;
				putchar('\b');
				putchar(' ');
				putchar('\b');
			}
			break;
		case '#':
			if (lp > buf)
				--lp;
			break;
		case 'r'&037: {
			register char *p;

			putchar('\n');
			for (p = buf; p < lp; ++p)
				putchar(*p);
			break;
		}
		case '@':
		case 'u'&037:
		case 'w'&037:
			lp = buf;
			putchar('\n');
			break;
		default:
			*lp++ = c;
		}
	/*NOTREACHED*/
}
