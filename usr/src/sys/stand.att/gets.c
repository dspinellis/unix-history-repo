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
 *	@(#)gets.c	7.2 (Berkeley) %G%
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
			if (lp > buf) {
				lp--;
				putchar('\b');
				putchar(' ');
				putchar('\b');
			}
			break;
		case '#':
		case '\177':
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
