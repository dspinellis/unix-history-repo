/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)gets.c	7.4 (Berkeley) %G%
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
