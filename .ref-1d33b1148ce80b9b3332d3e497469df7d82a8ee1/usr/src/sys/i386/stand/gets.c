/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)gets.c	7.1 (Berkeley) %G%
 */

/* special characters */
#define bs	8
#define lf	10	
#define cr	13	
#define cntlc	3	
#define del	0177	
#define cntld	4

gets(buf)
	char *buf;
{
	register char *lp;
	register c;

	lp = buf;
	for (;;) {
		c = getchar() & 0177;

		switch(c) {
		case '\n': case '\r': *lp++ = '\0';
			return;
		case '#':
			if (lp > buf && lp[-1] == '\\') {
				lp--;
				goto norm;
			}
			/* FALL THROUGH */
		case bs:
		case del:
			lp--;
			if (lp < buf)
				lp = buf;
			break;
		case '@':
			if (lp > buf && lp[-1] == '\\') {
				lp--;
				goto norm;
			}
			/* FALL THROUGH */
		case 'u'&037:
		case 'x'&037:
			lp = buf;
			putchar('\n');
			break;
		default:
		norm:
			*lp++ = c;
		}
	}
}

getchar()
{
	register char thechar;

	while (1) {
		thechar = kbd();
		switch (thechar) {
		    default: if (thechar >= ' ')
			     	sput(thechar);
			     return(thechar);
		    case cr:
		    case lf: sput(cr);
			     sput(lf);
			     return(lf);
		    case bs:
		    case del:
			     sput(bs);
			     sput(' ');
			     sput(bs);
			     return(thechar);
		    case cntlc:
			     sput('^') ; sput('C') ; sput('\r') ; sput('\n') ;
			     exit(-2) ;
		    case cntld:
			     sput('^') ; sput('D') ; sput('\r') ; sput('\n') ;
			     return(0);
		}
	}
}
