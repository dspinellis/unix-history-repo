#include "ex.h"
/*
 * Ex3a - a text editor
 * Bill Joy UCB July 1977
 */

xop(f, c)
	int (*f)();
	char c;
{
	register int *dp;

	nonzero();
	if (!inglobal)
		save12(), undkind = UNDCHANGE;
	for (dp = addr1; dp <= addr2; dp++) {
		dot = dp;
		getline(*dp);
		strcpy(genbuf, linebuf);
		(*f)(c);
		if (!eq(linebuf, genbuf))
			putmark(dp);
	}
}

xpand()
{
	register char *cp, *dp;
	register int col;

	col = 0;
	for (dp = linebuf, cp = genbuf; *cp; cp++) {
		if (*cp == '\t') {
			do {
				if (dp >= &linebuf[LBSIZE-2])
					goto xovflo;
				*dp++ = ' ';
				col++;
			} while (col & 07);
			continue;
		}
		if (dp >= &linebuf[LBSIZE-2])
xovflo:
			error("Line too long@after expansion of tabs");
		*dp++ = *cp;
		col++;
	}
	*dp++ = 0;
}

tabulate(c)
	char c;
{
	register char *cp, *dp;
	register int dcol;
	int ocol;

	ocol = 0;
	dcol = 0;
	cp = genbuf, dp = linebuf;
	for (;;) {
		switch (*cp) {
			case ' ':
				dcol++;
				break;
			case '\t':
				dcol =+ 8;
				dcol =& ~07;
				break;
			default:
				while (((ocol + 8) &~ 07) <= dcol) {
					if (ocol + 1 == dcol)
						break;
					*dp++ = '\t';
					ocol =+ 8;
					ocol =& ~07;
				}
				while (ocol < dcol) {
					*dp++ = ' ';
					ocol++;
				}
				if (*cp == 0 || c == 0) {
					strcpy(dp, cp);
					return;
				}
				*dp++ = *cp;
				ocol++, dcol++;
		}
		cp++;
	}
}
