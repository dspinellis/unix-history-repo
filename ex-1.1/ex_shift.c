#
/*
 * Ex - a text editor
 * Bill Joy UCB October 1977
 */
#include "ex.h"

shift(c, cnt)
	char c;
	int cnt;
{
	register int *addr;
	register char *cp;
	char *dp;
	register int i;

	if (!inglobal)
		save12(), undkind = UNDCHANGE;
	cnt =* value(SHIFTWIDTH);
	for (addr = addr1; addr <= addr2; addr++) {
		getline(*addr);
		i = whitecnt(linebuf);
		switch (c) {
			case '>':
				if (linebuf[0] == 0)
					continue;
				cp = genindent(i + cnt);
				break;
			case '<':
				if (i == 0)
					continue;
				i =- cnt;
				cp = i > 0 ? genindent(i) : genbuf;
				break;
		}
		dot = addr;
		if (cp + strlen(dp = vpastwh(linebuf)) >= &genbuf[LBSIZE - 2])
			error("Line too long|Result line after shift would be too long");
		strcpy(cp, dp);
		strcLIN(genbuf);
		putmark(addr);
	}
}
