#include "ex.h"
#include "ex_re.h"
/*
 * Ex - a text editor
 * Bill Joy UCB October, 1977
 */

/*
 * Unpack into linebuf
 */
unpack(sp)
	register char *sp;
{
	register char *lp;
	register int c;
	int i;

	for (lp = linebuf; c = *sp++;) {
		if ((c & 0200) != 0) {
			c =& 0177;
			switch (c) {

			case 0:
				c = ' ';
gosh:
				for (i = *sp++; i > 0; i--)
					*lp++ = c;
				continue;
#ifdef UNIMP
			case 012:
				*lp++ = '\t';
				*lp++ = '\t';
				continue;
#endif
			case 015:
				c = '\t';
				goto gosh;
			}
			*lp++ = ' ';
		}
		*lp++ = c;
	}
	*lp = 0;
	return (linebuf);
}

pack(sp)
	register char *sp;
{
	register char *lp;
	register int c;
	int i;

	for (lp = linebuf; c = *lp++;) {
		if (c == '\n') {
			linebp = lp;
			break;
		}
		if (c != ' ' && c != '\t' || *lp == 0 || c == '\t' && *lp != '\t') {
			*sp++ = c;
			continue;
		}
		if (c == ' ' && *lp != ' ' && *lp != '\n') {
			*sp++ = *lp++ | 0200;
			continue;
		}
		for (i = 1; *lp == c && i < 127; lp++, i++)
			continue;
#ifdef UNIMP
		if (c == '\t' && i == 2) {
			*sp++ = 0212;
			continue;
		}
#endif
		*sp++ = c == '\t' ? 0215 : 0200;
		*sp++ = i;
	}
	*sp = 0;
}
