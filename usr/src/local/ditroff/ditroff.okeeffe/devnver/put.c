#ifndef lint
static char sccsid[] = "@(#)put.c	1.1	87/07/06";
#endif

/********************************************************************
 *
 * code to put a character
 *
 *******************************************************************/

#include "the.h"

extern struct	dev dev;
extern struct	Font *fontbase[];
extern int	output;
extern short	*pstab;
extern int	nfonts, smnt, nchtab;
extern char	*chname, *fitab[];
extern char	*widthtab[], *codetab[];
extern short	*chtab, *fonttab[];
extern struct	fontname fontname[];
extern int	size, font, hpos, vpos, lastw, maxv;
extern int	virtRES;
extern char	buffer[];
extern int	vorigin, pagelen;
extern int	debug, dbg;




/* 
 * s is the name of a special character
 */

put1s(s)	
char *s;
{
	int i;

	if (!output)
		return;

	DBGPRINT(2, ("%s ", s));

	for (i = 0; i < nchtab; i++)
		if (strcmp(&chname[chtab[i]], s) == 0)
			break;

	if (i < nchtab)
		put1(i + 128);
}


/* 
 *  output char c
 */

put1(c)	
int c;
{
	char *pw;
	register char *p;
	register int i, k;
	int j, ofont, code;
	short f;

	if (!output)
		return;

	c -= 32;
	if (c <= 0) {
		DBGPRINT(0, ("non-exist 0%o\n", c+32));
		lastw = (widthtab[font][0] * pstab[size] + dev.unitwidth/2)/dev.unitwidth;
		return;
	}
	k = ofont = font;
	i = fitab[font][c] & BMASK;
	if (i != 0) {	/* it's on this font */
		p = codetab[font];
		pw = widthtab[font];
	} else if (smnt > 0) {		/* on special (we hope) */
		for (k=smnt, j=0; j <= nfonts; j++, k = (k+1) % (nfonts+1))
		/*
		 * Look for the character, start at the special font
		 * and search further in a wrap around manner 
		 */ 
			if ((i = fitab[k][c] & BMASK) != 0) {
				p = codetab[k];
				pw = widthtab[k];
				setfont(k);
				break;
			}
	}
	if (i == 0 || (code = p[i] & BMASK) == 0 || k > nfonts) {
#ifdef DEBUGABLE
		if (dbg) {
			if (isprint(c+32) && isascii(c+32)) 
				fprintf(stderr,"not found %c\n", c+32);
			else
				fprintf(stderr,"not found \\(%s\n", &chname[chtab[c -128+32]]);
		}
#endif DEBUGABLE
		return;
	}
	if (fontbase[k]->fonttab == 1){
		f = fonttab[k][i];
	}
	else {
		f = fontname[k].number;
	}
#ifdef DEBUGABLE
	if (dbg) {
		if (isprint(c+32) && isascii(c+32)) { /* My God! */
			fprintf(stderr,"%c %d %d\n", c+32, code, f);
		}
		else
			fprintf(stderr,"\\(%s %d %d\n", &chname[chtab[c -128+32]], code, f);
	}
#endif DEBUGABLE
	/*
	if(code == 0 || code > 0200) {
		error(FATAL,"Illegal code 0%o found for char %03o\n", code, c+32);
	}
	*/
	putcode(code, f);	/* character is < 254 */
	if (font != ofont)	/* char on special font, reset	*/
		setfont(ofont);
	lastw = pw[i] & NMASK;

	lastw = (lastw * pstab[size] + dev.unitwidth/2) / dev.unitwidth;
}





/*
 * Plot a dot at (x, y).  
 *
 * x and y are *VIRTUAL* coordinates (a change from prev. versions).
 *
 * The physical mapping of the point should be in the range 0 <= x < RASTER_LENGTH,
 * vorigin <= y < vorigin + NLINES.  If the point will not fit on the buffer,
 * it is left out.  Things outside the x boundary are wrapped around the end.
 */

point(x, y)
register int x;
register int y;
{
    register char *ptr;

    x = PHYS(x);
    y = PHYS(y);

    ptr = buf0p + (y - vorigin) * BYTES_PER_LINE + (x >> 3);

    if (ptr > BUFBOTTOM || ptr < BUFTOP)	/* ignore if it's off buffer */
	return;

    *ptr |= 1 << (7 - (x & 07));
    if (y > maxv) maxv = y;
}
