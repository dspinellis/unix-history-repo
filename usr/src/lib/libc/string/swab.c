#ifndef lint
static char sccsid[] = "@(#)swab.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * Swab bytes
 * Jeffrey Mogul, Stanford
 */

swab(from, to, n)
	register char *from, *to;
	register int n;
{
	register unsigned long temp;
	
	n >>= 1; n++;
#define	STEP	temp = *from++,*to++ = *from++,*to++ = temp
	/* round to multiple of 8 */
	while ((--n) & 07)
		STEP;
	n >>= 3;
	while (--n >= 0) {
		STEP; STEP; STEP; STEP;
		STEP; STEP; STEP; STEP;
	}
}
