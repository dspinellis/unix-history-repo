/* @(#)swab.c	4.1 (Berkeley) 12/21/80 */
/*
 * Swap bytes in 16-bit [half-]words
 * for going between the 11 and the interdata
 */

swab(pf, pt, n)
register short *pf, *pt;
register n;
{

	n /= 2;
	while (--n >= 0) {
		*pt++ = (*pf << 8) + ((*pf >> 8) & 0377);
		pf++;
	}
}
