/* @(#)bmove.c	2.1	11/5/80 */

bmove(s, d, l)
	register char *s, *d;
	register int l;
{
	while (l-- > 0)
		*d++ = *s++;
}
