/* @(#)bmove.c	1.3	8/2/80 */

bmove(s, d, l)
	register char *s, *d;
	register int l;
{
	while (l-- > 0)
		*d++ = *s++;
}
