#ifndef lint
static char sccsid[] = "@(#)hash.c	1.1	(Berkeley)	3/29/83";
#endif lint

/*
 * Hash function.  Used for pass 2 symbol table and string table,
 * and structure/union name passing between passes.
 * The hash function is a modular hash of
 * the sum of the characters with the sum
 * rotated before each successive character
 * is added.
 * Only 15 bits are used.
 */
#ifdef FLEXNAMES
hashstr(s)
#else
hashstr(s, n)
register n;
#endif
register char *s;
{
	register i;

	i = 0;
#ifdef FLEXNAMES
	while (*s)
#else
	while (n-- > 0 && *s)
#endif
		i = (i << 3 | i >> 12 & 0x07) + *s++;
	return i & 0x7fff;
}
