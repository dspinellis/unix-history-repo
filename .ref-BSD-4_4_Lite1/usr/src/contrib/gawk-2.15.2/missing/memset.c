/*
 * memset --- initialize memory
 *
 * We supply this routine for those systems that aren't standard yet.
 */

char *
memset (dest, val, l)
register char *dest, val;
register int l;
{
	register char *ret = dest;

	while (l--)
		*dest++ = val;

	return ret;
}
