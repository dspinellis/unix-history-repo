/*
 * memcpy --- copy strings.
 *
 * We supply this routine for those systems that aren't standard yet.
 */

char *
memcpy (dest, src, l)
register char *dest, *src;
register int l;
{
	register char *ret = dest;

	while (l--)
		*dest++ = *src++;

	return ret;
}
