/*
char id_lnblnk[] = "@(#)lnblnk_.c	1.1";
 *
 * find last occurrence of a non-blank character in string
 *
 * calling sequence:
 *	character*(*) string
 *	indx = lnblnk (string)
 * where:
 *	indx will be the index of the last occurence
 *	of a non-blank character in string, or zero if not found.
 */

long lnblnk_(str, slen)
char *str; long slen;
{
	register char *p = str + slen;

	while (--p >= str && *p == ' ' ) ;
	return((long)(++p - str));
}
