/*
char id_rindex[] = "@(#)rindex_.c	1.2";
 *
 * find last occurrence of substring in string
 *
 * calling sequence:
 *	character*(*) substr, string
 *	indx = rindex (string, substr)
 * where:
 *	indx will be the index of the first character of the last occurence
 *	of substr in string, or zero if not found.
 */

long rindex_(str, substr, slen, sublen)
char *str, *substr; long slen, sublen;
{
	register char *p = str + (slen - sublen);

	while (p >= str)
		if (s_cmp(substr, p, sublen, slen) == 0)
			return((long)(++p - str));
		else
			p--;
	return(0L);
}
