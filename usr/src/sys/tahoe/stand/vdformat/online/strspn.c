#ifndef lint
static char sccsid[] = "@(#)strspn.c	1.1 (Berkeley/CCI) 7/5/86";
#endif

/*	@(#)strspn.c	1.2	*/
/*LINTLIBRARY*/
/*
 * Return the number of characters in the maximum leading segment
 * of string which consists solely of characters from charset.
 */
int
strspn(string, charset)
char	*string;
register char	*charset;
{
	register char *p, *q;

	for(q=string; *q != '\0'; ++q) {
		for(p=charset; *p != '\0' && *p != *q; ++p)
			;
		if(*p == '\0')
			break;
	}
	return(q-string);
}
