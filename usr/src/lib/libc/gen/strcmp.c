#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)strcmp.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

/*
 * Compare strings:  s1>s2: >0  s1==s2: 0  s1<s2: <0
 */

strcmp(s1, s2)
register char *s1, *s2;
{

	while (*s1 == *s2++)
		if (*s1++=='\0')
			return(0);
	return(*s1 - *--s2);
}
