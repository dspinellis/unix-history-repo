/* m_atoi.c - parse a string representation of a message number */
#ifndef	lint
static char ident[] = "@(#)$Id: m_atoi.c,v 1.2 1992/10/26 22:52:05 jromine Exp $";
#endif /* lint */

#include "../h/mh.h"


m_atoi (str)
register char *str;
{
    register int    i;
    register char  *cp;

    i = 0;
    cp = str;
#ifdef LOCALE
    while (isdigit(*cp)) {
	i *= 10;
	i += *cp++ - '0';
    }
#else
    while (*cp) {
	if (*cp < '0' || *cp > '9')
	    return 0;
	i *= 10;
	i += *cp++ - '0';
    }
#endif

    return i;
}
