/* m_atoi.c - parse a string representation of a message number */

#include "../h/mh.h"


m_atoi (str)
register char *str;
{
    register int    i;
    register char  *cp;

    i = 0;
    cp = str;
    while (*cp) {
	if (*cp < '0' || *cp > '9')
	    return 0;
	i *= 10;
	i += *cp++ - '0';
    }

    return i;
}
