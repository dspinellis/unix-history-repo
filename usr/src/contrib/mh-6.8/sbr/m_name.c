/* m_name.c - return a message number as a string */

#include "../h/mh.h"
#include <stdio.h>


static char name[BUFSIZ];

char   *m_name (num)
register int     num;
{
    if (num <= 0)
	return "?";

    (void) sprintf (name, "%d", num);
    return name;
}
