/* m_seqbits.c - return the sprintb() string for a sequence */

#include "../h/mh.h"
#include <stdio.h>


char   *m_seqbits (mp)
register struct msgs *mp;
{
    int     bits;
    register int    i;
    static char buffer[BUFSIZ];

    bits = FFATTRSLOT;
    (void) strcpy (buffer, MBITS);
    for (i = 0; mp -> msgattrs[i]; i++)
	(void) sprintf (buffer + strlen (buffer), "%c%s",
		bits + i + 1, mp -> msgattrs[i]);

    return buffer;
}
