/* m_seqflag.c - return the flag (bit) for a sequence */

#include "../h/mh.h"


int     m_seqflag (mp, cp)
register struct msgs *mp;
register char   *cp;
{
    int     bits;
    register int    i;

    bits = FFATTRSLOT;
    for (i = 0; mp -> msgattrs[i]; i++)
	if (strcmp (mp -> msgattrs[i], cp) == 0)
	    return (1 << (bits + i));

    return 0;
}
