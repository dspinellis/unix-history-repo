/* m_setcur.c - set "cur" */

#include "../h/mh.h"
#include <stdio.h>


void m_setcur (mp, num)
register struct  msgs *mp;
register int     num;
{
    int     bits,
            public;
    register int    i;

    public = mp -> msgflags & READONLY ? 0 : 1;
    bits = FFATTRSLOT;
    for (i = 0; mp -> msgattrs[i]; i++)
	if (strcmp (mp -> msgattrs[i], current) == 0) {
	    public = mp -> attrstats & (1 << (bits + i)) ? 0 : 1;
	    break;
	}

    if (!m_seqnew (mp, current, public))
	return;
    (void) m_seqadd (mp, current, mp -> curmsg = num, public);
}
