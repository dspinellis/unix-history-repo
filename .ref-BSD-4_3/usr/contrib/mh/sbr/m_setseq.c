/* m_setseq.c - set the previous-sequence */

#include "../h/mh.h"
#include <stdio.h>


void m_setseq (mp)
register struct msgs *mp;
{
    register int    msgnum;
    register char  *cp,
                   *dp,
                  **ap;

    dp = NULL;
    if ((cp = m_find (psequence)) == NULL
	    || (ap = brkstring (dp = getcpy (cp), " ", "\n")) == NULL
	    || *ap == NULL) {
	if (dp)
	    free (dp);
	return;
    }

    for (; *ap; ap++)
	if (m_seqnew (mp, *ap, -1))
	    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
		if (mp -> msgstats[msgnum] & SELECTED)
		    (void) m_seqadd (mp, *ap, msgnum, -1);

    if (dp)
	free (dp);
}
