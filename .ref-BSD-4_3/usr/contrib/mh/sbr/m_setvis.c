/* m_setvis.c - set the unseen-sequence */

#include "../h/mh.h"
#include <stdio.h>


void m_setvis (mp, seen)
register struct msgs *mp;
int	seen;
{
    register int    msgnum;
    register char  *cp,
                   *dp,
                  **ap;

    dp = NULL;
    if ((cp = m_find (usequence)) == NULL
	    || (ap = brkstring (dp = getcpy (cp), " ", "\n")) == NULL
	    || *ap == NULL) {
	if (dp)
	    free (dp);
	return;
    }

    for (; *ap; ap++)
	if (seen) {
	    if (m_seqflag (mp, *ap))
		for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
		    if (mp -> msgstats[msgnum] & UNSEEN)
			(void) m_seqdel (mp, *ap, msgnum);
	}
	else
	    for (msgnum = mp -> lowmsg; msgnum <= mp -> hghmsg; msgnum++)
		if (mp -> msgstats[msgnum] & UNSEEN)
		    (void) m_seqadd (mp, *ap, msgnum, -1);

    if (dp)
	free (dp);
}
