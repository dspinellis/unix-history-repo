/* m_seq.c - print out a message sequence */

#include "../h/mh.h"
#include <stdio.h>


char   *m_seq (mp, cp)
struct msgs *mp;
char   *cp;
{
    int     bits,
            found,
            hack;
    register int    i,
                    j,
                    k;
    register char  *bp;
    static char buffer[BUFSIZ];

    bits = FFATTRSLOT;
    hack = strcmp (current, cp) == 0;
    for (i = 0; mp -> msgattrs[i]; i++)
	if (strcmp (mp -> msgattrs[i], cp) == 0) {
	    found = 0;
	    bp = buffer;
	    for (j = mp -> lowmsg; j <= mp -> hghmsg; j++)
		if ((mp -> msgstats[j] & EXISTS)
			&& (mp -> msgstats[j] & (1 << (bits + i)))) {
		    (void) sprintf (bp, "%s%s", found ? " " : "", m_name (j));
		    bp += strlen (bp);
		    for (k = j + 1; k <= mp -> hghmsg
			    && (mp -> msgstats[k] & EXISTS)
			    && (mp -> msgstats[k] & (1 << (bits + i)));
			    k++)
			continue;
		    if (--k > j) {
			(void) sprintf (bp, "-%s", m_name (k));
			bp += strlen (bp);
		    }
		    j = k + 1;
		    found++;
		}
	    if (found == 0 && hack && mp -> curmsg) {
		found++;
		(void) sprintf (buffer, "%s", m_name (mp -> curmsg));
	    }
	    return (found > 0 ? buffer : NULL);
	}

    return NULL;
}
