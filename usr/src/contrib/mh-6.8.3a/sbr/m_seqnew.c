/* m_seqnew.c - manage sequences */
#ifndef	lint
static char ident[] = "@(#)$Id: m_seqnew.c,v 1.7 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <ctype.h>
#include <stdio.h>

static int	m_seqok();

int     m_seqnew (mp, cp, public)
register struct msgs *mp;
register char   *cp;
register int	public;
{
    int     bits;
    register int    i,
                    j;

    if (!m_seqok (cp))
	return 0;

    if (public == -1)		/* XXX */
	public = mp -> msgflags & READONLY ? 0 : 1;

    bits = FFATTRSLOT;
    for (i = 0; mp -> msgattrs[i]; i++)
	if (strcmp (mp -> msgattrs[i], cp) == 0) {
	    for (j = mp -> lowmsg; j <= mp -> hghmsg; j++)
		mp -> msgstats[j] &= ~(1 << (bits + i));
	    if (public)
		mp -> attrstats &= ~(1 << (bits + i));
	    else
		mp -> attrstats |= 1 << (bits + i);
	    mp -> msgflags |= SEQMOD;

	    return 1;
	}

    if (i >= NATTRS) {
	advise (NULLCP, "only %d sequences allowed (no room for %s)!",
		NATTRS, cp);
	return 0;
    }

    mp -> msgattrs[i] = getcpy (cp);
    for (j = mp -> lowmsg; j <= mp -> hghmsg; j++)
	mp -> msgstats[j] &= ~(1 << (bits + i));
    if (public)
	mp -> attrstats &= ~(1 << (bits + i));
    else
	mp -> attrstats |= 1 << (bits + i);
    mp -> msgflags |= SEQMOD;

    mp -> msgattrs[++i] = NULL;

    return 1;
}

/*  */

int     m_seqadd (mp, cp, j, public)
register struct msgs *mp;
register char   *cp;
register int     j,
		 public;
{
    int     bits;
    register int    i,
                    k;

    if (!m_seqok (cp))
	return 0;

    /* keep mp->curmsg & msgattrs["cur"] in sync - see m_seq() */
    if (strcmp (current,cp) == 0)
	mp->curmsg = j;	

    if (public == -1)		/* XXX */
	public = mp -> msgflags & READONLY ? 0 : 1;

    bits = FFATTRSLOT;
    for (i = 0; mp -> msgattrs[i]; i++)
	if (strcmp (mp -> msgattrs[i], cp) == 0) {
	    mp -> msgstats[j] |= 1 << (bits + i);
	    if (public)
		mp -> attrstats &= ~(1 << (bits + i));
	    else
		mp -> attrstats |= 1 << (bits + i);
	    mp -> msgflags |= SEQMOD;

	    return 1;
	}

    if (i >= NATTRS) {
	advise (NULLCP, "only %d sequences allowed (no room for %s)!",
		NATTRS, cp);
	return 0;
    }

    mp -> msgattrs[i] = getcpy (cp);
    for (k = mp -> lowmsg; k <= mp -> hghmsg; k++)
	mp -> msgstats[k] &= ~(1 << (bits + i));
    mp -> msgstats[j] |= 1 << (bits + i);
    if (public)
	mp -> attrstats &= ~(1 << (bits + i));
    else
	mp -> attrstats |= 1 << (bits + i);
    mp -> msgflags |= SEQMOD;

    mp -> msgattrs[++i] = NULL;

    return 1;
}

/*  */

int     m_seqdel (mp, cp, j)
register struct msgs *mp;
register char   *cp;
register int     j;
{
    int     bits;
    register int    i;

    if (!m_seqok (cp))
	return 0;

    bits = FFATTRSLOT;
    for (i = 0; mp -> msgattrs[i]; i++)
	if (strcmp (mp -> msgattrs[i], cp) == 0) {
	    mp -> msgstats[j] &= ~(1 << (bits + i));
	    mp -> msgflags |= SEQMOD;

	    return 1;
	}

    advise (NULLCP, "no such sequence as %s", cp);
    return 0;
}

/*  */

static int  m_seqok (cp)
register char   *cp;
{
    register char  *pp;

    if (cp == NULL || *cp == 0) {
	advise (NULLCP, "empty sequence name");
	return 0;
    }

    if (strcmp (cp, "new") == 0
#ifdef	notdef
	    || strcmp (cp, "cur") == 0
#endif	/* notdef */
	    || strcmp (cp, "all") == 0
	    || strcmp (cp, "first") == 0
	    || strcmp (cp, "last") == 0
	    || strcmp (cp, "prev") == 0
	    || strcmp (cp, "next") == 0) {
	advise (NULLCP, "illegal sequence name: %s", cp);
	return 0;
    }

    if (!isalpha (*cp)) {
	advise (NULLCP, "illegal sequence name: %s", cp);
	return 0;
    }
    for (pp = cp + 1; *pp; pp++)
	if (!isalnum (*pp)) {
	    advise (NULLCP, "illegal sequence name: %s", cp);
	    return 0;
	}

    return 1;
}
