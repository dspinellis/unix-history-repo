/* m_remsg.c - realloc a msgs structure */
#ifndef	lint
static char ident[] = "@(#)$Id: m_remsg.c,v 1.8 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>


struct msgs *m_remsg (mp, lo, hi)
register struct msgs *mp;
int	lo,
	hi;
{
    int     msgnum;
#ifdef	MTR
    register int   *sp,
		   *pp;
#endif	/* MTR */

    if (lo == 0 && (lo = mp -> lowmsg) == 0)
	lo = 1;
    if (hi < mp -> hghmsg)
	hi = mp -> hghmsg + (MAXFOLDER - mp -> nummsg);
    if (hi <= mp -> hghmsg)
	hi = mp -> hghmsg + MAXFOLDER;
    if (lo == mp -> lowmsg && hi == mp -> hghmsg)
	return mp;

#ifndef	MTR
    mp = (struct msgs  *) realloc ((char *) mp, MHSIZE (mp, lo, hi));
    if (mp == NULL)
	adios (NULLCP, "unable to re-allocate folder storage");
#else	/* MTR */
    if ((sp = (int *) calloc ((unsigned) 1, MHSIZEX (mp, lo, hi))) == NULL)
	adios (NULLCP, "unable to re-allocate messages storage");

    pp = sp - lo;
    if (pp < (int *)0)
	adios (NULLCP, "m_remsg() botch -- you lose big[1]");
    for (msgnum = mp -> lowmsg; msgnum <= mp -> hghmsg; msgnum++)
	pp[msgnum] = mp -> msgstats[msgnum];
    free ((char *) mp -> msgbase);
    mp -> msgstats = sp;
#endif	/* MTR */
    mp -> lowoff = lo;
    mp -> hghoff = hi;
#ifdef	MTR
    mp -> msgstats = (mp -> msgbase = mp -> msgstats) - mp -> lowoff;
    if (mp -> msgstats < (int *)0)
	adios (NULLCP, "m_remsg() botch -- you lose big[2]");
#endif	/* MTR */
    for (msgnum = mp -> lowmsg - 1; msgnum >= lo; msgnum--)
	mp -> msgstats[msgnum] = 0;
    for (msgnum = mp -> hghmsg + 1; msgnum <= hi; msgnum++)
	mp -> msgstats[msgnum] = 0;

    return mp;
}
