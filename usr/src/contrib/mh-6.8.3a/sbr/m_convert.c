/* m_convert.c - parse a message sequence and set SELECTED */
#ifndef	lint
static char ident[] = "@(#)$Id: m_convert.c,v 1.10 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>
#include <ctype.h>

#define	BADMSG	(-2)
#define	BADRNG	(-3)
#define	BADNEW	(-4)
#define	BADNUM	(-5)
#define	BADLST	(-6)

#define	FIRST	1
#define	LAST	2

#define	getnew(mp)	(mp -> hghmsg + 1)

static int  convdir;
static char *delimp;

static m_conv(), attr();
/*  */

m_convert (mp, name)
register struct msgs *mp;
register char   *name;
{
    register int    first,
                    last;
    register char  *bp,
		   *cp;
    int     found,
            range,
            err,
            flags;

    switch (err = attr (mp, cp = name)) {
	case NOTOK: 
	    return 0;

	case OK: 
	    break;

	default: 
	    if (err < 0)
		goto badmsg;
	    return 1;
    }

    found = 0;
    flags = mp -> msgflags & MHPATH ? EXISTS | SELECT_EMPTY : EXISTS;

    if ((mp -> msgflags & MHPATH) && strcmp (cp, "new") == 0)
	if ((err = first = getnew (mp)) <= 0)
	    goto badmsg;
	else
	    goto single;
    if (strcmp (cp, "all") == 0)
	cp = "first-last";
    if ((err = first = m_conv (mp, cp, FIRST)) <= 0)
	goto badmsg;
    if (*(cp = delimp) && *cp != '-' && *cp != ':') {
badelim: ;
	advise (NULLCP, "illegal argument delimiter: `%c'(0%o)",
		*delimp, *delimp);
	return 0;
    }
    if (*cp == '-') {
	cp++;
	if ((err = last = m_conv (mp, cp, LAST)) <= 0) {
    badmsg: ;
	    switch (err) {
		case BADMSG: 
		    advise (NULLCP, "no %s message", cp);
		    break;

		case BADNUM: 
		    advise (NULLCP, "message %s doesn't exist", cp);
		    break;

		case BADRNG: 
		    advise (NULLCP, "message %s out of range 1-%d",
			    cp, mp -> hghmsg);
		    break;

		case BADLST: 
	    badlist: ;
		    advise (NULLCP, "bad message list %s", name);
		    break;

		case BADNEW:
		    advise (NULLCP, "folder full, no %s message", name);
		    break;

		default: 
		    advise (NULLCP, "no messages match specification");
	    }
	    return 0;
	}
	if (last < first)
	    goto badlist;
	if (*delimp)
	    goto badelim;
	if (first > mp -> hghmsg || last < mp -> lowmsg) {
    rangerr: ;
	    advise (NULLCP, "no messages in range %s", name);
	    return 0;
	}
	if (last > mp -> hghmsg)
	    last = mp -> hghmsg;
	if (first < mp -> lowmsg)
	    first = mp -> lowmsg;
	}
    else
	if (*cp == ':') {
	    cp++;
	    if (*cp == '-') {
		convdir = -1;
		cp++;
	    }
	    else
		if (*cp == '+') {
		    convdir = 1;
		    cp++;
		}
	    if ((range = atoi (bp = cp)) == 0)
		goto badlist;
	    while (isdigit (*bp))
		bp++;
	    if (*bp)
		goto badelim;
	    if ((convdir > 0 && first > mp -> hghmsg)
		    || (convdir < 0 && first < mp -> lowmsg))
		goto rangerr;
	    if (first < mp -> lowmsg)
		first = mp -> lowmsg;
	    if (first > mp -> hghmsg)
		first = mp -> hghmsg;
	    for (last = first;
		    last >= mp -> lowmsg && last <= mp -> hghmsg;
		    last += convdir)
		if (mp -> msgstats[last] & EXISTS)
		    if (--range <= 0)
			break;
	    if (last < mp -> lowmsg)
		last = mp -> lowmsg;
	    if (last > mp -> hghmsg)
		last = mp -> hghmsg;
	    if (last < first) {
		range = last;
		last = first;
		first = range;
	    }
	}
	else {
	    if (!(mp -> msgflags & MHPATH))
		if (first > mp -> hghmsg
			|| first < mp -> lowmsg
			|| !(mp -> msgstats[first] & EXISTS)) {
		    if (strcmp (name, "cur") == 0 || strcmp (name, ".") == 0)
			advise (NULLCP, "no %s message", name);
		    else
			advise (NULLCP, "message %d doesn't exist", first);
		    return 0;
		}
    single: ;
	    last = first;
	    if (mp -> msgflags & MHPATH)
		mp -> msgstats[first] |= SELECT_EMPTY;
	}
    for (; first <= last; first++)
	if (mp -> msgstats[first] & flags) {
	    if (!(mp -> msgstats[first] & SELECTED)) {
		mp -> numsel++;
		mp -> msgstats[first] |= SELECTED;
		if (mp -> lowsel == 0 || first < mp -> lowsel)
		    mp -> lowsel = first;
		if (first > mp -> hghsel)
		    mp -> hghsel = first;
	    }
	    found++;
	}
    if (!found)
	goto rangerr;

    return 1;
}

/*  */

static  m_conv (mp, str, call)
register struct msgs *mp;
register char   *str;
register int     call;
{
    register int    i;
    register char  *cp,
                   *bp;
    char    buf[16];

    convdir = 1;
    cp = bp = str;
    if (isdigit (*cp)) {
	while (isdigit (*bp))
	    bp++;
	delimp = bp;
	return ((i = atoi (cp)) <= mp -> hghmsg ? i
		: *delimp || call == LAST ? mp -> hghmsg + 1
		: mp -> msgflags & MHPATH ? BADRNG : BADNUM);
    }

    bp = buf;
#ifndef LOCALE
    while ((*cp >= 'a' && *cp <= 'z') || *cp == '.')
#else
    while (isalpha(*cp) || *cp == '.')	/* doesn't enforce lower case */
#endif /* LOCALE */
	*bp++ = *cp++;
    *bp++ = 0;
    delimp = cp;

    if (strcmp (buf, "first") == 0)
	return (mp -> hghmsg || !(mp -> msgflags & MHPATH)
		? mp -> lowmsg : BADMSG);

    if (strcmp (buf, "last") == 0) {
	convdir = -1;
	return (mp -> hghmsg || !(mp -> msgflags & MHPATH)
		? mp -> hghmsg : BADMSG);
    }

    if (strcmp (buf, "cur") == 0 || strcmp (buf, ".") == 0)
	return (mp -> curmsg > 0 ? mp -> curmsg : BADMSG);

    if (strcmp (buf, "prev") == 0) {
	convdir = -1;
	for (i = (mp -> curmsg <= mp -> hghmsg) ? mp -> curmsg - 1 : mp -> hghmsg;
		i >= mp -> lowmsg; i--) {
	    if (mp -> msgstats[i] & EXISTS)
		return i;
	}
	return BADMSG;
    }

    if (strcmp (buf, "next") == 0) {
	for (i = (mp -> curmsg >= mp -> lowmsg) ? mp -> curmsg + 1 : mp -> lowmsg;
		i <= mp -> hghmsg; i++) {
	    if (mp -> msgstats[i] & EXISTS)
		return i;
	}
	return BADMSG;
    }

    return BADLST;
}

/*  */

static  attr (mp, cp)
register struct msgs *mp;
register char   *cp;
{
    char   *bp = (char *)NULL;
    int     bits,
            found,
            inverted,
            range = 0,		/* no range */
	    first = 0;
    register int    i,
                    j;
    register char  *dp;

    if (strcmp (cp, "cur") == 0)/* hack for "cur-xyz", etc. */
	return OK;
    if (ssequal ("cur:", cp))	/* this code need to be rewritten... */
	return OK;

    if (inverted = (dp = m_find (nsequence)) && *dp && ssequal (dp, cp))
	cp += strlen (dp);

    convdir = 1;
    for (dp = cp; *dp && isalnum(*dp); dp++)
	continue;
    if (*dp == ':') {
	bp = dp++;

	range = 1;
	if (isalpha (*dp)) {	/* optimize? */
	    if (strcmp (dp, "prev") == 0) {
		convdir = -1;
		first = (mp -> curmsg > 0) && (mp -> curmsg <= mp -> hghmsg)
			? mp -> curmsg - 1
			: mp -> hghmsg;
	    }
	    else if (strcmp (dp, "next") == 0) {
		convdir = 1;
		first = (mp -> curmsg >= mp -> lowmsg)
			    ? mp -> curmsg + 1
			    : mp -> lowmsg;
	    }
	    else if (strcmp (dp, "first") == 0) {
		convdir = 1;
	    }
	    else if (strcmp (dp, "last") == 0) {
		convdir = -1;
	    }
	    else
		return BADLST;
	}
	else {			/* a numeric range */
	    if (*dp == '+')
		dp++;
	    else if (*dp == '-') {
		dp++;
		convdir = -1;
	    }
	    if ((range = atoi(dp)) == 0)
		return BADLST;
	    while (isdigit (*dp))
		dp++;
	    if (*dp)
		return BADLST;
	}

	*bp = '\0';		/* terminate sequence name */
    }


    bits = FFATTRSLOT;
    for (i = 0; mp -> msgattrs[i]; i++)
	if (strcmp (mp -> msgattrs[i], cp) == 0)
	    break;
    if (bp)
	*bp = ':';		/* restore sequence name */
    if (mp -> msgattrs[i] == NULL)
	return OK;

    found = 0;
    for (j = first ? first : (convdir > 0) ? mp -> lowmsg : mp -> hghmsg;
		j >= mp -> lowmsg && j <= mp -> hghmsg; j += convdir)
	if ((mp -> msgstats[j] & EXISTS)
		&& inverted ? !(mp -> msgstats[j] & (1 << (bits + i)))
		: mp -> msgstats[j] & (1 << (bits + i))) {
	    if (!(mp -> msgstats[j] & SELECTED)) {
		mp -> numsel++;
		mp -> msgstats[j] |= SELECTED;
		if (mp -> lowsel == 0 || j < mp -> lowsel)
		    mp -> lowsel = j;
		if (j > mp -> hghsel)
		    mp -> hghsel = j;
	    }
	    found++;
	    if (range && found >= range)
		break;		/* we've done enough */
	}
    if (found > 0)
	return found;

    if (first)
	return BADMSG;
    advise (NULLCP, "sequence %s %s", cp, inverted ? "full" : "empty");
    return NOTOK;
}
