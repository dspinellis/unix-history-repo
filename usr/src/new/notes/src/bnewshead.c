/*
 * header.c - header functions plus some other goodies
 *
 *	TAKEN FROM BNEWS 2.10 6/24/83
 *
 */

#ifdef	RCSIDENT
static char *SccsId = "@(#)header.c	2.20	6/24/83";
static char *RCSid = "$Header: bnewshead.c,v 1.7.0.1 85/03/06 20:03:00 notes Rel $";
#endif	RCSIDENT

#include <stdio.h>
#include <sys/types.h>
#include	"parms.h"				/* from notes */
#include	"structs.h"				/* ditto */
							/* above maybe unused */
#include	"newsgate.h"


char   *hfgets ();

static int  seenrelay;
static char bfr[PATHLEN];				/* header buffer */

/*
 * Read header from file fp into *hp.  If wholething is FALSE,
 * it's an incremental read, otherwise start from scratch.
 * Return (FILE *) if header okay, else NULL.
 */

newsheader (hp, fp, wholething)
register struct hbuf   *hp;
FILE * fp;
int     wholething;
{
    register int    len;

    if (wholething)					/* from scratch */
	bclear ((char *) hp, sizeof (*hp));

    seenrelay = 0;

/*
 *	Check that it's a B news style header.
 */
    if (((hfgets (bfr, PATHLEN, fp) != NULL &&
		    *bfr >= 'A' && *bfr <= 'Z') && index (bfr, ':')))
	if (frmread (fp, hp))
	    goto strip;

/*
 * It's not.  Try A news (begins with PROTO).
 */
    if (*bfr != PROTO)
	return (0);

/*
 *	Read in an A news format article.
 */
    strncpy (hp -> oident, &(bfr[1]), NAMELEN);		/* file name */
    if (!nstrip (hp -> oident))
	return (0);
    hfgets (hp -> nbuf, BUFLEN, fp);			/* newsgroup list */
    if (!nstrip (hp -> nbuf))
	return (0);
    ngcat (hp -> nbuf);					/* trailing delim */
    hfgets (hp -> path, PATHLEN, fp);			/* source path */
    if (!nstrip (hp -> path))
	return (0);
    hfgets (hp -> subdate, DATELEN, fp);		/* date */
    if (!nstrip (hp -> subdate))
	return (0);
    hfgets (hp -> title, BUFLEN, fp);			/* title */
    if (!nstrip (hp -> title))
	return (0);

/*
 * strip off sys! from front of path.
 */
strip: 
    strcpy (bfr, System);
    if (strncmp (bfr, hp -> path, (len = strlen (bfr))) == 0
	    && index (NETCHRS, hp -> path[len]))
	strcpy (hp -> path, &(hp -> path[len + 1]));

    if (wholething && hp -> from[0] == '\0')		/* intuit the from: */
	intuitfrom (hp);				/* if wasn't there */

    if (wholething)					/* Get message ID's. */
	fixid (hp);
    return (1);
}


/*
 * Get header info from mail-format file.
 * Return non-zero on success.
 */

#include <ctype.h>
#define FROM 		1
#define NEWSGROUP 	2
#define TITLE 		3
#define SUBMIT		4
#define RECEIVE		5
#define EXPIRE		6
#define ARTICLEID	7
#define MESSAGEID	8
#define REPLYTO		9
#define FOLLOWID	10
#define CONTROL		11
#define SENDER		12
#define FOLLOWTO	13
#define PATH		14
#define POSTVERSION	15
#define RELAYVERSION	16
#define DISTRIBUTION	17
#define ORGANIZATION	18
#define NUMLINES	19
#define KEYWORDS	20
#define APPROVED	21

#define	NLINE1		22
#define	NLINE2		23

#define OTHER		99


char   *malloc ();

frmread (fp, hp)
register    FILE * fp;
register struct hbuf   *hp;
{
    int     unreccnt = 0;
    register int    i;
    long    curpos;
    int     hdrlineno = 0;
    int     iu;

    for (iu = 0; iu < NUNREC; iu++)
	hp -> unrec[iu] = NULL;

    i = type (bfr);
    do
    {
	curpos = ftell (fp);
	hdrlineno++;
	switch (i)
	{
	    case PATH: 
		getfield (hp -> path);
		break;
	    case FROM: 
		getfield (hp -> from);
		break;
	    case NEWSGROUP: 
		getfield (hp -> nbuf);
		break;
	    case TITLE: 
		getfield (hp -> title);
		break;
	    case SUBMIT: 
		getfield (hp -> subdate);
		break;
	    case RECEIVE: 
		getfield (hp -> recdate);
		break;
	    case EXPIRE: 
		getfield (hp -> expdate);
		break;
	    case ARTICLEID: 
		getfield (hp -> oident);
		break;
	    case MESSAGEID: 
		getfield (hp -> ident);
		break;
	    case REPLYTO: 
		getfield (hp -> replyto);
		break;
	    case FOLLOWID: 
		getfield (hp -> followid);
		break;
	    case SENDER: 
		getfield (hp -> sender);
		break;
	    case FOLLOWTO: 
		getfield (hp -> followto);
		break;
	    case CONTROL: 
		getfield (hp -> ctlmsg);
		break;
	    case POSTVERSION: 
		getfield (hp -> postversion);
		break;
	    case DISTRIBUTION: 
		getfield (hp -> distribution);
		break;
	    case ORGANIZATION: 
		getfield (hp -> organization);
		break;
	    case NUMLINES: 
		getfield (hp -> numlines);
		hp -> intnumlines = atoi (hp -> numlines);
		break;
	    case KEYWORDS: 
		getfield (hp -> keywords);
		break;
	    case APPROVED: 
		getfield (hp -> approved);
		break;
	    case NLINE1: 				/* notes-specific */
		getfield (hp -> nline1);
		break;
	    case NLINE2: 				/* notes-specific */
		getfield (hp -> nline2);
		break;
	    case RELAYVERSION: 
		/* 
		 * Only believe a relay version if it's the first
		 * line, otherwise it probably got passed through
		 * by some old neighbor.
		 */
		if (hdrlineno == 1)
		{
		    getfield (hp -> relayversion);
		    seenrelay = 1;
		}
		break;
	    case OTHER: 
		if (unreccnt < NUNREC)
		{
		    hp -> unrec[unreccnt] = malloc (strlen (bfr) + 1);
		    strcpy (hp -> unrec[unreccnt], bfr);
		    unreccnt++;
		}
		break;
	}
    } while ((i = type (hfgets (bfr, LBUFLEN, fp))) > 0);

    if (*bfr != '\n')
    {
	printf ("Bizzaro header line: %s\n", bfr);
	return (0);
    }

/*
 *	Check to see if the REQUIRED headers are present.  If so, return
 *	that we found a message. Otherwise barf.
 */
    if ((hp -> from[0] || hp -> path[0]) &&
	    hp -> subdate[0] &&
	    (hp -> ident[0] || hp -> oident[0]))
    {
	return TRUE;
    }
    return FALSE;
}

/*
 * There was no From: line in the message (because it was generated by
 * an old news program).  Guess what it should have been and create it.
 */

intuitfrom (hp)
register struct hbuf   *hp;
{
    char   *tp;
    char   *user,
           *host,
           *fullname;
    char   *tailpath ();
    char   *at,
           *dot;

    tp = tailpath (hp);
    user = rindex (tp, '!');
    if (user == NULL)
	user = tp;
    else
	*user++ = '\0';

    /* Check for an existing Internet address on the end. */
    at = index (user, '@');
    if (at)
    {
	dot = index (at, '.');
	if (dot)
	{
	    strcpy (hp -> from, user);
	    return;
	}
/* @ signs are illegal except for the biggie, so */
	*at = '%';
    }

    if (tp[0] == '.')
	host = index (tp, '!') + 1;
    else
	if (user == tp)
	    host = System;
	else
	    host = tp;

    tp = index (host, '@');
    if (tp != NULL)
	*tp = 0;
    sprintf (hp -> from, "%s@%s.%s", user, host, DFLTDOMAIN);

    fullname = index (hp -> path, '(');
    if (fullname != NULL)
    {
	fullname--;
	strcat (hp -> from, fullname);
	*fullname = 0;
    }
}

/*
 * If the message has only one of ident/oident, guess what
 * the other one should be and fill them both in.
 */

fixid (hp)
register struct hbuf   *hp;
{
    char    lbuf[100];
    char   *p;
#ifdef	OLD
    char   *q;
#endif	OLD

    if (hp -> ident[0] == '\0' && hp -> oident[0] != '\0')
    {
	strcpy (lbuf, hp -> oident);
	p = index (lbuf, '.');
	if (p == 0)
	{
	    strcpy (hp -> ident, hp -> oident);
	    return;
	}
	*p++ = '\0';
	/* 
	 * It may seem strange that we hardwire ".UUCP" in
	 * here instead of DFLTDOMAIN.  However, we are trying
	 * to guess what the domain was on the posting system,
	 * not the local system.  Since we don't really know
	 * what the posting system does, we just go with the
	 * majority - almost everyone will be a .UUCP if they
	 * didn't fill in their Message-ID.
	 */
	sprintf (hp -> ident, "<%s@%s%s>", p, lbuf, ".UUCP");
    }

#ifdef OLD
    if (hp -> oident[0] == '\0' && hp -> ident[0] != '\0')
    {
	strcpy (lbuf, hp -> ident);
	p = index (lbuf, '@');
	if (p == 0)
	{
	    strcpy (hp -> oident, hp -> ident);
	    return;
	}
	*p++ = '\0';
	q = index (p, '.');
	if (!q)
	    q = index (p, '>');
	if (q)
	    *q++ = '\0';
	p[SNLN] = '\0';
	sprintf (hp -> oident, "%s.%s", p, lbuf + 1);
    }
#endif
}

/*
 * Get the given field of a header (char * parm) from bfr, but only
 * if there's something actually there (after the colon).  Don't
 * bother if we already have an entry for this field.
 */

getfield (hpfield)
char   *hpfield;
{
    char   *ptr;

    if (hpfield[0])
	return;
    for (ptr = index (bfr, ':'); isspace (*++ptr);)
	;
    if (*ptr != '\0')
    {
	strcpy (hpfield, ptr);
	nstrip (hpfield);
    }
    return;
}


/*
 *	Determine the type of the header
 */

#define	its(type) (!strncmp(ptr,type,strlen(type)))

type (ptr)
char   *ptr;
{
    char   *colon,
           *space;

    if (!isalpha (*ptr) && strncmp (ptr, "From ", 5))
	return FALSE;
    colon = index (ptr, ':');
    space = index (ptr, ' ');
    if (!colon || colon + 1 != space)
	return FALSE;
    if (its ("From: "))
	if (index (ptr, '@') && !index (ptr, '!') && seenrelay)
	    return FROM;
	else
	    return PATH;
    if (its ("Path: "))
	return PATH;
    if (its ("Newsgroups: "))
	return NEWSGROUP;
    if (its ("Subject: ") || its ("Title: "))
	return TITLE;
    if (its ("Posted: ") || its ("Date: "))
	return SUBMIT;
    if (its ("Date-Received: ") || its ("Received: "))
	return RECEIVE;
    if (its ("Expires: "))
	return EXPIRE;
    if (its ("Article-I.D.: "))
	return ARTICLEID;
    if (its ("Message-ID: "))
	return MESSAGEID;
    if (its ("Reply-To: "))
	return REPLYTO;
    if (its ("References: "))
	return FOLLOWID;
    if (its ("Control: "))
	return CONTROL;
    if (its ("Sender: "))
	return SENDER;
    if (its ("Followup-To: "))
	return FOLLOWTO;
    if (its ("Posting-Version: "))
	return POSTVERSION;
    if (its ("Relay-Version: "))
	return RELAYVERSION;
    if (its ("Distribution: "))
	return DISTRIBUTION;
    if (its ("Organization: "))
	return ORGANIZATION;
    if (its ("Lines: "))
	return NUMLINES;
    if (its ("Keywords: "))
	return KEYWORDS;
    if (its ("Approved: "))
	return APPROVED;
    if (its ("Nf-ID: "))
	return NLINE1;
    if (its ("Nf-From: "))
	return NLINE2;
    return OTHER;
}

/*
 * Set nc bytes, starting at cp, to zero.
 */

bclear (cp, nc)
register char  *cp;
register int    nc;
{
    while (nc--)
	*cp++ = 0;
}

/*
 * Strip trailing newlines, blanks, and tabs from 's'.
 * Return TRUE if newline was found, else FALSE.
 */

nstrip (s)
register char  *s;
{
    register char  *p;
    register int    rc;

    rc = FALSE;
    p = s;
    while (*p)
	if (*p++ == '\n')
	    rc = TRUE;
    while (--p >= s && (*p == '\n' || *p == ' ' || *p == '\t'));
    *++p = '\0';
    return (rc);
}

/*
 * Append NGDELIM to string.
 */

ngcat (s)
register char  *s;
{
    if (*s)
    {
	while (*s++);
	s -= 2;
	if (*s++ == NGDELIM)
	    return;
    }
    *s++ = NGDELIM;
    *s = '\0';
}

/*
 * Return a compact representation of the person who posted the given
 * message.  A sender or internet name will be used, otherwise
 * the last part of the path is used preceeded by an optional ".."
 */
char   *
        tailpath (hp)
struct hbuf *hp;
{
    char   *p,
           *r;
    static char resultbuf[BUFLEN];
    char    pathbuf[PATHLEN];
    char   *malloc ();

    /* 
     * This only happens for articles posted by old news software
     * in non-internet format.
     */
    resultbuf[0] = '\0';
    strcpy (pathbuf, hp -> path);
    p = index (pathbuf, ' ');
    if (p)
	*p = '\0';					/* Chop off trailing " (name)" */
    r = rindex (pathbuf, '!');
    if (r == 0)
    {
	r = pathbuf;
    }
    else
    {
	while (r > pathbuf && *--r != '!')
	    ;
	if (r > pathbuf)
	{
	    r++;
	    strcpy (resultbuf, "..!");
	}
    }
    strcat (resultbuf, r);
    return resultbuf;
}



/*
 * hfgets is like fgets, but deals with continuation lines.
 * It also ensures that even if a line that is too long is
 * received, the remainder of the line is thrown away
 * instead of treated like a second line.
 */

char   *hfgets (buf, len, fp)
char   *buf;
int     len;
FILE * fp;
{
    register int    c;
    register char  *cp,
                   *tp;

    cp = fgets (buf, len, fp);
    if (cp == NULL)
	return NULL;

    tp = cp + strlen (cp);
    if (tp[-1] != '\n')
    {
	/* 
	 * Line too long - part read didn't fit into a newline
	 */
	while ((c = getc (fp)) != '\n' && c != EOF)
	    ;
    }
    else
	*--tp = '\0';					/* clobber newline */

    while ((c = getc (fp)) == ' ' || c == '\t')		/* continuation */
    {
	/* 
	 * Continuation line.
	 */
	while ((c = getc (fp)) == ' ' || c == '\t')	/* skip white space */
	    ;
	if (tp - cp < len)
	{
	    *tp++ = ' ';
	    *tp++ = c;
	}
	while ((c = getc (fp)) != '\n' && c != EOF)
	    if (tp - cp < len)
		*tp++ = c;
    }
    *tp++ = '\n';
    *tp++ = '\0';
    if (c != EOF)
	ungetc (c, fp);					/* push back char */
    return cp;
}
