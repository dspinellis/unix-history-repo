#include "parms.h"
#include "structs.h"
#include "newsgate.h"

#ifdef	RCSIDENT
static char *RCSid = "$Header: newsdump.c,v 1.7.0.8 85/07/20 13:43:56 notes Rel $";
#endif	RCSIDENT

/*
 *	newsnote - take a note and dump it in a format that news will
 *	understand. Submit the article to the news program as 
 *	defined in newsgate.h
 *
 *	newsresp - similar to newsnote, but it dumps a response instead.
 *
 *	The routines build some title lines and other headers for 
 *	submission to the news insertion program. The rest of the
 *	article is fed to the news program through a pipe.
 *	This turned out to be mucho mucho easier than building the
 *	properly formatted intersystem files.
 *
 *	Original Coding:	Ray Essick	April 1982
 *	Modified to produce cleaner looking output:
 *				Ray Essick	May 1, 1982
 *				(with good help from Brian Redman (harpo!ber)
 *	Modified to solve meta-character ("'`) problems in popen()
 *				Ray Essick	September 1982
 *
 *	Lots of code by Tw Cook at Hewlett-Packard to implement
 *	compatibility with the USENET standards for news.
 *			January 1984.
 */

extern char *mnames[];					/* month names */
FILE * popen ();

char   *Version = "$Revision: 1.7.0.8 $";		/* Use RCS info */

newsnote (io, note, notenum, ngroup, backwards)
struct io_f *io;
struct note_f  *note;
char   *ngroup;						/* to this newsgroup */
{
    FILE * rnews;					/* rnews pipe */
    char    cmdline[CMDLEN];				/* command line  */
    char    line[TITLEN + 50];				/* scratch line */
    int     i;
    char   *p,
           *q;
    char    ntitle[TITLEN + 10];			/* title */
    char    from[SYSSZ + NAMESZ + 3];			/* formatted author */
    char    path[BUFSIZ];				/* path to author */
    char   *author;					/* for Path line */

/*
 *	format some of the internal data structures in a suitable
 *	format to send to the news system.
 */

    strcpy (ntitle, note -> ntitle);			/* make a title */
    if (backwards)
    {
	strcat (ntitle, " ");				/* separate */
	strcat (ntitle, NFSUFFIX);			/* add suffix */
    }

    /* 
     * From:
     *		author information
     */
    sprintf (from, "%s@%s",				/* author */
	    note -> n_auth.aname, note -> n_auth.asystem);
#ifdef	notdef
    /* 
     * decided it was better to NOT append a domain here...
     */
    if (index (note -> n_auth.asystem, '.') != (char *) NULL)/* domained */
    {
	sprintf (from, "%s@%s",
		note -> n_auth.aname, note -> n_auth.asystem);
    }
    else
    {
	sprintf (from, "%s@%s.%s",
		note -> n_auth.aname, note -> n_auth.asystem,
		DFLTDOMAIN);				/* append default */
    }
#endif	notdef
    /* 
     * Path:
     *	where the article has been
     *  Maybe we want to use some format besides a!b!c, something like
     *  a,b,c
     */
    if (!strcmp (System, note -> n_id.sys))		/* local */
    {
	sprintf (path, "%s", note -> n_id.sys);
    }
    else
    {
	if (!strcmp (note -> n_id.sys, note -> n_from))	/* one hop */
	{
	    sprintf (path, "%s!%s",
		    System,
		    note -> n_id.sys);
	}
	else
	{
	    sprintf (path, "%s!%s!%s",			/* several hops */
		    System, note -> n_from, note -> n_id.sys);
	}
    }
#ifdef	UGLYPATH					/* usually so */
    /* 
     * Fill in the "authorname" with either a placeholder
     * or the real author name. Key this off whether the unique
     * id matches the author's home system.
     *
     * This will usually use AUTHORPLACEHOLDER on new systems
     * since the unique id's don't have the domains in yet.
     *
     * hacked at to satisfy the public at large still using the
     * Path line although it's not supposed to be good any longer.
     * new code checks to see if things are a common prefix. and
     * that the next character in the longer string is a "."
     * if so, it declares things good enough and uses the real author.
     */
    {
	int     idlen,
	        authlen;
	int     isok;
	int     minlen;

	idlen = strlen (note -> n_id.sys);
	authlen = strlen (note -> n_auth.asystem);
	minlen = idlen < authlen ? idlen : authlen;	/* get shortest */
	isok = (strncmp (note -> n_id.sys, note -> n_auth.asystem, minlen) == 0);
	if (isok && idlen < authlen)			/* id is short */
	{
	    isok &= (note -> n_auth.asystem[idlen] == '.');
	}
	else
	{
	    if (isok && authlen < idlen)
		isok &= (note -> n_id.sys[authlen] == '.');
	}
	if (isok)
	    author = note -> n_auth.aname;		/* ok for him */
	else
	    author = AUTHORPLACEHOLDER;			/* not local */
    }
#else	!UGLYPATH
    /* 
     * the bnews code all assumes that the last component of
     * the Path line is a user name. If we don't append something,
     * that system will see the articles again, since news will
     * disregard it when comparing to see where things have been.
     */
    author = AUTHORPLACEHOLDER;
#endif	UGLYPATH
/*
 *	dump the article
 */

    if ((rnews = popen (rnewscmd, "w")) == NULL)	/* open news pipe */
	return (-1);					/* report failure */

    fprintf (rnews, "Relay-Version: Notesfiles %s; site %s\n",
	    Version, System);
/*
 *	Here we make a slight deviation from what one would expect.
 *	We use the LOCAL version of the notes/news gateway to pick
 *	the Posting Version line. This is because we are the version
 *	that actually makes the insertion into the news system.
 *
 *	To back it up, the Usenet Standards papr (rfc whatever) says
 *	that the Posting-version "identifies the software responsible
 *	for entering this message into the network".
 *	Whether the "site" field should be the gatewaying site or where
 *	the article originated is a good question.
 *	I chose to make it the notes->news gateway running locally and
 *	the site where the article originated.
 *
 *	The stuff with the #ifdef notdef is to preserve the old code
 *	that just labeled article we gateway with a Posting version
 *	of "Notesfiles" instead of "Notesfiles 1.x".
 */
#ifdef	notdef
    if (!strcmp (System, note -> n_id.sys))		/* local note */
#endif	notdef
    {
	/* 
	 * always consider ourselves the posting version. We are the
	 * site that posted it to news!  There could be a question
	 * about articles gated in two places or which site should be
	 * there.
	 */
	fprintf (rnews, "Posting-Version: Notesfiles %s; site %s\n",
		Version, note -> n_id.sys);
    }
#ifdef	notdef
    else						/* remote note */
	fprintf (rnews, "Posting-Version: Notesfiles; site %s.%s\n",
		note -> n_id.sys, DFLTDOMAIN);		/* unknown version */
#endif	notdef
    fprintf (rnews, "From: %s\n", from);		/* formatted */
/*
 *	Sample format that is legal
 *  fprintf (rnews, "Date: 13-Jan-83 12:08 PST\n");
 */
    fprintf (rnews, "Date: %02d-%3s-%02d %02d:%02d %3s\n",
	    note -> n_date.w_day,
	    mnames[note -> n_date.w_month],
	    note -> n_date.w_year - 1900,
	    note -> n_date.w_hours,
	    note -> n_date.w_mins,
	    tzone (&note -> n_date));
    fprintf (rnews, "Newsgroups: %s\n", ngroup);
    fprintf (rnews, "Subject: %s\n", ntitle);
    fprintf (rnews, "Message-ID: <%ld@%s>\n", note -> n_id.uniqid,
	    note -> n_id.sys);
    fprintf (rnews, "Path: %s!%s\n", path, author);

/*
 *	send out the notesfile specfic headers
 */
    if (note -> n_addr.addr == 0)			/* make sure */
	note -> n_addr.textlen = 0;			/* on empty text */

    fprintf (rnews, "%s: #N:%s:%ld:%03o:%ld\n",		/* nf header */
	    NFLINE1,					/* "NF-line1" */
	    note -> n_id.sys, note -> n_id.uniqid,
	    note -> n_stat, ((long) note -> n_addr.textlen));

    fprintf (rnews, "%s: %s!%s    %3s %2d %02d:%02d:00 %4d\n",
	    NFLINE2,					/* "Nf-line2" */
	    note -> n_auth.asystem, note -> n_auth.aname,/* author */
	    mnames[note -> n_date.w_month],		/* date written */
	    note -> n_date.w_day,
	    note -> n_date.w_hours,
	    note -> n_date.w_mins,
	    note -> n_date.w_year);
/*
 *	Optional headers that we don't hassle with right now:
 *	   Organization:	make a table driven routine to grab
 */

    putc ('\n', rnews);					/* separator */
    if (backwards)					/* include old stuff */
    {
	fprintf (rnews, "#N:%s:%ld:%03o:%ld\n",		/* nf header */
		note -> n_id.sys, note -> n_id.uniqid,
		note -> n_stat, ((long) note -> n_addr.textlen));

	fprintf (rnews, "%s!%s    %3s %2d %02d:%02d:00 %4d\n",
		note -> n_auth.asystem, note -> n_auth.aname,/* author */
		mnames[note -> n_date.w_month],		/* date written */
		note -> n_date.w_day,
		note -> n_date.w_hours,
		note -> n_date.w_mins,
		note -> n_date.w_year);
    }
    putc ('\n', rnews);					/* separator */
    pageout (io, &note -> n_addr, rnews);		/* dump text */
    fprintf (rnews, "\n");				/* ensure newline */
    pclose (rnews);					/* close it */
    sleep (SLEEPTIME);					/* wait a while */
    return (0);
}

/*
 *	newsresp
 *
 *	Dump a response to the news system.
 */

newsresp (io, note, notenum, rsprec, roffset, respnum, ngroup, backwards)
struct io_f *io;
struct note_f  *note;
struct resp_f  *rsprec;
char   *ngroup;
{
    char    cmdline[CMDLEN];				/* leggo brand */
    char    line[TITLEN + 50];				/* scratch */
    FILE * rnews;
    int     i;
    char   *p,
           *q;
    char    ntitle[TITLEN + 20];			/* formatted title */
    char    from[SYSSZ + NAMESZ + 3];			/* formatted author */
    char    path[BUFSIZ];				/* path to author */
    char   *author;					/* for Path: */
    long    uniquenum;

/*
 *	pre-format a few fields like titles, author information,
 *	paths to authors, and that sort of gunk.
 */

    ntitle[0] = '\0';					/* empty string */
    if (strncmp (note -> ntitle, "Re:", 3) &&		/* is it already */
	    strncmp (note -> ntitle, "RE:", 3) &&	/* a response-like */
	    strncmp (note -> ntitle, "re:", 3))		/* title? */
    {							/* flag it as a */
	strcat (ntitle, "Re: ");			/* response */
    }
    strcat (ntitle, note -> ntitle);			/* include title */
    if (backwards)
    {
	strcat (ntitle, " ");
	strcat (ntitle, NFSUFFIX);			/* include old */
    }

    /* 
     * From:
     *	author information
     */
    sprintf (from, "%s@%s",				/* author */
	    rsprec -> r_auth[roffset].aname, rsprec -> r_auth[roffset].asystem);
#ifdef	notdef
    /* 
     * decided it was better to NOT append a domain at this point
     */
    if (index (rsprec -> r_auth[roffset].asystem, '.') != (char *) NULL)
    {							/* already domained */
	sprintf (from, "%s@%s",
		rsprec -> r_auth[roffset].aname, rsprec -> r_auth[roffset].asystem);
    }
    else
    {
	sprintf (from, "%s@%s.%s",
		rsprec -> r_auth[roffset].aname, rsprec -> r_auth[roffset].asystem,
		DFLTDOMAIN);
    }
#endif	notdef
    /* 
     * Path:
     *	Where the article has been
     */
    if (!strcmp (System, rsprec -> r_id[roffset].sys))	/* local */
    {
	sprintf (path, "%s", rsprec -> r_id[roffset].sys);
    }
    else
    {
	if (!strcmp (rsprec -> r_id[roffset].sys, rsprec -> r_from[roffset]))
	{						/* one hop */
	    sprintf (path, "%s!%s",
		    System,
		    rsprec -> r_id[roffset].sys);
	}
	else
	{
	    sprintf (path, "%s!%s!%s",			/* several hops */
		    System,
		    rsprec -> r_from[roffset],
		    rsprec -> r_id[roffset].sys);
	}
    }
#ifdef	UGLYPATH					/* usually so */
    /* 
     * See if we can use the author's name instead of
     * AUTHORPLACEHOLDER.  Check this by comparin unique id's
     * and the author's system.
     *
     * see similar code above in newsnote() for explanation of the
     * gyrations here.
     */
    {
	int     idlen,
	        authlen;
	int     isok;
	int     minlen;

	idlen = strlen (rsprec -> r_id[roffset].sys);
	authlen = strlen (rsprec -> r_auth[roffset].asystem);
	minlen = idlen < authlen ? idlen : authlen;
	isok = (strncmp (rsprec -> r_id[roffset].sys, rsprec -> r_auth[roffset].asystem, minlen) == 0);
	if (isok && idlen < authlen)			/* id is short */
	{
	    isok &= (rsprec -> r_auth[roffset].asystem[idlen] == '.');
	}
	else
	{
	    if (isok && authlen < idlen)		/* author syste is short */
		isok &= (rsprec -> r_id[roffset].sys[authlen] == '.');
	}
	if (isok)
	    author = rsprec -> r_auth[roffset].aname;
	else
	    author = AUTHORPLACEHOLDER;
    }
#else	!UGLYPATH
    /* 
     * since bnews programs all assume the last component of the
     * path line is a user name, we have to put something there
     * to keep them from disregarding the system name when trying
     * to see where the article has been.
     */
    author = AUTHORPLACEHOLDER;
#endif	UGLYPATH

/*
 *	Time to send the article to the news system
 */

    if ((rnews = popen (rnewscmd, "w")) == NULL)	/* will it work */
	return (-1);					/* report failure */

    fprintf (rnews, "Relay-Version: Notesfiles %s; site %s\n",
	    Version, System);
#ifdef	notdef
    if (!strcmp (System, rsprec -> r_id[roffset].sys))	/* local note */
#endif	notdef
    {
	/* 
	 * always consider ourselves the posting version. We are the
	 * site that posted it to news!  There could be a question
	 * about articles gated in two places or which site should be
	 * there.
	 *
	 * See comments around similar lines within the newsnote()
	 * routine above
	 */
	fprintf (rnews, "Posting-Version: Notesfiles %s; site %s\n",
		Version, rsprec -> r_id[roffset].sys);
    }
#ifdef	notdef
    else						/* remote note */
	fprintf (rnews, "Posting-Version: Notesfiles; site %s.%s\n",
		rsprec -> r_id[roffset].sys, DFLTDOMAIN);/* unknown version */
#endif	notdef
    fprintf (rnews, "From: %s\n", from);		/* formatted */
/*
 *	Sample format that is legal
 *  fprintf (rnews, "Date: 13-Jan-83 12:08 PST\n");
 */
    fprintf (rnews, "Date: %02d-%3s-%02d %02d:%02d %3s\n",
	    rsprec -> r_when[roffset].w_day,
	    mnames[rsprec -> r_when[roffset].w_month],
	    rsprec -> r_when[roffset].w_year - 1900,
	    rsprec -> r_when[roffset].w_hours,
	    rsprec -> r_when[roffset].w_mins,
	    tzone (&rsprec -> r_when[roffset]));
    fprintf (rnews, "Newsgroups: %s\n", ngroup);
    fprintf (rnews, "Subject: %s\n", ntitle);
    fprintf (rnews, "Message-ID: <%ld@%s>\n",
	    rsprec -> r_id[roffset].uniqid, rsprec -> r_id[roffset].sys);
    fprintf (rnews, "Path: %s!%s\n", path, author);

/*
 *	send out the notesfile specfic headers
 */
    fprintf (rnews, "%s: #R:%s:%ld:%s:%ld:%03o:%ld\n",
	    NFLINE1,					/* Nf-ID */
	    note -> n_id.sys, note -> n_id.uniqid,
	    rsprec -> r_id[roffset].sys,
	    rsprec -> r_id[roffset].uniqid,
	    rsprec -> r_stat[roffset],
	    ((long) rsprec -> r_addr[roffset].textlen));/* force it long */


    fprintf (rnews, "%s: %s!%s    %3s %2d %02d:%02d:00 %4d\n",
	    NFLINE2,					/* NF-From */
	    rsprec -> r_auth[roffset].asystem, rsprec -> r_auth[roffset].aname,
	    mnames[rsprec -> r_when[roffset].w_month],	/* date written */
	    rsprec -> r_when[roffset].w_day,
	    rsprec -> r_when[roffset].w_hours,
	    rsprec -> r_when[roffset].w_mins,
	    rsprec -> r_when[roffset].w_year);
    if ((uniquenum = note -> n_id.uniqid) < 0)		/* re-invert news */
	uniquenum /= (-100);				/* back to normal */
    fprintf (rnews, "References: <%ld@%s>\n",
	    uniquenum, note -> n_id.sys);

    putc ('\n', rnews);					/* separator */
    if (backwards)					/* old stuff included */
    {
	fprintf (rnews, "#R:%s:%ld:%s:%ld:%03o:%ld\n",
		note -> n_id.sys, note -> n_id.uniqid,
		rsprec -> r_id[roffset].sys,
		rsprec -> r_id[roffset].uniqid,
		rsprec -> r_stat[roffset],
		((long) rsprec -> r_addr[roffset].textlen));/* force it long */

	fprintf (rnews, "%s!%s    %3s %2d %02d:%02d:00 %4d\n",
		rsprec -> r_auth[roffset].asystem, rsprec -> r_auth[roffset].aname,
		mnames[rsprec -> r_when[roffset].w_month],/* date written */
		rsprec -> r_when[roffset].w_day,
		rsprec -> r_when[roffset].w_hours,
		rsprec -> r_when[roffset].w_mins,
		rsprec -> r_when[roffset].w_year);
    }
    putc ('\n', rnews);					/* separator */
    pageout (io, &rsprec -> r_addr[roffset], rnews);	/* dump text */
    fprintf (rnews, "\n");				/* ensure a newline */
    pclose (rnews);					/* close it */
    sleep (SLEEPTIME);					/* and wait */
    return (0);
}

/*
 *	A quick and cheap way to calculate a timezone.
 *
 *	This routine makes the assumption that a notesfile site
 *	we gateway for is in the same timezone as our site.
 *
 */

char   *tzone (when)
struct when_f   when;					/* unused */
{
#ifdef	BSD42
#include		<sys/time.h>			/* it moved! */
#else
#include		<time.h>
#endif	BSD42
#include	<sys/types.h>				/* for ftime */

    struct tm  *ltime;
    extern struct tm   *localtime ();
    long    timenow;

#ifdef	USG
    struct tm  *bp;
    extern char *tzname[];

    time (&timenow);
    ltime = localtime (&timenow);
    return (tzname[ltime -> tm_isdst]);
#else
/*
 *	for systems that still have the "timezone" function from V7
 */
#include	<sys/timeb.h>				/* for ftime */
    struct timeb    rawtime;
    extern char *timezone ();				/* please lint */

    ftime (&rawtime);					/* get information */
    time (&timenow);					/* get now */
    ltime = localtime (&timenow);			/* see if DST */
    return (timezone ((int) rawtime.timezone, ltime -> tm_isdst));
#endif
}
