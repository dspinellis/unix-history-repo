#define	MAINLINE
#include "parms.h"
#include "structs.h"
#include "newsgate.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: newsinput.c,v 1.7.0.6 85/10/01 23:06:08 notes Rel $";
#endif	RCSIDENT

/*
 *	newsinput
 *
 *	A total re-coding of the original.  Makes use of the
 *	work that Lou Salkind and Tw Cook have done.  Lou rearranged
 *	a bunch of stuff and Tw put the notes headers into the
 *	news header.
 */

static char title[TITLEN + 1];				/* hold titles */
extern char fromsys[SYSSZ + 1];				/* gave it to us */
static struct when_f    entered;			/* date written */
extern char origsys[SYSSZ + 1];				/* originator */
extern char authname[NAMESZ + 1];			/* author */
static int  has_suffix = 0;				/* had -(nf) suffix */

#define	dprintf		if (0) printf


main (argc, argv)
int     argc;
char  **argv;
{
    FILE * rawnews;
    FILE * body;
    char    pathname[BUFSIZ];
    struct io_f io;
    struct hbuf header;
    char    nf[WDLEN];
    struct nflist_f *nfptr;				/* expand newsgroups */
    int     c;
    int     onechar;
    char   *tail;
    int     fid;					/* for close */


    setuid (geteuid ());				/* force to "notes" */
    startup (argc, argv);				/* comon init */
    rawnews = stdin;					/* usually here */
    for (fid = 3; fid < 20; fid++)			/* close all extras */
	close (fid);

/*
 *	Parse the Header.  Follow all the USENET standards
 *	for doing this.  Result is left in a fun little
 *	structure.
 *	Internalize some of the information to help us figure out
 *	some things quickly.
 */

    if (!newsheader (&header, rawnews, TRUE))		/* read the headers */
    {
	printf ("Incoming News mangled more than usual\n");
	exit (BAD);
    }

/*
 *	Parse things like origsys, fromsys, author, date written
 */
    parsepath (header.path, header.from);		/* systems, authors */
    parsetime (header.subdate, &entered);		/* submitted */
    sprintf (pathname, "/tmp/nfxx%d", getpid ());
    dprintf ("Origsys: %s\n", origsys);
    dprintf ("fromsys: %s\n", fromsys);
    dprintf ("Date Written:");
#ifdef	notdef
    prdate (&entered);
#endif
    dprintf ("\nauthor: %s\n", authname);
/*
 *	See if this might be a control message. Notes readers don't
 *	care to see these.
 *
 *	News code also recognizes titles with first 5 characters set 
 *	to "cmsg " as control messages. We should clean them up too.
 */
    if (header.ctlmsg[0] != '\0')			/* is control */
    {
	printf ("Control message (ignored): %s\n", header.ctlmsg);
	exit (0);					/* "success" */
    }
/*
 *	Save the body of the article somewhere safe (like not in
 *	memory).
 */
    if ((body = fopen (pathname, "w")) == NULL)		/* failed */
    {
	printf ("Had problems creating/opening file %s\n", pathname);
	exit (BAD);					/* die */
    }
    while ((onechar = getc (rawnews)) != EOF)		/* save it */
	putc (onechar, body);
    fflush (body);					/* make sure */
    fclose (body);					/* and close it */

/*
 *	Now run through the specified list of newsgroups,
 *	re-scan the body and such each time.
 */

    expand (header.nbuf);				/* expand groups */
    while ((nfptr = nextgroup ()) != (struct nflist_f *) NULL)
    {

	newsgroup (nfptr -> nf_name, nf, NEWSNF);	/* map it */
	dprintf ("Newsgroup %s maps to notesfile %s\n", nfptr -> nf_name, nf);
	tail = rindex (nfptr -> nf_name, '.');		/* catch ctl msgs */
	if (tail != (char *) NULL && !strcmp (tail, CTL))/* it is one */
	{
	    char    pbuf[256];				/* for title fixing */

	    strcpy (nf, NFMAINT);			/* map it */
	    dprintf ("Control newsgroup %s mapped to %s\n",
		    nfptr -> nf_name, nf);
	    sprintf (pbuf, "%s:%s", nfptr -> nf_name, header.title);
	    strncpy (header.title, pbuf, BUFLEN);	/* prefix title */
	    header.title[BUFLEN - 1] = '\0';		/* ensure terminater */
	}

	if ((body = fopen (pathname, "r")) == NULL)
	{
	    goto failed;				/* shit */
	}
	if (init (&io, nf) < 0)				/* open the nf */
	{
	    char    pbuf[512];
	    char    tbuf[128];
#ifdef AUTOCREATE
	    sprintf (pbuf,
		    "Notesfile: %s\nNewsgroup: %s\n\nCreated by newsinput\n",
		    nf, nfptr -> nf_name);
	    sprintf (tbuf, "New NF: %s", nf);
	    nfcomment (NFMAINT, pbuf, tbuf, TRUE, 0);
	    buildnf (nf, Mstdir, 0, 1, 1);		/* open and networked */
	    x (init (&io, nf) < 0, "newsinput: open newly created notesfile");
#else
	    sprintf (pbuf, "Notesfile: %s, newsgroup %s\n",
		    nf, nfptr -> nf_name);
	    sprintf (tbuf, "New newsgroup %s", nfptr -> nf_name);
	    nfcomment (NFMAINT, pbuf, tbuf, 0, 0);
	    printf ("Inserting into %s\n", NEWNEWS);
	    strcpy (nf, NEWNEWS);			/* Change newsgroup */
	    if (init (&io, nf) < 0)
		exit (BAD);				/* Give up */
	    printf ("Open of %s suceeded\n", nf);
#endif AUTOCREATE
	}

	if (nfgen (&io, &header, body, pathname) < 0)	/* not from notes */
	{
	    dprintf ("Nfgen returns failure\n");
	    fclose (body);				/* give bnewsgen */
	    body = fopen (pathname, "r");		/* a clean copy */
	    if (bnewsgen (&io, &header, body) < 0)	/* or news */
	    {
		dprintf ("bnewsgen returns failure\n");
		goto failed;				/* drop out */
	    }
	}
	fclose (body);					/* ready for loop */
	finish (&io);
    }

    unlink (pathname);
    exit (GOOD);

/*
 *	jump here on totally screwed up article.
 */
failed: 
    dprintf ("Jumped to failed\n");
    unlink (pathname);					/* body of article */
    exit (BAD);
}

/*
 *	nfgen(&io,&header,&FILE,pathname)
 *
 *	parse a notesfile-generated article.  Check the fields of
 *	header and look for # lines in the body of the article to
 *	determine if it came from notes.
 *	
 *	returns:	0 no permission for author
 *			> 0 signifies note or response where it wound up
 *			-1 if the article wasn't generated by notes
 */

nfgen (io, header, body, pathname)
struct io_f *io;
struct hbuf *header;
FILE * body;
char   *pathname;
{
    register int    i;
    register char  *p;
    struct note_f   note;
    struct note_f   note2;
    struct id_f respid;
    struct daddr_f  where;
    struct when_f   whentime;
    struct auth_f   auth;				/* author */
    int     oldstyle = 0;
    int     found;
    char    line[CMDLEN];				/* scratch */
    char   *suffix;
    int     notenum;
    int     status;
    int     fosterstat;					/* for foster parents */
    int     count;
    char    hline1[BUFLEN];				/* in-text header */
    char    hline2[BUFLEN];				/* in-text header 2 */
    int     onechar;					/* scratch character */
    char    field1[100],				/* scanf tmps */
            field2[100];

/* 
 * Check for titles ending in "- nf".
 * We always remove these.
 */
    suffix = rindex (header -> title, '-');		/* find last */
    if (!strcmp (suffix, NFSUFFIX) || !strcmp (suffix, OLDSUFFIX))
    {
	if (--suffix > header -> title)			/* if we can */
	    *suffix = '\0';				/* strip "- (nf)" */
	has_suffix++;					/* flag it */
    }
/*
 *	at this point we should check for embodied #N.... lines and
 *	remove them.  This is conditional on having a "- (nf)" in the
 *	title of the note.
 */

    strcpy (hline1, "");				/* empty these */
    strcpy (hline2, "");
    if (has_suffix)					/* look for embedded */
    {
	long    position,				/* place marker */
	        ftell ();				/* for types */

	position = ftell (body);			/* save it */
	while (fgets (hline1, sizeof hline1, body) != NULL)
	    if (hline1[0] == '#')
		break;					/* found one */
	if (hline1[0] != '#')				/* actually didn't */
	{
	    fseek (body, position, 0);			/* rewind */
	    strcpy (hline1, "");			/* empty it */
	}
	else
	{						/* grab line 2 */
	    fgets (hline2, sizeof hline2, body);
	    while ((onechar = getc (body)) != '\n' && onechar != EOF)
		;					/* zap separator line */
	}
    }

    if (strlen (header -> nline1) == 0)			/* no new headers */
    {
	/* 
	 * No notes header in the B news article header...
	 * If title ends with "- nf", look for the
	 * header in the body of the text.
	 * (for backwards compatability)
	 */
	if (has_suffix == 0)				/* not from notes */
	{
	    dprintf ("No NFSUFFIX and no header lines\n");
	    return (-1);
	}
	oldstyle = 1;
	found = 0;
	if (hline1[0] == '#')				/* got them earlier */
	{
	    strcpy (header -> nline1, hline1);		/* first line */
	    strcpy (header -> nline2, hline2);		/* second line */
	    found++;					/* and mark it */
	}
	while (!found &&				/* search body */
		fgets (header -> nline1, sizeof header -> nline1, body))
	{
	    if (header -> nline1[0] == '#')		/* bingo */
	    {
		found++;
		break;
	    }
	}
	if (!found ||
		fgets (header -> nline2, sizeof header -> nline2, body) == NULL)
	{
	    dprintf ("no header lines in text body\n");
	    return (-1);				/* not from notes */
	}
    }

/* 
 * We now have the header lines.
 * Check validity and do the appropriate action.
 */
    if (header -> nline1[0] != '#')
    {
	dprintf ("Invalid first header line\n");
	return (-1);
    }
    dprintf ("First line is: %s\n", header -> nline1);
    dprintf ("Second line is: %s\n", header -> nline2);
    strncpy (title, header -> title, TITLEN);		/* get title */
    title[TITLEN - 1] = '\0';				/* terminate for sure */

    switch (header -> nline1[1])			/* parse it */
    {
	case 'N': 					/* base note */
	    if (sscanf (header -> nline1, "#N:%99[^:]:%ld:%o:%d", field1,
			&note.n_id.uniqid, &status, &count) != 4)
	    {
		return (-1);				/* no good */
	    }
	    strncpy (note.n_id.sys, field1, SYSSZ);	/* copy */
	    note.n_id.sys[SYSSZ - 1] = '\0';		/* and terminate */
	    status |= FRMNEWS;				/* it's been there */

	    /* 
	     * parse the second header line
	     */

	    p = header -> nline2;
	    for (i = 0; (i < HOMESYSSZ - 1) && (*p != '!' && *p != '\0'); i++)
		auth.asystem[i] = *p++;			/* get the author */
	    auth.asystem[i] = '\0';			/* terminate */
	    while (*p != '!' && *p != '\0')
		p++;					/* skip to end of system */
	    if (*p == '!')
		p++;					/* skip the ! */
	    for (i = 0; (i < NAMESZ - 1) && (*p != ' ' && *p != '\0'); i++)
		auth.aname[i] = *p++;			/* get the author */
	    auth.aname[i] = '\0';			/* terminate */
	    auth.aid = Anonuid;

	    while (*p != ' ' && *p)
		p++;					/* drop rest of author */
	    while (*p == ' ')				/* find the date */
		p++;
	    parsetime (p, &note.n_date);		/* and parse it */

	    getperms (io, 1, note.n_id.sys);		/* check permissions */
	    if (allow (io, WRITOK) == 0)		/* not a chance */
		return (0);				/* sort of success */

	    locknf (io, DSCRLOCK);			/* MUTEX */
	    if ((notenum = chknote (io, &note.n_id, &note2)) == 0)
	    {						/* not in data base */
		pagein (io, body, &where);		/* grab text */
		status |= FRMNEWS;			/* through news */
		strcpy (note.n_from, fromsys);		/* who gave it to us */
		i = putnote (io, &where, title, status, &note, &auth,
			NOPOLICY, NOLOCKIT, NOADDID, fromsys, ADDTIME);
		io -> nnotrcvd++;			/* count it */
		unlocknf (io, DSCRLOCK);		/* MUTEX done */
		return (i);				/* return notenum */
	    }
	    if ((note2.n_stat & ORPHND) && (status & ORPHND) == 0)
	    {						/* replace foster */
							/* with true parent */
		pagein (io, body, &note2.n_addr);	/* the text */
		gettime (&note2.n_rcvd);		/* update timestamp */
		gettime (&note2.n_lmod);		/* time stamp it */
		copyauth (&auth, &note2.n_auth);	/* correct author */
		note2.n_stat = status | FRMNEWS;	/* and status bits */
		strncpy (note2.ntitle, title, TITLEN);
		note2.n_date = entered;
		strcpy (note2.n_from, fromsys);
		putnrec (io, notenum, &note2);		/* and replace */
		io -> adopted++;			/* count adoption */
		io -> nnotrcvd++;			/* count in */
		unlocknf (io, DSCRLOCK);
		printf ("Orphaned response chain adopted\n");
		return (notenum);			/* note number */
	    }
	    else
		printf ("Duplicate note handed back by news\n");
	    unlocknf (io, DSCRLOCK);
	    return (0);					/* mark resolved */

	case 'R': 					/* response */
	    if (sscanf (header -> nline1, "#R:%99[^:]:%ld:%99[^:]:%ld:%o:%d",
			field1, &note.n_id.uniqid, field2,
			&respid.uniqid, &status, &count) != 6)
	    {
		return (-1);				/* no good */
	    }
	    strncpy (note.n_id.sys, field1, SYSSZ);	/* copy them */
	    strncpy (respid.sys, field2, SYSSZ);	/* both and */
	    note.n_id.sys[SYSSZ - 1] = respid.sys[SYSSZ - 1] = '\0';/* stop */
	    status |= FRMNEWS;				/* it's been there */

	    getperms (io, 1, respid.sys);		/* check modes */
	    if (allow (io, RESPOK) == 0)		/* not a chance */
		return (0);				/* resolved */

	    p = header -> nline2;			/* second line */
	    for (i = 0; (i < HOMESYSSZ - 1) && (*p != '!' && *p != '\0'); i++)
		auth.asystem[i] = *p++;			/* get the author */
	    auth.asystem[i] = '\0';			/* terminate */
	    while (*p != '!' && *p != '\0')
		p++;					/* skip to end of system */
	    if (*p == '!')
		p++;					/* skip the ! */
	    for (i = 0; (i < NAMESZ - 1) && (*p != ' ' && *p != '\0'); i++)
		auth.aname[i] = *p++;			/* parse author */
	    auth.aname[i] = '\0';			/* terminate */
	    auth.aid = Anonuid;				/* default */
	    while (*p != ' ' && *p)
		p++;					/* rest of author */
	    while (*p == ' ')				/* find the date */
		p++;
	    parsetime (p, &entered);			/* and parse it */

	    locknf (io, DSCRLOCK);			/* MUTEX */
	    notenum = chknote (io, &note.n_id, &note2);
	    if (notenum == 0)				/* found parent? */
	    {						/* build foster */
		printf ("Orphaned response handed in by news\n");
		strcpy (note.n_from, fromsys);		/* make basic info */
		note.n_nresp = 0;
		note.n_auth.aid = Anonuid;
		strcpy (note.n_auth.aname, "Unknown");
		strcpy (note.n_auth.asystem, note.n_id.sys);/* system */
		note.n_date = entered;
		gettime (&whentime);			/* current time */
		fosterstat = ORPHND | FRMNEWS;		/* combo there */
#ifdef	notdef
		strcpy (note.ntitle, "(Orphan) ");	/* prefix */
#else
		strcpy (note.ntitle, "");		/* empty */
#endif
		i = strlen (note.ntitle);		/* index */
		for (p = header -> title; i < TITLEN && *p; i++, p++)/* rest of title */
		    note.ntitle[i] = *p;		/* basic title */
		if (i < TITLEN)
		    note.ntitle[i] = '\0';		/* null it */
		else
		    note.ntitle[TITLEN - 1] = '\0';	/* null */
		where.addr = 0;				/* no text */
		where.textlen = 0;			/* still no text */
		notenum = putnote (io, &where, note.ntitle, fosterstat,
			&note, &note.n_auth, NOPOLICY, NOLOCKIT, NOADDID,
			fromsys, ADDTIME);		/* insert him */
		io -> norphans++;			/* orphan census */
		getnrec (io, notenum, &note2);		/* get good one */
	    }
/*
 *	At this point we know we have a parent because if there wasn't
 *	one before, we built a foster parent.
 */
	    if (chkresp (io, &respid, &note2, notenum) == 0)
	    {						/* none, insert it */
		status |= FRMNEWS;
		pagein (io, body, &where);
		i = putresp (io, &where, status, notenum, &entered, &auth,
			&note, NOLOCKIT, &respid, NOADDID, fromsys,
			ADDTIME, &whentime);
		io -> nrsprcvd++;			/* count him in */
		unlocknf (io, DSCRLOCK);		/* UNMUTEX */
		return (i);				/* resp number */
	    }
	    else
		printf ("Duplicate response handed back by news\n");
	    unlocknf (io, DSCRLOCK);
	    return (0);					/* resolved */

	default: 					/* bad news */
	    return (-1);
    }							/* NOTREACHED */
    return (0);
}

/*
 *	bnewsgen(&io,&header,&FILE)
 *
 *	parse an article that came through B-news.  We've already
 *	checked to see if it was a notesfile generated article
 *	so all we have to do is decide if it's a note/response
 *	and put it in the appropriate place.
 */

bnewsgen (io, header, body)
struct io_f *io;
struct hbuf *header;
FILE * body;
{
    register int    i;
    char   *p;
    struct note_f   note;
    struct note_f   note2;
    struct when_f   whentime;
    struct daddr_f  where;
    int     notenum;
    int     status;
    char    pbuf[BUFLEN];				/* scratch */
    long    newsseq;
    char    newssys[SYSSZ];
    struct id_f newsid;
    struct auth_f   auth;
    char   *lead,
           *trail;					/* references */
    char    basesys[SYSSZ];				/* references */
    long    baseseq;					/* ditto */
    struct id_f baseid;					/* ditto ditto */
    char    field1[100],				/* scanf tmps */
            field2[100];

    getperms (io, 1, origsys);
    if (allow (io, WRITOK) == 0)			/* let him* */
    {
	printf ("System %s not allowed to write notes\n", origsys);
	return (0);					/* NO! */
    }

    i = sscanf (header -> ident, "<%ld@%99[^>]>", &newsseq, field1, pbuf);
    if (i < 2)						/* try old */
    {
	i = sscanf (header -> ident, "%99[^.].%ld", field1, &newsseq);
    }
    if (i < 2)						/* no id */
    {
#ifdef	NFMAINT
	char    pbuf[BUFSIZ];

	sprintf (pbuf,
		"Message-ID: %s\nPath: %s\nFrom: %s\nNewsgroups: %s\n",
		header -> ident, header -> path,
		header -> from, header -> nbuf);
	nfcomment (NFMAINT, pbuf, "Unfathomable Article ID", 0, 0);
#endif	NFMAINT
	dprintf ("can't fathom article ID: %s\n", header -> ident);
	return (-1);					/* mark bogus */
    }
    strncpy (newssys, field1, SYSSZ);			/* copy */
    newssys[SYSSZ - 1] = '\0';				/* and truncate */

    note.n_date = entered;
    strcpy (note.n_from, fromsys);
    strncpy (auth.aname, authname, NAMESZ);		/* fill in author */
    strncpy (auth.asystem, origsys, HOMESYSSZ);		/* system */
    auth.asystem[HOMESYSSZ - 1] = auth.aname[NAMESZ - 1] = '\0';
    auth.aid = Anonuid;
    status = FRMNEWS;					/* came through news */
    strncpy (title, header -> title, TITLEN);		/* move new title */
    title[TITLEN - 1] = '\0';				/* sure it stops */

    locknf (io, DSCRLOCK);				/* MUTEX */
/*
 *	first thing is to see if it's a base note somewhere.
 */
    strcpy (newsid.sys, newssys);			/* build uniq id */
    strcpy (note.n_id.sys, newssys);			/* build descriptor */
    note.n_id.uniqid = newsid.uniqid = newsseq;
    notenum = chknote (io, &note.n_id, &note2);		/* try normal */
    if (notenum == 0)					/* try -100 trick */
    {
	note.n_id.uniqid = newsid.uniqid = newsseq * -100;
	notenum = chknote (io, &note.n_id, &note2);
    }
    if (notenum != 0)
    {
	if (!(note2.n_stat & ORPHND))
	{
	    printf ("Duplicate news article received\n");
	    io -> nnotdrop++;				/* count as dropped */
	    unlocknf (io, DSCRLOCK);
	    return (0);					/* done with it */
	}
	/* 
	 *	 replace foster parent
	 */
	pagein (io, body, &note2.n_addr);		/* collect text */
	gettime (&note2.n_rcvd);			/* current tod */
	gettime (&note2.n_lmod);			/* last touched */
	copyauth (&auth, note2.n_auth);			/* fill in author */
	note2.n_stat |= FRMNEWS;			/* brand it */
	strncpy (note2.ntitle, title, TITLEN);		/* move title */
	note2.n_date = entered;
	strcpy (note2.n_from, fromsys);			/* who sent it to us */
	putnrec (io, notenum, &note2);			/* and replace */
	io -> adopted++;				/* count it */
	io -> nnotrcvd++;				/* count in */
	unlocknf (io, DSCRLOCK);
	printf ("Orphaned Response Chain adopted\n");
	return (notenum);				/* correctly placed */
    }

/*
 *	See if we can turn this into a response to some base note.
 *	First priority is to match it to any of the articles listed
 *	in a References field if there is one.
 */

    notenum = 0;					/* init to not found */
    if (header -> followid[0])				/* references */
    {
	trail = header -> followid;
	while ((lead = index (trail, '<')) && (trail = index (lead, '>')))
	{						/* delimited id */
	    i = sscanf (lead, "<%ld@%99[^>]>", &baseseq, field1, pbuf);
	    if (i < 2)					/* try old format */
		i = sscanf (lead, "%99[^.].%ld", field1, &baseseq);
	    if (i < 2)
		continue;				/* try next one */
	    strncpy (basesys, field1, SYSSZ);
	    basesys[SYSSZ - 1] = '\0';			/* and truncate */

	    strcpy (baseid.sys, basesys);		/* build goal */
	    baseid.uniqid = baseseq;			/* try notes source */
	    if ((notenum = chknote (io, &baseid, &note2)))/* WANT ASSIGN */
		break;					/* yes! */

	    baseid.uniqid = baseseq * -100;		/* try news source */
	    if ((notenum = chknote (io, &baseid, &note2)))/* WANT ASSIGN */
		break;					/* yes! */

	    notenum = 0;				/* ensure "unfound" */
	}
    }

/*
 *	If References did any good, "notenum" is positive non-zero.
 *	Otherwise it didn't help out at all and we have to resort to
 *	parsing the title for "re:" prefixes
 *	If we can find a base title, use the title search code to
 *	scan for it.
 */

    if (notenum == 0 &&					/* not found */
	    !strncmp (header -> title, "re: ", 4) ||	/* and looks like */
	    !strncmp (header -> title, "Re: ", 4) ||	/* a response */
	    !strncmp (header -> title, "RE: ", 4))
    {
	dprintf ("Looking at titles\n");
	p = header -> title;
	do
	{
	    for (p += 3; *p == ' ' || *p == '\t'; p++);	/* drop spaces */
	} while (!strncmp (p, "re: ", 4) ||
		!strncmp (p, "Re: ", 4) ||
		!strncmp (p, "RE: ", 4));
	strncpy (io -> xstring, p, TITLEN);		/* load it */
	io -> xstring[TITLEN - 1] = '\0';		/* and terminate it */
	notenum = findtitle (io, io -> descr.d_nnote, FALSE);/* start at back */
	if (notenum > 0)				/* found one */
	    getnrec (io, notenum, &note2);		/* get a ptr to it */
    }

/*
 *	OK. By now, we have a "notenum" if the article can be pegged
 *	as a response to one of our notes.
 *	Otherwise, notenum==0 and we'll have to turn it into
 *	a base note.
 */

    if (notenum > 0)
    {
	dprintf ("Looking in response chain for note %d\n", notenum);
	if (!chkresp (io, &newsid, &note2, notenum))
	{						/* no copy here */
	    pagein (io, body, &where);
	    gettime (&whentime);
	    i = putresp (io, &where, status, notenum, &entered, &auth, &note,
		    NOLOCKIT, &newsid, NOADDID, fromsys, ADDTIME, &whentime);
	    unlocknf (io, DSCRLOCK);			/* un-MUTEX */
	    return (i);
	}
	else
	{						/* copy there */
	    unlocknf (io, DSCRLOCK);			/* all done */
	    printf ("Duplicate Response handed back by news\n");
	    io -> nrspdrop++;				/* bong it */
	    return (0);					/* count as done */
	}
    }
/*
 *	If we are going to do things this way, here is the point
 *	where we should check about turning a news-generated 
 *	article into an orphaned response.
 *
 *	Basically, look for a non-empty references line and
 *	make a foster parent with the first article id on that
 *	line.
 */

/*
 *	by this point, it's obvious that we can't turn the note into
 *	a response.  We can skip the check to see if it is already
 *	there because we did that at the very top of this loop
 *	and since we've locked the notesfile up while we're doing this,
 *	we know that nobody added a note.
 */
    dprintf ("Processing article as a base note\n");
    pagein (io, body, &where);
    notenum = putnote (io, &where, title, status, &note,
	    &auth, NOPOLICY, NOLOCKIT, NOADDID, fromsys, ADDTIME);
    io -> nnotrcvd++;					/* count it */
    unlocknf (io, DSCRLOCK);
    return (notenum);
}
