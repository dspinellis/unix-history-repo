#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: loadem.c,v 1.7.0.1 85/04/11 09:23:51 notes Rel $";
#endif	RCSIDENT

/*
 *	load a file of generic notes.
 *	This routine will read the file supplied ( as an fid )   
 *	and place it into the notefile.
 *	locking is supressed if the lockit flag is false
 *	uids are mapped to zero if the system that the note came from
 *	does not match the local system.
 *
 *	Original coding:	Ray Essick	December 1981
 *	Recoding:		Ray Essick	March 1984
 *				Streamlining, prepare for alternate protocols
 */

/*
 *	routines for loading notes and responses
 */
extern int  loadnote ();				/* proto 0 note */
extern int  loadresp ();				/* proto 0 resp */
extern int  ldnote1 ();					/* proto 1 note */
extern int  ldresp1 ();					/* proto 1 resp */

/*
 *	jump tables pointing to the proper routines to parse
 *	notes and responses in the assorted protocols
 */
static int  (*noteproto[]) () =				/* notes */
{
                loadnote
};
static int  (*respproto[]) () =				/* responses */
{
                loadresp
};
static int  nproto = sizeof noteproto / sizeof noteproto[0];
static int  rproto = sizeof respproto / sizeof respproto[0];


loadem (io, infile, lockit, whofrom, extensive, articles)
struct io_f *io;
FILE * infile;
char   *whofrom;
{
    char    oneline[BUFSIZ];				/* hold a line */
    char    articletype[32];				/* proto switching */
    int     proto;					/* dynamic protocol */
    int     loaded;					/* article count */

    loaded = 0;
    while (loaded != articles &&			/* if want more */
	    fgets (oneline, sizeof (oneline) - 1, infile) != (char *) NULL)
    {
	switch (oneline[0])				/* what is it */
	{
	    case 'N': 					/* proto 1 note */
		loadnote (io, oneline, whofrom, extensive, lockit, infile);
		loaded++;				/* count it */
		break;

	    case 'R': 					/* proto 1 response */
		loadresp (io, oneline, whofrom, extensive, lockit, infile);
		loaded++;				/* count */
		break;

/*
 *	Code to catch later protocols.  This is currently unimplemented
 */
	    case 'P': 					/* newer protocol */
		sscanf (oneline, "Protocol: %d %s", &proto, articletype);
		if (proto == 0)				/* old protocol */
		    break;				/* to while() loop */
		if (!strcmp (articletype, "Note"))
		{
		    if (proto < nproto)			/* exists */
			(*noteproto[proto]) (io, whofrom, extensive, lockit, infile);
		    else
			printf ("Unsupported note protocol %d\n", proto);
		}
		else					/* assume response */
		{
		    if (proto < rproto)			/* exists */
			(*respproto[proto]) (io, whofrom, extensive, lockit, infile);
		    else
			printf ("Unsupported response protocol %d\n", proto);
		}

	    default: 					/* bong it */
		x (1, "loadem: Bad generic format");
		break;
	}
    }
}

/*
 *	loadnote(line,infile)
 *
 *	Load a note in protocol 1. line points to the first line
 *	of the article's header.
 */

loadnote (io, firstline, whofrom, extensive, lockit, infile)
struct io_f *io;
char   *firstline;
char   *whofrom;
FILE * infile;
{
    char    oneline[BUFSIZ];				/* hold lines */
    long    count;
    long    count2;
    int     i;
    char    title[TITLEN];
    struct note_f   note,
                    note2;
    struct id_f noteid;
    struct daddr_f  where;
    struct auth_f   auth;
    int     posit;
    int     status;
    int     fields;					/* scanf retcodes */
    char    field1[101],				/* scanf temps */
            field2[100];

/*
 *	grab unique id (sys,integer). Ignore "number of responses"
 */
    sscanf (firstline, "N:%99[^:]:%ld", field1, &noteid.uniqid);
    strncpy (noteid.sys, field1, SYSSZ);
    noteid.sys[SYSSZ - 1] = '\0';			/* terminate */

    fgets (oneline, sizeof (oneline) - 1, infile);	/* title */
    for (i = 0; (i < TITLEN) && oneline[i] && (oneline[i] != '\n'); i++)
	title[i] = oneline[i];
    if (i < TITLEN)
	title[i] = '\0';
    else
	title[TITLEN - 1] = '\0';			/* make sure it stops */
    for (i = strlen (title) - 1; i >= 0 && title[i] == ' '; i--)
	title[i] = '\0';				/* chop trailing spaces */

    fgets (oneline, sizeof (oneline) - 1, infile);	/* author */
    fields = sscanf (oneline, "%99[^:]:%d:%99[^:]:",
	    field1, &auth.aid, field2);
    strncpy (auth.aname, field1, NAMESZ);
    auth.aname[NAMESZ - 1] = '\0';			/* shift name */
    if (fields < 3)					/* no home system -- */
    {
	strcpy (auth.asystem, noteid.sys);		/* use unique id */
    }
    else
    {
	strncpy (auth.asystem, field2, HOMESYSSZ);
	auth.asystem[HOMESYSSZ - 1] = '\0';		/* terminate */
    }
    auth.aid &= UIDMASK;
    if (strcmp (System, noteid.sys) != 0)		/* map non-local to */
	auth.aid = Anonuid;				/* local anonymous */

    fgets (oneline, sizeof (oneline) - 1, infile);	/* time written */
    timein (oneline, &note.n_date);			/* fills in if bad */

    if (extensive)					/* if reloading */
    {
	fgets (oneline, sizeof (oneline) - 1, infile);	/* received */
	timein (oneline, &note.n_rcvd);
	fgets (oneline, sizeof (oneline) - 1, infile);	/* last modified */
	timein (oneline, &note.n_lmod);
	fgets (oneline, sizeof (oneline) - 1, infile);	/* from */
	sscanf (oneline, "%s", field1);
	strncpy (note.n_from, field1, SYSSZ);
	note.n_from[SYSSZ - 1] = '\0';			/* make sure */
    }
    else
    {
	strcpy (note.n_from, whofrom);			/* who gave it to us */
    }

    do
    {
	fgets (oneline, sizeof (oneline) - 1, infile);	/* status */
	/* 
	 * old code forgot that fgets keeps the newline and didn't
	 * remove it before a strcpy. result is that we have a bunch
	 * of notesfiles with "\n" in the n_from field. This
	 * lets a nfdump/nfload cycle clear them up for us.
	 */
    } while (oneline[0] == '\n');			/* fix old bug */
    sscanf (oneline, "%o:%ld", &status, &count);


    if (extensive == 0)					/* if not reloading */
    {
	getperms (io, 1, noteid.sys);			/* check permission */
	if (!allow (io, WRITOK))			/* not allowed */
	{
	    io -> nnotdrop++;				/* drop it */
	    for (count2 = 0; count2 < count; count2++)	/* drop message */
		getc (infile);				/* ignore the character */
	    return;					/* back for another */
	}
    }

    if (lockit)
	locknf (io, DSCRLOCK);				/* lock us up now */
    posit = chknote (io, &noteid, &note2);		/* see if here */
    if (posit == 0)					/* not in data base */
    {
	for (i = 0; i < SYSSZ; i++)
	    note.n_id.sys[i] = noteid.sys[i];
	note.n_id.uniqid = noteid.uniqid;		/* copy unique id in */
	puttrec (io, infile, &where, count);		/* suck text */
	putnote (io, &where, title, status, &note, &auth, NOPOLICY,
		NOLOCKIT, NOADDID, note.n_from, (extensive == NODETAIL));
	io -> nnotrcvd++;				/* count as a recieved */
    }
    else
    {
/*
 *	A copy exists. See if the one here is an orphan and possibly
 *	replace it
 */
	if ((note2.n_stat & ORPHND) && NOT (status & ORPHND))
	{						/* extant is orphan */
							/* new one isn't */
	    puttrec (io, infile, &note2.n_addr, count);	/* suck text */
	    gettime (&note2.n_rcvd);
	    gettime (&note2.n_lmod);			/* time stamp it */
	    copyauth (&auth, &note2.n_auth);		/* load author */
	    note2.n_stat = status;			/* correct status */
	    strncpy (note2.ntitle, title, TITLEN);
	    copydate (&note.n_date, &note2.n_date);
	    strmove (note.n_from, note2.n_from);
	    putnrec (io, posit, &note2);		/* and replace */
	    io -> adopted++;				/* orphan adopted */
	    printf ("Foster Parent Replaced. Id=%ld@%s\n",
		    noteid.uniqid, noteid.sys);
	}
	else
	{
	    for (count2 = 0; count2 < count; count2++)
		getc (infile);				/* skip text */
	    printf ("Duplicate note recieved id=%ld@%s\n",
		    noteid.uniqid, noteid.sys);
	    io -> nnotdrop++;				/* count a dropped */
	}
    }

    if (lockit)
	unlocknf (io, DSCRLOCK);			/* release lock */
}

/*
 *	loadresp(firstline,infile)
 *
 *	load a protocol 1 response
 */

loadresp (io, firstline, whofrom, extensive, lockit, infile)
struct io_f *io;
char   *firstline;
char   *whofrom;
FILE * infile;
{
    char    oneline[BUFSIZ];
    long    count;
    long    count2;
    struct note_f   note;
    struct id_f noteid,
                respid;
    struct auth_f   auth;
    struct daddr_f  where;
    struct when_f   ztime,
                    ztime2;
    char    zfrom[BUFSIZ];				/* usually <10 */
    int     status;
    int     fosterstat;
    int     posit;
    int     fields;					/* scanf return codes */
    char    field1[100],				/* scanf temps */
            field2[100];
    int     i;
    char   *p;

/*
 *	parse the parent id and the response id.
 */
    sscanf (firstline, "R:%99[^:]:%ld:%99[^:]:%ld",
	    field1, &noteid.uniqid,
	    field2, &respid.uniqid);
    strncpy (noteid.sys, field1, SYSSZ);
    strncpy (respid.sys, field2, SYSSZ);
    noteid.sys[SYSSZ - 1] = respid.sys[SYSSZ - 1] = '\0';

    fgets (oneline, sizeof (oneline) - 1, infile);	/* author */
    fields = sscanf (oneline, "%99[^:]:%d:%99[^:]:",
	    field1, &auth.aid, field2);
    strncpy (auth.aname, field1, NAMESZ);		/* shift and */
    auth.aname[NAMESZ - 1] = '\0';			/* terminate */
    if (fields < 3)					/* no home system */
    {
	strcpy (auth.asystem, respid.sys);		/* use unique id */
    }
    else
    {
	strncpy (auth.asystem, field2, HOMESYSSZ);
	auth.asystem[HOMESYSSZ - 1] = '\0';
    }
    auth.aid &= UIDMASK;				/* mask appropriately */
    if (strcmp (System, note.n_id.sys) != 0)		/* map non-local to */
	auth.aid = Anonuid;				/* local anonymous */

    fgets (oneline, sizeof (oneline) - 1, infile);	/* date written */
    timein (oneline, &ztime);

    if (extensive)					/* if reloading */
    {
	fgets (oneline, sizeof (oneline) - 1, infile);	/* date received */
	timein (oneline, &ztime2);
	fgets (oneline, sizeof (oneline) - 1, infile);	/* received from */
	sscanf (oneline, "%s", field1);
	strncpy (zfrom, field1, SYSSZ);
	zfrom[SYSSZ - 1] = '\0';			/* make sure */
    }
    else
    {
	strcpy (zfrom, whofrom);			/* who gave it to us */
    }

    do
    {
	fgets (oneline, sizeof (oneline) - 1, infile);	/* status */
	/* 
	 * old code forgot that fgets keeps the newline and didn't
	 * remove it before a strcpy. result is that we have a bunch
	 * of notesfiles with "\n" in the n_from field. This
	 * lets a nfdump/nfload cycle clear them up for us.
	 */
    } while (oneline[0] == '\n');			/* fix old bug */
    sscanf (oneline, "%o:%ld", &status, &count);


    if (lockit)
	locknf (io, DSCRLOCK);				/* CRITICAL SECTION */
    posit = chknote (io, &noteid, &note);		/* look for daddy */
    if (posit == 0)					/* no daddy */
    {							/* build foster parent */
	strcpy (note.n_id.sys, noteid.sys);
	note.n_id.uniqid = noteid.uniqid;
	note.n_nresp = 0;
	note.n_auth.aid = Anonuid;
	strcpy (note.n_auth.aname, "Unknown");
	strcpy (note.n_auth.asystem, note.n_id.sys);	/* use unique id */
	copydate (&ztime, &note.n_date);
	fosterstat = ORPHND;				/* mark as foster */
	strcpy (note.ntitle, "Orphaned Response");
	where.addr = 0;					/* no text */
	where.textlen = 0;
	posit = putnote (io, &where, note.ntitle, fosterstat, &note,
		&note.n_auth, NOPOLICY, NOLOCKIT, NOADDID, whofrom, ADDTIME);
	io -> norphans++;				/* count orphans */
	printf ("Response Id=%ld@%s is an orphan of note Id=%ld@%s\n",
		respid.uniqid, respid.sys,
		noteid.uniqid, noteid.sys);
    }
/*
 *	we definitely have a parent here, since we either found one
 *	or created one
 */
    if (chkresp (io, &respid, &note, posit) == 0)	/* response here */
    {
	if (extensive == 0)				/* if not reloading */
	    getperms (io, 1, respid.sys);		/* can he? */
	if (allow (io, RESPOK) || extensive)
	{
	    puttrec (io, infile, &where, count);	/* read text */
	    putresp (io, &where, status, posit, &ztime, &auth,
		    &note, 0, &respid, 0, zfrom, (extensive == NODETAIL), &ztime2);
	    io -> nrsprcvd++;				/* he is a rcvd ! */
	}
	else
	{						/* no permission */
	    io -> nrspdrop++;				/* dropped */
	    for (count2 = 0; count2 < count; count2++)
		getc (infile);				/* skip text */
	}
    }
    else
    {							/* copy already here */
	io -> nrspdrop++;				/* on the floor */
	for (count2 = 0; count2 < count; count2++)
	    getc (infile);				/* skip text */
	printf ("Duplicate response id=%ld@%s to note id=%ld@%s\n",
		respid.uniqid, respid.sys, noteid.uniqid, noteid.sys);
    }
    if (lockit)
	unlocknf (io, DSCRLOCK);			/* no longer critical */
}
