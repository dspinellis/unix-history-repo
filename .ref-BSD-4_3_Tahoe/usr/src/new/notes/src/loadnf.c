#include	"parms.h"
#include	"structs.h"
#include	"dump.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: loadnf.c,v 1.7 85/01/18 15:41:25 notes Rel $";
#endif	RCSIDENT

/*
 *	loadnf(basedir, nfname, infile)
 *
 *	creates and loads a notesfile.  Notesfile is read in from
 *	FILE *infile.  Given name "nfname" in directory "basedir".
 *
 */

loadnf (basedir, nfname, infile)
char   *basedir;
char   *nfname;
FILE * infile;
{
    struct io_f io;
    char    pathname[BUFSIZ];
    struct note_f   note;

    if (buildnf (nfname, basedir, 0, 0, 0) < 0)		/* damn */
    {
	printf ("Unable to create notesfile %s in %s\n", nfname, basedir);
	return (-1);					/* failed right away */
    }
    sprintf (pathname, "%s/%s", basedir, nfname);
    if (init (&io, pathname) < 0)			/* pisser */
    {
	printf ("Unable to open newly created notesfile %s\n", pathname);
	return (-1);
    }

/*
 *	Well, now that we have an open notesfile, let's start
 *	layering in the appropriate information from the file.
 *
 *	First thing in is the notesfile descriptor.
 */

    locknf (&io, DSCRLOCK);				/* Ensure privacy */
    getdscr (&io, &io.descr);
    loaddescr (&io.descr, infile);			/* loads it */
    putdscr (&io, &io.descr);				/* save it */
    if (io.descr.d_plcy)				/* policy there? */
    {
	loadem (&io, infile, NOLOCKIT, "Noplace", DETAIL, 1);/* load */
	getdscr (&io, &io.descr);			/* update descr */
	getnrec (&io, io.descr.d_nnote, &note);		/* grab */
	putnrec (&io, 0, &note);			/* move */
	io.descr.d_nnote--;
	putdscr (&io, &io.descr);			/* fix count */
    }

/*
 *	Now we should load the access lists. This isn't
 *	perfect since this loading mechanism won't remove
 *	the "Other" entries that are placed there by default.
 */

    loadaccess (&io, infile);				/* load it */

/*
 *	Now, load the notesfile text itself.
 *	This is the easiest part.
 */

    loadem (&io, infile, NOLOCKIT, "Noplace", DETAIL, -1);/* load articles */

/*
 *	use closenf() instead of finish() to circumvent screwing
 *	with statistics.
 */
    unlocknf (&io, DSCRLOCK);				/* all done */
    closenf (&io);
    return (0);

}

/*
 *	loaddescr(&descr_f, infile)
 *
 *	Read an ASCII representation of the notesfile descriptor
 *	and load it.
 */

loaddescr (descr, infile)
struct descr_f *descr;
FILE * infile;
{
    register int    varno;				/* variable parsed */
    register char  *field;				/* field data */
    register int    i;
    long    longval;					/* sscanf temp */
    char    line[BUFSIZ];				/* line buffer */

    while (fgets (line, sizeof line, infile) != NULL)	/* read */
    {
	if ((varno = rfcparse (line, descrnames)) == -1)/* nothing */
	    continue;					/* ignore */
	if (varno == NF_DESCRIPTOR)			/* end sentinel */
	    return (0);					/* all cool */
	field = index (line, ':');			/* find data */
	field++;					/* skip colon */
	field++;					/* and space */
	switch (varno)					/* parse line */
	{
	    case NF_TITLE: 
		for (i = 0; i < NNLEN && *field != '\n'; i++, field++)
		    descr -> d_title[i] = *field;
		if (i < NNLEN)
		    descr -> d_title[i] = '\0';		/* fill */
		else
		    descr -> d_title[NNLEN - 1] = '\0';
		break;
	    case NF_DIRECTOR_MESSAGE: 
		for (i = 0; i < DMLEN && *field != '\n'; i++, field++)
		    descr -> d_drmes[i] = *field;
		if (i < DMLEN)
		    descr -> d_drmes[i] = '\0';		/* fill */
		else
		    descr -> d_drmes[DMLEN - 1] = '\0';	/* fill */
		break;
	    case NF_LAST_MODIFIED: 
		parsetime (field, &descr -> d_lastm);	/* get it */
		break;
	    case NF_STATUS: 
		{
		    char    statname[32];		/* status token */
		    char   *p;				/* into token list */

		    p = field;
		    p--;				/* get leading space */
		    while (*p && *p != '\n')		/* end string */
		    {
			if (sscanf (p, "%s", statname) != 1)
			    break;			/* no more tokens */
			if (!strcmp (statname, "Anonymous"))
			    descr -> d_stat |= ANONOK;
			if (!strcmp (statname, "Open"))
			    descr -> d_stat |= OPEN;
			if (!strcmp (statname, "Networked"))
			    descr -> d_stat |= NETWRKD;
			if (!strcmp (statname, "Archive"))
			    descr -> d_stat |= ISARCH;
			p += strlen (statname) + 1;	/* leading space */
		    }
		}
		break;
	    case NF_ID_SEQUENCE: 
		if (index (field, '@') != (char *) NULL)/* new format */
		{
		    char    field1[100];		/* big temp */
		    sscanf (field, "%ld@%99s", &descr -> d_id.uniqid, field1);
		    field1[SYSSZ - 1] = '\0';
		    strcpy (descr -> d_id.sys, field1);
		}
		else
		{
		    char   *pp;
		    pp = rindex (field, '.');		/* find last */
		    *pp++ = '\0';
		    strcpy (descr -> d_id.sys, field);
		    sscanf (pp, "%ld", &descr -> d_id.uniqid);
		}
		break;
	    case NF_NUMBER: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_nfnum = longval;		/* assign */
		break;
	    case NF_LAST_TRANSMIT: 
		parsetime (field, &descr -> d_lstxmit);
		break;
	    case NF_CREATED: 
		parsetime (field, &descr -> d_created);
		break;
	    case NF_LAST_USED: 
		parsetime (field, &descr -> d_lastuse);
		break;
	    case NF_DAYS_USED: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_daysused = longval;	/* assign */
		break;
	    case NF_NOTES_WRITTEN: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_notwrit = longval;	/* assign */
		break;
	    case NF_NOTES_READ: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_notread = longval;	/* assign */
		break;
	    case NF_NOTES_TRANSMITTED: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_notxmit = longval;	/* assign */
		break;
	    case NF_NOTES_RECEIVED: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_notrcvd = longval;	/* assign */
		break;
	    case NF_NOTES_DROPPED: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_notdrop = longval;	/* assign */
		break;
	    case NF_RESPONSES_WRITTEN: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_rspwrit = longval;	/* assign */
		break;
	    case NF_RESPONSES_READ: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_rspread = longval;	/* assign */
		break;
	    case NF_RESPONSES_TRANSMITTED: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_rspxmit = longval;	/* assign */
		break;
	    case NF_RESPONSES_RECEIVED: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_rsprcvd = longval;	/* assign */
		break;
	    case NF_RESPONSES_DROPPED: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_rspdrop = longval;	/* assign */
		break;
	    case NF_ENTRIES: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> entries = longval;		/* assign */
		break;
	    case NF_WALLTIME: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> walltime = longval;	/* assign */
		break;
	    case NF_ORPHANS_RECEIVED: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_orphans = longval;	/* assign */
		break;
	    case NF_ORPHANS_ADOPTED: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_adopted = longval;	/* assign */
		break;
	    case NF_TRANSMITS: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> netwrkouts = longval;	/* assign */
		break;
	    case NF_RECEIVES: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> netwrkins = longval;	/* assign */
		break;
	    case NF_EXPIRATION_AGE: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_archtime = longval;	/* assign */
		break;
	    case NF_EXPIRATION_ACTION: 
		{
		    char    action[32];
		    sscanf (field, "%s", action);	/* get it */
		    if (!strcmp (action, "Archive"))
			descr -> d_archkeep = KEEPYES;
		    if (!strcmp (action, "Delete"))
			descr -> d_archkeep = KEEPNO;
		    if (!strcmp (action, "Default"))
			descr -> d_archkeep = KEEPDFLT;
		    break;
		}
	    case NF_EXPIRATION_STATUS: 
		{
		    char    action[32];
		    sscanf (field, "%s", action);	/* get it */
		    if (!strcmp (action, "On"))
			descr -> d_dmesgstat = DIRON;
		    if (!strcmp (action, "Off"))
			descr -> d_dmesgstat = DIROFF;
		    if (!strcmp (action, "Either"))
			descr -> d_dmesgstat = DIRNOCARE;
		    if (!strcmp (action, "Default"))
			descr -> d_dmesgstat = DIRDFLT;
		    break;
		}
	    case NF_WORKING_SET_SIZE: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_workset = longval;	/* assign */
		break;
	    case NF_LONGEST_TEXT: 
		if (sscanf (field, "%ld", &longval) == 1)/* ok */
		    descr -> d_longnote = longval;	/* assign */
		break;
	    case NF_POLICY_EXISTS: 
		if (!strncmp ("Yes", field, 3))		/* it's there */
		    descr -> d_plcy = TRUE;		/* it's there */
		else
		    descr -> d_plcy = FALSE;		/* not there */
		break;
	}
    }
    return (-1);					/* never reached */
}

/*
 *	loadaccess(&io, infile)
 *
 *	load the access list of the specified notesfile from
 *	the supplied file.  Stops on the keyword "Access-End".
 */

loadaccess (io, infile)
struct io_f *io;
FILE * infile;
{
    char    pline[BUFSIZ];
    struct perm_f   perms[NPERMS];			/* access rights */
    int     nmodes,
            i;
    register char  *field;

    nmodes = 0;						/* load perms[] */
    while (fgets (pline, sizeof pline, infile) != NULL)
    {
	if ((i = rfcparse (pline, &accessnames)) != ACCESS_RIGHT && i != (-1))
	    break;					/* signals the end */
	if (nmodes == NPERMS)				/* full list */
	    continue;
	i = strlen (pline);
	if (pline[i] == '\n')
	    pline[i] = '\0';				/* zap newline */
	field = index (pline, ':');			/* find data */
	field++;					/* skip colon */
	field++;					/* and space */
	if (parsemode (field, &perms[nmodes], 0) == 0)	/* worked? */
	    nmodes++;					/* bump counter */
    }
/*
 *	we break to here when we've sucked in the line 
 *	"NF-Access-Termination" or some such. Anyway, it's
 *	after we've abused the sentinel line.
 */
    addmodes (io, nmodes, &perms, 0);			/* add them */
}
