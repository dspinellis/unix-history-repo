#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: dump.c,v 1.7.0.1 85/10/09 18:11:12 notes Rel $";
#endif	RCSIDENT

/*
 *	dumpnf(nfname)
 *
 *	Dump a notesfile. Parameters are a notesfile name, a name
 *	for the descriptor information, and a name for the rest
 *	of the notesfile.
 *
 */

dumpnf (nfname, dfile)
char   *nfname;						/* name of notesfile */
char   *dfile;						/* descriptor */
{
    struct io_f io;
    struct note_f   note;
    FILE * dout;
    int     i;

    if ((i = init (&io, nfname)) < 0)
    {
	fprintf (stderr, "Problems opening notesfile %s\n", nfname);
	return (i);					/* die */
    }
    if (!allow (&io, READOK))				/* no read access! */
    {
	fprintf (stderr, "You aren't allowed to read %s\n", nfname);
	closenf (&io);
	return (-1);
    }
    if (!strcmp (dfile, "-"))				/* use stdout */
	dout = stdout;
    else
	if ((dout = fopen (dfile, "w")) == NULL)	/* a real file */
	{
	    closenf (&io);				/* die */
	    return (-1);
	}

    dmpdescr (&io, dout);				/* dump descriptor */
    dmpaccess (&io, dout);				/* access list out */
    for (i = 1; i <= io.descr.d_nnote; i++)		/* dump each note */
    {
	getnrec (&io, i, &note);			/* get record */
	if (note.n_stat & DELETED)
	    continue;					/* ignore */
	dmpnote (&io, &note, i, dout, DETAIL, 0);	/* base note */
	dmprall (&io, &note, i, dout, DETAIL, 0);	/* and responses */
    }

    fclose (dout);					/* close descriptor */
    closenf (&io);					/* and notesfile */
    return (0);						/* return nicely */
}


/*
 *	dmpdescr
 *
 *	Dump a notesfile descriptor and it's policy note if there is
 *	one.  Send it to the supplied stdio file.
 *
 */

dmpdescr (io, dmpfile)
struct io_f *io;
FILE * dmpfile;
{
    char    buf[128];					/* hold strings */
    register int    i;

    getdscr (io, &io -> descr);				/* grab descriptor */
    fprintf (dmpfile, "NF-Title: %s\n", io -> descr.d_title);
    fprintf (dmpfile, "NF-Director-Message: %s\n", io -> descr.d_drmes);
    sprdate (&io -> descr.d_lastm, buf);
    fprintf (dmpfile, "NF-Last-Modified: %s\n", buf);
    fprintf (dmpfile, "NF-Status:");			/* status */
    {
	if (io -> descr.d_stat & ANONOK)
	    fprintf (dmpfile, " Anonymous");
	if (io -> descr.d_stat & OPEN)
	    fprintf (dmpfile, " Open");
	if (io -> descr.d_stat & NETWRKD)
	    fprintf (dmpfile, " Networked");
	if (io -> descr.d_stat & ISARCH)
	    fprintf (dmpfile, " Archive");
    }
    putc ('\n', dmpfile);				/* end status line */
    fprintf (dmpfile, "NF-Id-Sequence: %ld@%s\n",
	    io -> descr.d_id.uniqid, io -> descr.d_id.sys);
    fprintf (dmpfile, "NF-Number: %ld\n", (long) io -> descr.d_nfnum);
    sprdate (&io -> descr.d_lstxmit, buf);
    fprintf (dmpfile, "NF-Last-Transmit: %s\n", buf);
    sprdate (&io -> descr.d_created, buf);
    fprintf (dmpfile, "NF-Created: %s\n", buf);
    sprdate (&io -> descr.d_lastuse, buf);
    fprintf (dmpfile, "NF-Last-Used: %s\n", buf);
    fprintf (dmpfile, "NF-Days-Used: %ld\n", io -> descr.d_daysused);
    fprintf (dmpfile, "NF-Notes-Written: %ld\n", io -> descr.d_notwrit);
    fprintf (dmpfile, "NF-Notes-Read: %ld\n", io -> descr.d_notread);
    fprintf (dmpfile, "NF-Notes-Transmitted: %ld\n", io -> descr.d_notxmit);
    fprintf (dmpfile, "NF-Notes-Received: %ld\n", io -> descr.d_notrcvd);
    fprintf (dmpfile, "NF-Notes-Dropped: %ld\n", io -> descr.d_notdrop);
    fprintf (dmpfile, "NF-Responses-Written: %ld\n", io -> descr.d_rspwrit);
    fprintf (dmpfile, "NF-Responses-Read: %ld\n", io -> descr.d_rspread);
    fprintf (dmpfile, "NF-Responses-Transmitted: %ld\n",
	    io -> descr.d_rspxmit);
    fprintf (dmpfile, "NF-Responses-Received: %ld\n", io -> descr.d_rsprcvd);
    fprintf (dmpfile, "NF-Responses-Dropped: %ld\n", io -> descr.d_rspdrop);
    fprintf (dmpfile, "NF-Entries: %ld\n", io -> descr.entries);
    fprintf (dmpfile, "NF-Walltime: %ld seconds\n",
	    io -> descr.walltime);
    fprintf (dmpfile, "NF-Orphans-Received: %ld\n", io -> descr.d_orphans);
    fprintf (dmpfile, "NF-Orphans-Adopted: %ld\n", io -> descr.d_adopted);
    fprintf (dmpfile, "NF-Transmits: %ld\n", io -> descr.netwrkouts);
    fprintf (dmpfile, "NF-Receives: %ld\n", io -> descr.netwrkins);
    fprintf (dmpfile, "NF-Expiration-Age: %ld Days\n", io -> descr.d_archtime);
    switch ((int) io -> descr.d_archkeep)
    {
	case KEEPYES: 
	    strcpy (buf, "Archive");
	    break;
	case KEEPNO: 
	    strcpy (buf, "Delete");
	    break;
	default: 
	    strcpy (buf, "Default");
	    break;
    }
    fprintf (dmpfile, "NF-Expiration-Action: %s\n", buf);
    switch ((int) io -> descr.d_dmesgstat)
    {
	case DIRON: 
	    strcpy (buf, "On");
	    break;
	case DIROFF: 
	    strcpy (buf, "Off");
	    break;
	case DIRNOCARE: 
	    strcpy (buf, "Either");
	    break;
	default: 
	    strcpy (buf, "Default");
	    break;
    }
    fprintf (dmpfile, "NF-Expiration-Status: %s\n", buf);
    fprintf (dmpfile, "NF-Working-Set-Size: %ld\n", io -> descr.d_workset);
    fprintf (dmpfile, "NF-Longest-Text: %ld bytes\n", io -> descr.d_longnote);

    fprintf (dmpfile, "NF-Policy-Exists: %s\n",
	    io -> descr.d_plcy ? "Yes" : "No");
    fprintf (dmpfile, "NF-Descriptor: Finished\n");	/* mark as done */
/*
 *	dump the policy note if there is one
 */
    if (io -> descr.d_plcy)				/* if a policy note */
    {							/* dump it */
	struct note_f   note;
	getnrec (io, 0, &note);
	dmpnote (io, &note, 0, dmpfile, DETAIL, 0);	/* dump it */
    }
}

/*
 *	dmpaccess(&io)
 *
 *	dump the access list.
 *	short circuited for now.
 */

dmpaccess (io, dfile)
struct io_f *io;
FILE * dfile;
{
    struct perm_f   perms[NPERMS];			/* access rights */
    int     nperms;					/* and how many */
    char    pathname[WDLEN];
    char   *atype;
    char    mode[32];					/* at most 5 */
    int     i;
    FILE * afile;

    sprintf (pathname, "%s/%s/%s", io -> basedir, io -> nf, ACCESS);
    x ((afile = fopen (pathname, "r")) == NULL, "dmpaccess: no access list");
    x ((nperms = fread (perms, sizeof (struct perm_f), NPERMS, afile)) == 0,
	    "dmpaccess: empty access list");
    fclose (afile);					/* all done */
    for (i = 0; i < nperms; i++)
    {
	switch (perms[i].ptype)
	{
	    case PERMUSER: 
		atype = "User";
		break;
	    case PERMGROUP: 
		atype = "Group";
		break;
	    case PERMSYSTEM: 
		atype = "System";
		break;
	    default: 
		atype = "Bizarro";
		break;
	}
	strcpy (mode, "");				/* build it up */
	if (perms[i].perms & DRCTOK)			/* director */
	    strcat (mode, "d");
	if (perms[i].perms & READOK)			/* read */
	    strcat (mode, "r");
	if (perms[i].perms & WRITOK)			/* write */
	    strcat (mode, "w");
	if (perms[i].perms & RESPOK)			/* respond */
	    strcat (mode, "a");
	fprintf (dfile, "Access-Right: %s:%s=%s\n",
		atype, perms[i].name, mode);
    }
    fprintf (dfile, "NF-Access-Finished:\n");
}
