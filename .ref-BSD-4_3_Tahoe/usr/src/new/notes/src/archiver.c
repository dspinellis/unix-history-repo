#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: archiver.c,v 1.7.0.4 85/06/05 14:16:24 notes Rel $";
#endif	RCSIDENT

/*
 *	archiver - archives a notesfile. Takes all articles older
 *	than 'daysold' days and places them, in generic format, in
 *	a sub-directory in the archie directory. The files are marked
 *	by the time that they were created. 
 *	The deleteonly parameter is normally zero. If it is non-zero,
 *	no archive is taken; the old notes are merely thrown away.
 *
 *	Ray Essick			March 1982
 *
 *	modified so that could also toggle on the director message.
 *	in addition to the days untouched.
 *				Ray Essick	June 1982
 *
 *	Now grabs an expiration threshold and a "working set size"
 *	from the notesfile itself.  If zero, the values passed
 *	in as paramaters are used.
 */

archiver (nfname, daysold, worksetsize, deleteonly, dirmsgflag)
char   *nfname;
int     daysold;
int     worksetsize;
int     deleteonly;
int     dirmsgflag;
{
    struct io_f io,
                archio;
    struct when_f   zaptime;				/* boundary time */
    struct note_f   note;
    struct note_f   note2;
    int     i,
            ncount,
            rcount;
    int     dnotes,					/* duplicates */
            dresps,					/* in the archive */
            adopts;					/* and adoptions */
    int     deletable;					/* how many can zap */
    int     presps;
    char    line[WDLEN];
    char    archdest[WDLEN];				/* target notesfile */
    char    archbase[WDLEN];				/* target directory */
    char    archend[WDLEN];				/* and nf name */
    char    timeline[DATELEN];
    char   *endname;
    FILE * log;
    int     wasopen;
    int     locktarget;					/* whether to */
    struct daddr_f  where;
    int     rnum;					/* copy responses */
    int     newnum;					/* note place in arch */
    int     rblock,
            roffset;
    struct resp_f   resp;
    FILE * txtfile;					/* for saving text */
    char    txtfn[WDLEN];				/* its name */
    int     dup_place;					/* is in archive? */
    int     dup_resp;					/* dup supression */

    if (init (&io, nfname) < 0)
	return (-1);					/* no notesfile */


    if (allow (&io, DRCTOK) == 0 && globuid != Notesuid)
    {
	closenf (&io);
	printf ("Archiver: %s: You don't have permission to archive\n",
		io.fullname);
	fflush (stdout);
	return (-1);
    }

    if (io.descr.d_stat & ISARCH)			/* can't archive an archive */
    {
	closenf (&io);
	printf ("Archiver: %s: You can't archive an archive\n", io.fullname);
	fflush (stdout);
	return (-1);
    }

/*
 *	select the archive name
 */

    switch (nfalias (io.fullname, archdest, ARCHALIAS))
    {
	case -1: 					/* no file */
	case 0: 					/* no match */
	    if (*nfname == '/')				/* absolute path name */
	    {
		strcpy (archend, io.nf);		/* get nf */
		strcpy (archbase, ARCHDIR);		/* base directory */
		printf ("Archiver: WARNING: possible naming conflict in %s (%s)\n",
			nfname, io.fullname);
		fflush (stdout);
	    }
	    else
	    {
		strcpy (archend, io.nf);
		strcpy (archbase, ARCHDIR);		/* base directory */
	    }
	    break;

	case 1: 					/* an alias! */
	    if (archdest[0] != '/')			/* expand it */
	    {
		strcpy (archbase, ARCHDIR);
		strcpy (archend, archdest);		/* hold it */
	    }
	    else
	    {
		endname = rindex (archdest, '/');
		*endname++ = '\0';			/* split */
		strcpy (archbase, archdest);		/* directory */
		strcpy (archend, endname);		/* and nf */
	    }
	    break;

    }

    sprintf (archdest, "%s/%s", archbase, archend);	/* full name */
    sprintf (txtfn, "/tmp/nfa%d", getpid ());		/* hold texts */
    ncount = rcount = 0;				/* count archived */
    dnotes = dresps = adopts = 0;			/* duplicates */
    locktarget = 0;					/* changed if should */

/*
 *	check notesfile specific thresholds, sizes and other options
 */

    if (io.descr.d_archtime == NEVER)			/* don't archive */
    {
	printf ("Archiver: %s has archive threshold of `never'\n",
		nfname);
	fflush (stdout);
	goto docompress;				/* compress anyway */
    }

    if (io.descr.d_archtime != 0)			/* non-default */
    {
	daysold = (int) io.descr.d_archtime;		/* use this one */
	printf ("Archiver: %s specifies threshold of %d days\n",
		nfname, daysold);
	fflush (stdout);
    }

    if (io.descr.d_workset != 0)
    {
	worksetsize = (int) io.descr.d_workset;
	printf ("Archiver: %s specifies working set size of %d\n",
		nfname, worksetsize);
	fflush (stdout);
    }

    if (io.descr.d_dmesgstat != DIRDFLT)		/* specific */
    {
	dirmsgflag = (int) io.descr.d_dmesgstat;	/* set it */
	printf ("Archiver: %s specifies dirmsg status of %s for expiring\n",
		nfname,
		dirmsgflag == DIRON ? "ON" :
		dirmsgflag == DIROFF ? "OFF" :
		"NOCARE");
	fflush (stdout);
    }

    if (io.descr.d_archkeep != KEEPDFLT)		/* keep/delete */
    {
	if (io.descr.d_archkeep == KEEPYES)
	    deleteonly = 0;
	else
	    deleteonly = 1;
	printf ("Archiver: %s specifies %s expired notes\n",
		nfname,
		deleteonly ? "deleting" : "archiving");
	fflush (stdout);
    }

    deletable = ((int) io.descr.d_nnote) - ((int) io.descr.d_delnote) - worksetsize;
    if (deletable <= 0)					/* candidates? */
    {
	if (io.descr.d_nnote - io.descr.d_delnote > 0)	/* only if non-empty */
	{
	    printf ("Archiver: %s: %d notes <= working set size of %d\n",
		    nfname,
		    io.descr.d_nnote - io.descr.d_delnote,
		    worksetsize);
	    fflush (stdout);
	}
	goto docompress;
    }

    gettime (&zaptime);					/* threshold */
    zaptime.w_gmttime -= 60L * 60L * 24L * ((long) daysold);/* internal */
    maketime (&zaptime, zaptime.w_gmttime);		/* re-format */


    if (!deleteonly)
    {
	if (init (&archio, archdest) < 0)		/* not already */
	{
	    printf ("Archiver creating archive notesfile %s\n", archdest);
	    fflush (stdout);
	    if (buildnf (archend, archbase, 0, 0, 0) < 0)/* make one */
	    {
		printf ("Archiver: Problems creating %s for archival\n",
			archdest);
		fflush (stdout);
		goto docompress;
	    }
	    if (init (&archio, archdest) < 0)		/* and open it */
	    {
		printf ("Archiver: Problems opening %s for archival\n",
			archdest);
		fflush (stdout);
		goto docompress;
	    }
	    locknf (&archio, DSCRLOCK);			/* watch conflicts */
	    getdscr (&archio, &archio.descr);
	    archio.descr.d_stat |= ISARCH + OPEN;
	    putdscr (&archio, &archio.descr);
	    unlocknf (&archio, DSCRLOCK);
	    /* 
	     *	Copy the active notesfile's access list to
	     *	the archive notesfile.
	     */
	    {
#ifdef	FASTFORK
		char    old[WDLEN];
		char    new[WDLEN];
		sprintf (old, "%s/%s/%s", io.basedir, io.nf, ACCESS);
		sprintf (new, "%s/%s/%s", archio.basedir, archio.nf, ACCESS);
		dounix (0, 0, "/bin/cp", old, new, 0, 0);
#else	! FASTFORK
		char    cmdline[WDLEN + WDLEN + 10];
		sprintf (cmdline, "%s %s/%s/%s %s/%s/%s",
			"/bin/cp",
			io.basedir, io.nf, ACCESS,
			archio.basedir, archio.nf, ACCESS);
		dounix (cmdline, 0, 0);
#endif	! FASTFORK
	    }
	}

	locktarget = strcmp (io.nf, archio.nf);		/* lock if differ */


	if (!(archio.descr.d_stat & ISARCH))		/* into archive? */
	{
	    printf ("Archiver: %s: Target %s is not an archive\n",
		    nfname, archdest);
	    fflush (stdout);
	    closenf (&archio);				/* close that */
	    goto docompress;				/* compress him anyway */
	}
    }


#ifdef OLDGROUP
/*
 *	This code looks at the directory to see if the notesfile
 *	has been idle long enough to be deleted.
 *
 *	This code hasn't been tested by me. It works in the 
 *	Salkind/Spickelmier version.
 *
 *	Should stuff a "wait-till-expire" in the master descriptor
 *	of each notesfile so "junk" ones can expire faster or
 *	something like that.  Essentially we want the age at which
 *	the notesfile is deleted to be grabbed from the notesfile
 *	itself.
 *
 *	My personal opinion is that they shouldn't disappear
 *	auto-magically
 *		NOTE: this probably no longer works with the 
 *		changes I've made to archiving.   (Dec '83)
 *
 *	N.B. Need some locking in here
 */

    /* delete inactive groups - RLS 1/8/83 */

    sprintf (line, "%s/%s", MSTDIR, nfname);
    stat (line, &buf);
    current = time (0);
    if (current - buf.st_mtime > 60 * 60 * 24 * (OLDGROUP - daysold))
    {
	finish (&io);

	sprintf (line, "/bin/rm -rf %s/%s", MSTDIR, nfname);
	system (line);

	gettime (&zaptime);
	sprdate (&zaptime, timeline);

	/* message in nfmaint */
	sprintf (line, "Archiver: removed %s\n", nfname);
	nfcomment (NOSUCHWARN, line, line, 0, 0);

	sprintf (line, "%s/%s/%s", MSTDIR, UTILITY, NETLOG);
	x ((log = fopen (line, "a")) == NULL, "archiver: no logfile");
	fprintf (log, "Archiver: deleted %s at %s\n", nfname, timeline);
	printf ("Archiver: deleted %s at %s\n", nfname, timeline);
	fclose (log);
	fflush (stdout);
	return (0);
    }
#endif	OLDGROUP


    locknf (&io, DSCRLOCK);				/* MUTEX */
    if (locktarget)					/* and target */
	locknf (&archio, DSCRLOCK);
    getdscr (&io, &io.descr);
    wasopen = io.descr.d_stat & OPEN;			/* hold this */
    io.descr.d_stat &= NOT OPEN;			/* privacy */
    putdscr (&io, &io.descr);

    for (i = 1; i <= io.descr.d_nnote && deletable; i++)
    {
	getnrec (&io, i, &note);
	if (note.n_stat & DELETED)
	    continue;					/* gone already */
	if (dirmsgflag == DIROFF && (note.n_stat & DIRMES))
	    continue;					/* don't if dir on */
	if (dirmsgflag == DIRON && (note.n_stat & DIRMES) == 0)
	    continue;					/* don't if dir off */
	if (inorder (&zaptime, &note.n_lmod))
	    continue;					/* too recent */
	presps = note.n_nresp;				/* response count */
	if (!deleteonly)				/* save it? */
	{
	    /* 
	     * check to see if this one is already in the archive
	     */
	    dup_place = chknote (&archio, &note.n_id, &note2);/* already there? */
	    if (dup_place == 0)				/* not there */
	    {
		/* 
		 *	This code copied almost verbatim from compression routines
		 */
#ifdef	notdef
		x ((txtfile = fopen (txtfn, "w")) == NULL, "archiver:bad txt");
		pageout (&io, &note.n_addr, txtfile);
		fclose (txtfile);
		x ((txtfile = fopen (txtfn, "r")) == NULL, "archiver: txt read");
		pagein (&archio, txtfile, &where);
		fclose (txtfile);
#else
		pagemove (&io, &note.n_addr, &archio, &where, LOCKIT);
#endif
		newnum = putnote (&archio, &where, note.ntitle, note.n_stat, &note,
			&note.n_auth, NOPOLICY, NOLOCKIT, NOADDID, note.n_from, NOADDTIME);
		getnrec (&archio, newnum, &note2);	/* get copy */
	    }
	    else
	    {
		if ((note2.n_stat & ORPHND) &&		/* archived is foster */
			!(note.n_stat & ORPHND))	/* and active isn't */
		{
#ifdef	notdef
		    x ((txtfile = fopen (txtfn, "w")) == NULL, "archiver:bad txt");
		    pageout (&io, &note.n_addr, txtfile);
		    fclose (txtfile);
		    x ((txtfile = fopen (txtfn, "r")) == NULL, "archiver: txt read");
		    pagein (&archio, txtfile, &where);
		    fclose (txtfile);
#else
		    pagemove (&io, &note.n_addr, &archio, &where, LOCKIT);
#endif

		    note.n_nresp = note2.n_nresp;	/* save resp chain */
		    note.n_rindx = note2.n_rindx;
		    note.n_addr = where;		/* get text pointer */
		    putnrec (&archio, dup_place, &note);/* replace descriptor */
		    note2 = note;			/* save good copy */
		    adopts++;				/* count 'em */
		}
		else
		{
		    dnotes++;				/* count duplicate */
		}
		newnum = dup_place;			/* for linking resps */
	    }

	    for (rnum = 1; rnum <= presps; rnum++)	/* process responses */
	    {
		if (lrsp (&io, i, rnum, &resp, &roffset, &rblock) != 0)
		    break;				/* bad response chain - drop rest */
		if (dup_place)				/* better check... */
		{
		    dup_resp = chkresp (&archio, &resp.r_id[roffset], &note2, newnum);
		    if (dup_resp)			/* already there */
		    {
			dresps++;			/* count doubles */
			continue;			/* skip this response */
		    }
		}
#ifdef	notdef
		x ((txtfile = fopen (txtfn, "w")) == NULL, "compress:bad txt");
		pageout (&io, &resp.r_addr[roffset], txtfile);
		fclose (txtfile);
		x ((txtfile = fopen (txtfn, "r")) == NULL, "compress: bad txt read");
		pagein (&archio, txtfile, &where);
		fclose (txtfile);
#else
		pagemove (&io, &resp.r_addr[roffset], &archio, &where, LOCKIT);
#endif
		putresp (&archio, &where, resp.r_stat[roffset], newnum, &resp.r_when[roffset],
			&resp.r_auth[roffset], &note, NOLOCKIT, &resp.r_id[roffset],
			NOADDID, resp.r_from[roffset], NOADDTIME, &resp.r_rcvd[roffset]);
	    }
	}
	delnote (&io, i, NOLOCKIT);			/* delete entry */
	ncount++;
	rcount += presps;				/* and responses */
	deletable--;					/* one down */
    }

    unlocknf (&io, DSCRLOCK);				/* Un MUTEX */
    if (locktarget)					/* and the target */
	unlocknf (&archio, DSCRLOCK);

    if (!deleteonly)
    {
	finish (&archio);				/* close target */
	unlink (txtfn);					/* don't litter */
    }

/*
 *	Time to compress the notesfile and eliminate those
 *	unsightly holes in the data structure.
 */
docompress: 

    locknf (&io, DSCRLOCK);				/* MUTEX */
    if (io.descr.d_nnote != 0)				/* non-empty */
    {
	if (io.descr.d_delnote != 0 || io.descr.d_delresp != 0)
	{						/* has holes */
	    int     nnote,
	            nresp;
	    compress (&io, NOLOCKIT, 0, &nnote, &nresp);
	    printf ("Archiver: %s contains (%d,%d) after compress\n",
		    nfname, nnote, nresp);
	    fflush (stdout);
	}
	else						/* no holes so */
	{						/* don't compress */
	    printf ("Archiver: %s already compressed\n", nfname);
	    fflush (stdout);
	}
    }
    else						/* nothing there to */
    {							/* compress */
	printf ("Archiver: %s is empty.\n", nfname);
	fflush (stdout);
    }


    if (wasopen)					/* if it was already */
    {
	getdscr (&io, &io.descr);			/* open season */
	io.descr.d_stat |= OPEN;
	putdscr (&io, &io.descr);			/* replace in file */
    }

    unlocknf (&io, DSCRLOCK);				/* all done with this */
    finish (&io);					/* and close the notesfile */

    gettime (&zaptime);
    sprdate (&zaptime, timeline);
    if (ncount)						/* log only if did somethine */
    {
	sprintf (line, "%s/%s/%s", Mstdir, UTILITY, NETLOG);
	x ((log = fopen (line, "a")) == NULL, "archiver: no logfile");
	if (!deleteonly)
	    fprintf (log, "%s: archived (%d,%d) [%d,%d dups, %d adopted] into %s at %s\n",
		    nfname, ncount, rcount, dnotes, dresps, adopts, archdest, timeline);
	else
	    fprintf (log, "%s: Archiver deleted (%d,%d) at %s\n",
		    nfname, ncount, rcount, timeline);
	fclose (log);
    }

    if (!deleteonly)
    {
	if (ncount)
	    printf ("Archiver: %s: (%d,%d) [%d,%d dups, %d adoptions] into %s at %s\n",
		    nfname, ncount, rcount, dnotes, dresps, adopts, archdest, timeline);
	else
	    printf ("Archiver: %s: no notes archived\n",
		    nfname);
    }
    else
	printf ("Archiver: %s: deleted (%d,%d) at %s\n",
		nfname, ncount, rcount, timeline);
    fflush (stdout);
    return (0);						/* and return */

}
