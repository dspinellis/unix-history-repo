#include "parms.h"
#include "structs.h"
#include <sys/types.h>
#include <sys/stat.h>

#ifdef	RCSIDENT
static char rcsid[] = "$Header: buildnf.c,v 1.7 85/01/18 15:05:13 notes Rel $";
#endif	RCSIDENT

/*
 *	buildnf
 *
 *	A subroutine which creates notesfiles. Given the (1) name of
 *	the new notesfile and (2) the anonymous, open, and 
 *	networked flags this routine will create a new notesfile,
 *	provided that it does not exist already.
 *
 *	This routine will blindly create the notesfile. It does not 
 *	check the real uid to see if it should create a notesfile or
 *	not.
 *
 *	Returns:	0	Notesfile created
 *			-1	Error in notesfile creation
 *			-2	Notesfile exists
 *
 *	Extracted from old "mknf" mainline. Modified mknf so it
 *	calls this routine.
 *	Also put conditional code into newsinput.c to create the 
 *	new notesfile if desired.
 *
 *	Original Coding:	Ray Essick	October 13, 1982
 */
#define	N_UMASK		0				/* when making nf's */

buildnf (nfname, basedir, Aflag, Oflag, Nflag)
char   *nfname;
char   *basedir;					/* nf directory */
int     Aflag,
        Oflag,
        Nflag;
{
    struct io_f io;
    char   *p;						/* misc. pointer */
    int     i;						/* misc counter */
    int     fid;					/* misc file id */
    struct daddr_f  freetext;				/* for writing */
    struct descr_f  descr;				/* for writing, too */
    struct perm_f   perms[NPERMS];
    struct perm_f   pentry;				/* single entry */
    struct auth_f   me;					/* for access list */
    char    cmdline[WDLEN];				/* for mkdir */
    char    pathname[WDLEN];				/* for filenames */
    char    filename[WDLEN];				/* for filenames */
    FILE * seqfile;
    FILE * pfile;					/* access list mods */
    struct stat statval;				/* testing existence */
    int     old_umask;					/* save his mask */

    globuid = Notesuid;					/* must be notes to work */
							/* users can't see this */
    if (chkpath (nfname))
    {
	printf ("Invalid notesfile name: `%s'\n", nfname);
	return (-1);					/* bad return */
    }

    if (stat (basedir, &statval) == -1)			/* can we read basedir? */
    {
	printf ("can't stat (assumed directory) %s\n", basedir);
	return (-1);					/* error in creation */
    }

    sprintf (pathname, "%s/%s", basedir, nfname);
    if (stat (pathname, &statval) == 0)			/* check existence */
    {
	printf ("Notesfile already exists\n");
/*
 *	Could talk about whether it's a file or directory...
 *	but probably not worth it.
 */
	return (-2);					/* already exists */
    }

    old_umask = umask (N_UMASK);			/* clear mask */
#ifdef	BSD42
    {
	/* 
	 * take advantage of 4.2 bsd system call...
	 */
	mkdir (pathname, 0770);				/* make the directory */
    }
#else
    {
	/* 
	 * do it the hard way ... by forking children
	 */
#ifndef	FASTFORK
	sprintf (cmdline, "mkdir %s", pathname);	/* build command */
	dounix (cmdline, 1, 0);				/* execute, captain */
	sprintf (cmdline, "chmod 0770 %s", pathname);	/* protect him */
	dounix (cmdline, 1, 0);				/* really do it */
#else
	dounix (1, 0, "/bin/mkdir", pathname, 0, 0, 0);
	dounix (1, 0, "/bin/chmod", "0770", pathname, 0, 0);
#endif	FASTFORK
    }
#endif	BSD42

/*
 *	Now build the files within the directory
 *	Build a default notesfile descriptor and then override with
 *	his specific values.
 */


    initdescr (&descr);					/* make default */

    glocknf (&io, SEQLOCK);				/* grab sequence # */
    sprintf (cmdline, "%s/%s", Mstdir, SEQ);
    x ((seqfile = fopen (cmdline, "r")) == NULL, "mknf: open unique");
    x (fscanf (seqfile, "%d", &i) != 1, "mknf: unique read");
    descr.d_nfnum = i++;
    fclose (seqfile);					/* close it and */
    x ((seqfile = fopen (cmdline, "w")) == NULL, "mknf: reopen unique");
    fprintf (seqfile, "%d\n", i);			/* update */
    fclose (seqfile);
    gunlocknf (&io, SEQLOCK);				/* release file */

    strncpy (descr.d_title, nfname, NNLEN);
    descr.d_title[NNLEN - 1] = '\0';			/* null it */

    if (Aflag)						/* he wants anon */
	descr.d_stat |= ANONOK;
    if (Oflag)						/* open */
	descr.d_stat |= OPEN;
    if (Nflag)						/* networkable */
	descr.d_stat |= NETWRKD;


    sprintf (filename, "%s/%s", pathname, INDEXN);	/* make the file */
    x ((fid = creat (filename, 0660)) < 0, "build:  Cant create note.indx");
    x (write (fid, &descr, sizeof descr) != sizeof descr,
	    "build:  Cant write note.indx");
    x (close (fid) < 0, "build: bad note.indx close");

/*
 *	Now create the text file
 */

    sprintf (filename, "%s/%s", pathname, TEXT);
    x ((fid = creat (filename, 0660)) < 0, "build:  Cant create text");
    freetext.addr = sizeof freetext;			/* past address slot */
    if (freetext.addr & 1)				/* odd (shouldn't be) */
	freetext.addr++;				/* make it even */
    x (write (fid, &freetext, sizeof freetext) != sizeof freetext,
	    "build:  Cant write text");

    x (close (fid) < 0, "build: bad text close");

/*
 *	Now the response index file.
 */

    sprintf (filename, "%s/%s", pathname, INDEXR);
    x ((fid = creat (filename, 0660)) < 0, "build:  Cant create resp.indx");
    i = 0;						/* resp 0 slot free */
    x (write (fid, &i, sizeof i) != sizeof i, "build:  Cant write resp.indx");
    x (close (fid) < 0, "build: bad close of resp.indx");

/*
 *	Build the access list.  This is a two phase operation.
 *	The first phase builds a default access list.  This
 *	is set up with one director (the notes signon) and
 *	a group "Other" with read/write.  System "Other" also
 *	has read/write privileges.  This is basically a wide-open
 *	policy.
 *	The second phase is customization.  This step uses the
 *	contents of the file ..../notes/.utilities/access-template
 *	as a batch of access specifications (like nfaccess)
 *	to add/modify specific permissions. 
 */

    sprintf (filename, "%s/%s", pathname, ACCESS);	/* build simple */
    x ((fid = creat (filename, 0660)) < 0, "build: couldnt create access");
    getname (&me, 0);					/* grab "notes" */
    perms[0].ptype = PERMUSER;
    strcpy (perms[0].name, me.aname);
    perms[0].perms = READOK + WRITOK + RESPOK + DRCTOK;
    perms[1].ptype = PERMGROUP;
    strcpy (perms[1].name, "Other");
    perms[1].perms = DFLTPERMS;
    perms[2].ptype = PERMSYSTEM;
    strcpy (perms[2].name, "Other");
    perms[2].perms = DFLTPERMS;

    x (write (fid, perms, 3 * sizeof pentry) != 3 * sizeof pentry,
	    "build: bad access write");
    x (close (fid) < 0, "build: close access broke");

/*
 *	Now, process any extra specifications from utilities/access-template
 *	Don't do anything unless the file exists.
 */

    sprintf (filename, "%s/%s/%s-template", Mstdir, UTILITY, ACCESS);
    if ((pfile = fopen (filename, "r")) != NULL)
    {
	char    pline[BUFSIZ];
	int     nmodes,
	        zapindex;


	nmodes = 0;					/* load perms[] */
	while (fgets (pline, sizeof pline, pfile) != NULL)
	{
	    if (pline[0] == '#')			/* comment */
		continue;
	    if (nmodes == NPERMS)			/* full list */
		break;
	    zapindex = strlen (pline);
	    if (pline[zapindex] == '\n')
		pline[zapindex] = '\0';			/* zap newline */
	    if (parsemode (pline, &perms[nmodes], 0) == 0)/* worked? */
		nmodes++;				/* bump counter */
	}
	fclose (pfile);					/* done with template */
	if (init (&io, pathname) >= 0)			/* open the nf */
	{
	    addmodes (&io, nmodes, perms, 0);		/* add them */
	    closenf (&io);
	}
    }

    x (umask (old_umask) != N_UMASK, "buildnf: reset umask");/* restore umask */
    return 0;						/* all's well */
}

/*
 *	initdescr(&descr_f)
 *
 *	Load a descriptor with a lot of default information
 *	Other people will load things like titles and specific
 *	values of flags.
 */

initdescr (descr)
struct descr_f *descr;
{
    register int    i;
    register char  *p;

    descr -> d_format = DBVERSION;			/* version */
    strcpy (descr -> d_title, "** Notesfile Title **");
    strcpy (descr -> d_drmes, "** director message **");
    descr -> d_plcy = 0;				/* no policy note */
    descr -> d_id.uniqid = 0;				/* unique within nf */
    strcpy (descr -> d_id.sys, System);			/* and system name */

    gettime (&descr -> d_lstxmit);			/* last network xmit */
    gettime (&descr -> d_lastm);			/* last modified now */
    gettime (&descr -> d_created);			/* creation date */
    gettime (&descr -> d_lastuse);			/* for days online */
    descr -> d_daysused = 1;				/* count making it ! */
    descr -> d_stat = 0;				/* no special status */
    descr -> d_nnote = 0;				/* no notes in file */
    descr -> d_delnote = descr -> d_delresp = 0;	/* no holes */
    descr -> d_archtime = 0;				/* default time */
    descr -> d_workset = 0;				/* working set */
    descr -> d_archkeep = KEEPDFLT;			/* keep/delete */
    descr -> d_dmesgstat = DIRDFLT;			/* dirmsg to expire */

    descr -> d_rspwrit = descr -> d_notwrit = 0;	/* initialize stats */
    descr -> d_rspread = descr -> d_notread = 0;
    descr -> d_notdrop = descr -> d_rspdrop = 0;
    descr -> d_orphans = 0;				/* orphan chains */
    descr -> d_adopted = 0;				/* chains adopted */
    descr -> entries = 0;				/* user entries */
    descr -> walltime = 0;
    descr -> d_rspxmit = descr -> d_notxmit = 0;	/* network activity */
    descr -> d_rsprcvd = descr -> d_notrcvd = 0;
    descr -> netwrkouts = descr -> netwrkins = 0;
    descr -> d_longnote = MAXMSG;			/* longest permitted */
}
