static char *sccsid = "@(#)nfxmit.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
#include "net.h"
#include "globs.h"			/* get the main body variables */
/*
 *	This program (netsend) will parse off the control card a destination 
 *	site, an optional date for sequencing purposes, and then a list
 *	of notefiles. These notefiles are then scanned to send new items
 *	to the destination site. The entire arguement list is scanned once
 *	to pull in the switches and then again to dump the notefiles.
 *	This implies that the switches are indeed global, and that the last
 *	specified ones are the ones which are used.
 *
 *	Original Coding:	Ray Essick	December 1981
 */

static int  usetime;			/* whether overriding sequencer time */
static int  sendnews;			/* forwarding news orig? */
static int  callback;			/* whether to have remote send back */
static char tosite[SYSSZ + 20];		/* with some buffer space */
static char dmpfile[WDLEN];		/* a scratch file name */
static struct when_f    ztime;		/* the explicit time */

xmitone (local)
char   *local;
{
    char    cmdline[CMDLEN];		/* build your favorite command */
    char    buf[CMDLEN];
    char    nfname[NNLEN];		/* hold aliased nf name */
    char   *xmit,
           *rply;			/* for non-standard */

    getnet (tosite, &xmit, &rply);	/* see if non standard */
    nfalias (local, nfname, tosite);	/* get remote file name */

    if (callback) {
	if (rply == NULL) {
	    sprintf (cmdline, "uux -z %s!%s -d%s %s",
		    tosite, NFXMIT, SYSTEM, nfname);
	} else {
	    sprintf (cmdline, rply, nfname, SYSTEM);    /* do his */
	}
	dounix (0, 0, SHELL, "-c", cmdline, 0, 0);  /* let shell interpret */
    }
    if (nfsend (tosite, local, dmpfile, usetime, &ztime, sendnews) > 0) {
	if (xmit == NULL) {
	    sprintf (cmdline,
		    "uux -z - %s!%s %s %s < %s",
		    tosite, NFRCV, nfname, SYSTEM, dmpfile);
	} else {
	    sprintf (buf, xmit, nfname, SYSTEM);
	    sprintf (cmdline, "%s < %s", buf, dmpfile); /* feed stdin to it */
	}
	dounix (0, 0, SHELL, "-c", cmdline, 0, 0);
    }
    x (unlink (dmpfile) < 0, "netsend: bad unlink of tmp file");
}

main (argc, argv)
char  **argv;
{
    int     count,
            c,
            i;
    char   *p;
    char    cmdline[CMDLEN];		/* build your favorite command */
    FILE * flist;			/* list of notesfiles to send */

#include "main.i"			/* common init code and such */

    if (argc == 1) {
	fprintf (stderr, "Usage: %s -d<site> [-r] [-a] [-tdd[mm[yy]]]  [-f file] nf [nf2 ..]\n", argv[0]);
	exit (BAD);
    }
    sendnews = 0;		/* default no send news */
    usetime = 0;		/* default to the sequencer supplied time */
    callback = 0;			/* default to no return messages */
    sprintf (tosite, "*None*");		/* null site */

    for (i = 1; i < argc; i++)		/* parse options */
	switch (argv[i][0]) {
	    case '-': 			/* some options oh goody */
		switch (argv[i][1]) {
		    case 'd': 
			if (strmove (argv[i] + 2, tosite) > SYSSZ) {
			    printf ("%s: system name: %s, too long\n", SYSTEM, argv[i] + 2);
			    exit (BAD);
			}
			break;		/* out of this switch statement */

		    case 'r': 		/* have them send a copy back */
			callback = 1;
			break;

		    case 'a': 		/* permit sending of news articles */
			sendnews = 1;
			break;

		    case 't': 			/* explicit time */
			gettime (&ztime);	/* grab now */
			ztime.w_hours = ztime.w_mins = 0;
						/* midnight */
			if (sscanf (argv[i] + 2, "%2hd%2hd%2hd", &ztime.w_day,
				    &ztime.w_month, &ztime.w_year) == 3)
							/* year specfd? */
			    ztime.w_year += 1900;      /* correct it */
			usetime = 1;			/* use this time */
			break;

		    case 'f': 		/* next list is a file name */
			i++;		/* skip over the file */
			break;

		    default: 
			printf ("%s: bad switch '%c'\n", SYSTEM, argv[i][1]);
			exit (BAD);
		}
	    default: 			/* this must be a notefile name(s) */
		count = 0;
		p = argv[i];
		while (*p++) {
		    count++;		/* count name of notefile */
		    if (count-1 > NNLEN) {
			printf ("%s: notefile name %s too long\n",
				    SYSTEM, argv[i]);
			exit (BAD);
		    }
		    if (*p == ' ') count = 0;
		}
		break;
	}

    sprintf (dmpfile, "/tmp/nfxmit%d", getpid ());
    if (strcmp ("*None*", tosite) == 0) {
	printf ("%s: null destination - use -d flag\n",SYSTEM);
	exit (BAD);
    }

/*	now that we have processed all the parameters, lets dump the
 *	notes and send them to the other people.
 *	This is a 2 step process. First we make a file and then
 *	we 'uucp' it to the other site.
 */

    for (i = 1; i < argc; i++) {
	switch (argv[i][0]) {		/* what is this arguement */
	    case '-': 
		if (argv[i][1] != 'f') {
		    break;		/* ignore parameters */
		}
		if ((flist = fopen (argv[++i], "r")) == NULL)
		    break;		/* couldn't grab file */
		do {
		    count = 0;
		    while (((c = getc (flist)) != ' ') && (c != '\n') && (c != EOF))
			cmdline[count++] = c;		/* build the name */
		    if (count != 0) {
			cmdline[count++] = '\0';	/* null terminate */
			expand(cmdline);
		    }
		} while (c != EOF);		/* until file exhausted */
		fclose (flist);			/* and close the file */
		break;


	    default: 				/* these are all notefiles */
		expand(argv[i]);
		break;
	}
    }
    for (i = 0; i < last_group; i++) {
	if (group[i].lookat == 1) {
	    xmitone(group[i].name);
	}
    }
    exit (GOOD);
}
