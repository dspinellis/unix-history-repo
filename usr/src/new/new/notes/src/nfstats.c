static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"
#include "globs.h"
/*
 *	stats - print out the statistics for some notefiles.
 *	Takes a list of notefiles, and prints the statistics that are
 *	kept about that notefile.
 *	A total summary is also presented.
 *
 *	Original coding:	Ray Essick	December 29, 1981
 *	Modified for wildcards:	Ray Essick	April 8, 1982
 */

struct when_f   ztime;
char    fmtd[DATELEN];					/* for time of report stuff */
int     statsdone;
int     summary;					/* summary only flag */
long gnotwrit,
	grspwrit,
	gnotread,
	grspread,
	gnotxmit,
	grspxmit,
	gnotdrop,
	grspdrop,
	gorphans,
	netwrkouts,
	netwrkins,
	gnotrcvd,
	grsprcvd,
	totaltime,
	entries;
							/* global summaries */

statone (nfname)
char   *nfname;
{

    struct io_f io;
    if (init (&io, nfname) < 0) {
	printf("No such notesfile: %s\n", nfname);
	return;
    }
    gnotwrit += io.descr.d_notwrit;	/* add in global stats */
    grspwrit += io.descr.d_rspwrit;
    gnotread += io.descr.d_notread;
    grspread += io.descr.d_rspread;
    gnotxmit += io.descr.d_notxmit;
    grspxmit += io.descr.d_rspxmit;
    gnotrcvd += io.descr.d_notrcvd;
    grsprcvd += io.descr.d_rsprcvd;
    gnotdrop += io.descr.d_notdrop;
    grspdrop += io.descr.d_rspdrop;
    gorphans += io.descr.d_orphans;
    netwrkouts += io.descr.netwrkouts;
    netwrkins += io.descr.netwrkins;
    totaltime += io.descr.walltime;
    entries += io.descr.entries;
    gettime (&ztime);
    sprdate (&ztime, fmtd);				/* get time now */

    if (summary == 0) {
	if (statsdone++ != 0) {
	    printf ("\n");
	}
	printf ("             %s on %s at %s\n", nfname, io.descr.d_id.sys, fmtd);
	printf ("                \tNOTES\tRESPS\tTOTALS\n");
	printf ("Local Reads     \t%ld\t%ld\t%ld\n", io.descr.d_notread,
		io.descr.d_rspread, io.descr.d_notread + io.descr.d_rspread);
	printf ("Local Written   \t%ld\t%ld\t%ld\n",
		io.descr.d_notwrit - io.descr.d_notrcvd,
		io.descr.d_rspwrit - io.descr.d_rsprcvd,
		io.descr.d_notwrit + io.descr.d_rspwrit - io.descr.d_notrcvd - io.descr.d_rsprcvd);
	printf ("Networked in    \t%ld\t%ld\t%ld\n", io.descr.d_notrcvd, io.descr.d_rsprcvd,
		io.descr.d_notrcvd + io.descr.d_rsprcvd);
	printf ("Networked out   \t%ld\t%ld\t%ld\n", io.descr.d_notxmit, io.descr.d_rspxmit,
		io.descr.d_notxmit + io.descr.d_rspxmit);
	printf ("Network Dropped \t%ld\t%ld\t%ld\n", io.descr.d_notdrop, io.descr.d_rspdrop,
		io.descr.d_notdrop + io.descr.d_rspdrop);
	printf ("Network Transmissions: %ld   Network Receptions: %ld\n",
		io.descr.netwrkouts, io.descr.netwrkins);
	printf ("Orphaned Responses Received: %ld    Entries into notefile: %ld\n",
		io.descr.d_orphans, io.descr.entries);
	printf ("Total time in notefile: %8.2f minutes ",
		((float) io.descr.walltime / 60.0));
	if (io.descr.entries > 0) {
	    printf (" Average Time/entry: %6.2f minutes\n",
		((float) io.descr.walltime / 60.0 / (float)  io.descr.entries));
	} else
		printf("\n");
	sprdate (&io.descr.d_created, fmtd);
	printf ("Created at %s, Used on %ld days\n", fmtd, io.descr.d_daysused);
    }

    closenf (&io);				/* close this notefile */
}


main (argc, argv)
char  **argv;
{
    int     i;

#include "main.i"				/* common init code and such */

    if (argc == 1) {
	printf ("Usage: %s [-s] notefile ...\n", argv[0]);
	exit (BAD);
    }

    gnotwrit = grspwrit = gnotread = grspread = 0;
    gnotxmit = grspxmit = gnotrcvd = grsprcvd = 0;
    gnotdrop = grspdrop = gorphans = 0;
    netwrkouts = netwrkins = 0;
    totaltime = entries = 0;
    statsdone = 0;			/* number of notesfiles reported */
    summary = 0;			/* summary only flag */

    for (i = 1; i < argc; i++) {
	if (argv[i][0] == '-') {
	    switch (argv[i][1]) {
		case 's': 				/* summary only */
		    summary = 1;
		    continue;

		default: 
		    printf ("Bad switch `%c'\n", argv[i][1]);
		    exit (BAD);
	    }
	}
	expand(argv[i]);
    }

    for (i = 0; i < last_group; i++) {
	if (group[i].lookat == 1) {
	    statone(group[i].name);
	}
    }

    if ((statsdone > 1) || (summary == 1)) {
	printf ("\n            Totals for all above notefiles\n");
	printf ("                \tNOTES\tRESPS\tTOTALS\n");
	printf ("Local Read      \t%ld\t%ld\t%ld\n", gnotread,
		grspread, gnotread + grspread);
	printf ("Local Written   \t%ld\t%ld\t%ld\n", gnotwrit - gnotrcvd,
		grspwrit - grsprcvd, gnotwrit + grspwrit - gnotrcvd - grsprcvd);
	printf ("Networked in    \t%ld\t%ld\t%ld\n", gnotrcvd, grsprcvd,
		gnotrcvd + grsprcvd);
	printf ("Networked out   \t%ld\t%ld\t%ld\n", gnotxmit, grspxmit,
		gnotxmit + grspxmit);
	printf ("Network Dropped \t%ld\t%ld\t%ld\n", gnotdrop, grspdrop,
		gnotdrop + grspdrop);
	printf ("Network Transmissions: %ld   Network Receptions: %ld\n",
		netwrkouts, netwrkins);
	printf ("Orphaned Responses: %ld  Entries into notefiles: %ld\n",
		gorphans, entries);
	printf ("Total time spent in notefile: %8.2f minutes", ((float) totaltime / 60.0));
	if (entries > 0) printf (" Average Time/entry: %6.2f minutes\n", ((float) totaltime / 60.0 / (float)  entries));

    }
    exit (GOOD);
}
