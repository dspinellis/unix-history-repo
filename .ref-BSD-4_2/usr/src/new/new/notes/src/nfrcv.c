static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"
#include "net.h"
#include "globs.h"
/* 
 *	netrcv - load a notefile from the network
 *	accepts 2 parameters (1) the source system
 *			     (2) the notefile to load into
 *
 *	This program should be called only by the netload program
 *	or a suitably informed user/wizard
 *
 *	Original author:	Ray Essick	december 1981
 */

main (argc, argv)
char  **argv;
{
    struct io_f io;
    FILE * log,				/* log file for network transactions */
	*fopen ();
    char    fn[WDLEN];			/* hold netwrk log file name */
    char    nowtime[DATELEN];
    char    buf[CMDLEN];			/* should hold it all */
    char    line[CMDLEN];
    struct when_f   atime;

#include "main.i"			/* common init code and such */

    if (argc != 3) {
	printf ("Usage: %s notefile fromsystem\n", argv[0]);
	exit (BAD);
    }

    sprintf (fn, "%s/%s/%s", MSTDIR, UTILITY, NETLOG);
    gettime (&atime);
    sprdate (&atime, nowtime);

    if (init (&io, argv[1]) < 0) {
	
#ifdef AUTOCREATE
	/* try to create the notes file */
	sprintf(line,"%s/%s/mknf -on %s",MSTDIR,UTILITY,argv[1]);
	system(line);
	if (init(&io, argv[1]) < 0) {
	    sprintf(line, "nfrcv: notesfile: %s, from %s\n", argv[1],argv[2]);
	    nfcomment(NOSUCHWARN, line, "Failure", 0, 0);
	    exit(BAD);
	}
	    
	sprintf(line, "nfrcv: created: %s, from %s\n", argv[1],argv[2]);
        nfcomment(NOSUCHWARN, line, line, 0, 0);
#else
	sprintf(line, "nfrcv: notesfile: %s, from %s\n", argv[1],argv[2]);
	nfcomment(NOSUCHWARN, line, "Failure", 0, 0);
	exit(BAD);
#endif
    }

    if ((io.descr.d_stat & NETWRKD) == 0) {
	printf ("%s: %s is not a networked notefile\n", SYSTEM, argv[1]);
	finish (&io);

	glock (&io, LOGLOCK);		/* make log entry */
	x ((log = fopen (fn, "a")) == NULL, "netrcv: bad log open");

	fprintf (log, "Non-networked notefile: %s sent from %s at %s\n",
		argv[1], argv[2], nowtime);
	x (fclose (log) == EOF, "netrcv: bad close of log file");

	gunlock (&io, LOGLOCK);		/* unlock */
	exit (NONF);			/* un-networked appears not there */
    }

    loadem (&io, stdin, LOCKIT, argv[2], NODETAIL);

    glock (&io, LOGLOCK);		/* make log entry */
    x ((log = fopen (fn, "a")) == NULL, "netrcv: bad log open");

    fprintf (log, "%s: received at %s from %s\n", argv[1],
	    nowtime, argv[2]);
    fprintf (log, "\tInserted: %d notes & %d responses. Dropped %d notes and %d responses\n",
	    io.nnotrcvd, io.nrsprcvd, io.nnotdrop, io.nrspdrop);
    x (fclose (log) == EOF, "netrcv: bad close of log file");
    gunlock (&io, LOGLOCK);

    lock(&io, 'n');

    getdscr (&io, &io.descr);
    io.descr.netwrkins++;		/* bump the count */
    putdscr (&io, &io.descr);

    unlock(&io, 'n');

    printf ("%s: notefile: %s\tInserted: %d notes and %d responses from %s\n",
	    SYSTEM, argv[1], io.nnotrcvd, io.nrsprcvd, argv[2]);
    printf ("%s: notefile: %s\tDropped: %d notes and %d responses from %s\n",
	    SYSTEM, argv[1], io.nnotdrop, io.nrspdrop, argv[2]);

    finish (&io);
    exit (GOOD);
}
