static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"
#include "net.h"
#include "globs.h"			/* get the main body variables */

static int  usetime;			/* whether overriding sequencer time */
static int  sendnews;			/* forwarding news orig? */
static int  callback;			/* whether to have remote send back */
static char tosite[SYSSZ + 20];		/* with some buffer space */
static char dmpfile[WDLEN];		/* a scratch file name */
static struct when_f    ztime;		/* the explicit time */
static int number;

storeone(local)
char *local;
{
    char nfname[NNLEN];				/* hold aliased nf name */

    nfalias(local, nfname, tosite);		/* get remote file name */
    sprintf(dmpfile,"/tmp/notes/%s",nfname);	/* create file name */
    if (nfsend(tosite, local, dmpfile, usetime, &ztime, sendnews) <= 0) {
	unlink(dmpfile);			/* empty file */
    } else {
	number++;				/* number of groups sent */
    }

    return;
}


send()
{
    char    cmdline[CMDLEN];
    char    fn[CMDLEN];

    if (number > 0) {
	sprintf(fn, "/tmp/nfarch%d",getpid());	/* archive file */
	sprintf(cmdline, "ar cr %s /tmp/notes/*", fn);

	system(cmdline);		/* create the archive */

	sprintf(cmdline, "(echo %s;cat %s) | /bin/mail Notes%s", SYSTEM, fn, tosite);
	system(cmdline);		/* mail it */

	unlink(fn);			/* remove the archive file */
    }
    system("/bin/rm -rf /tmp/notes");	/* remove the notes stuff */

    return;
}


main (argc, argv)
char  **argv;
{
    int count, c, i;
    char *p;
    char cmdline[CMDLEN];		/* build your favorite command */
    FILE *flist;			/* list of notesfiles to send */

#include "main.i"			/* common init code and such */

    umask(0);
    setuid(NOTESUID);

    sendnews = 0;		/* default no send news */
    usetime = 0;		/* default to the sequencer supplied time */
    callback = 0;		/* default to no return messages */
    number = 0;
    sprintf (tosite, "*None*");	/* null site */

    for (i = 1; i < argc; i++)	/* parse options */
	switch (argv[i][0]) {
	    case '-': 
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

#ifdef BSD4.1c
    mkdir("/tmp/notes",0777);
#else
    system("mkdir /tmp/notes");
#endif BSD4.1c

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
	    storeone(group[i].name);		/* store notes */
	}
    }

    send();				/* archive and send to site */

    exit (GOOD);
}
