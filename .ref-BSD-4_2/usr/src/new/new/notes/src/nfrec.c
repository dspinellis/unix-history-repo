static char *sccsid = "@(#)nfrcv.c	1.3 2/2/83";

#include "parms.h"
#include "structs.h"
#include "net.h"
#include "globs.h"
#include <sys/types.h>
#ifdef BSD4.1c
#include <sys/dir.h>
#else
#include <ndir.h>
#endif BSD4.1c

main (argc, argv)
char  **argv;
{
    struct io_f io;
    FILE *log, *fp;
    char    fn[WDLEN];			/* temp file name */
    char    fnlog[WDLEN];		/* hold netwrk log file name */
    char    fndir[WDLEN];		/* temp directory */
    char    nowtime[DATELEN];
    char    buf[CMDLEN];			/* should hold it all */
    char    line[CMDLEN];
    struct when_f   atime;
    DIR *dirp;
    struct direct *entry;
    char last;
    char *fromsys;
    int count;
    int header;
    char c;

#include "main.i"			/* common init code and such */

    setuid(NOTESUID);
    umask(0);

    sprintf (fnlog, "%s/%s/%s", MSTDIR, UTILITY, NETLOG);
    gettime (&atime);
    sprdate (&atime, nowtime);

    sprintf(fn,"/tmp/nfarch%d",getpid());

    /* strip off the header */
    if ((fp = fopen(fn,"w")) == NULL) {
	fprintf(stderr,"nfrec: can't open archive file\n");
	exit(BAD);
    }

    header = 1;
    count = 0;
    last = ' ';
    while ((c = getchar()) != EOF) {
	if (header == 1) {
	    if (last == '\n' && c == '\n') header = 0;
	    last = c;
	} else {
	    if (fromsys == NULL) {
		if (c == '\n') {
		    fromsys[count++] = c;
		} else {
		    fromsys[count] = '\0';
		}
	    } else {
		putc(c,fp);
	    }
	}
    }
    fclose(fp);

    sprintf(fndir,"/tmp/notes%d",getpid());
#ifdef BSD4.1c
    mkdir(fndir, 0777);
#else
    sprintf(line,"mkdir %s",fndir);
    system(line);
#endif BSD4.1c

    chdir(fndir);
    sprintf(line,"ar x %s",fn);
    system(line);
    unlink(fn);

    if ((dirp = opendir(fndir)) == NULL) {
	exit(BAD);
    }

    while ((entry = readdir(dirp)) != NULL) {	/* search thru the directory */

	strcpy(fn, entry->d_name);		/* file name */
	if (fn[0] == '.') continue;

	if (init (&io, fn) < 0) {

#ifdef AUTOCREATE
	    /* try to create the notes file */
	    sprintf(line,"%s/%s/mknf -on %s",MSTDIR,UTILITY,fn);
	    system(line);
	    if (init(&io, fn) < 0) {
		sprintf(line,"nfrcv: notesfile: %s, from %s\n",fn,fromsys);
		nfcomment(NOSUCHWARN, line, "Failure", 0, 0);
		continue;
	    }
		
	    sprintf(line, "nfrcv: created: %s, from %s\n", fn,fromsys);
	    nfcomment(NOSUCHWARN, line, line, 0, 0);
#else
	    sprintf(line, "nfrcv: notesfile: %s, from %s\n", fn,fromsys);
	    nfcomment(NOSUCHWARN, line, "Failure", 0, 0);
	    continue;
#endif
	}

	if ((io.descr.d_stat & NETWRKD) == 0) {
	    finish (&io);

	    glock (&io, LOGLOCK);		/* make log entry */
	    x ((log = fopen (fnlog, "a")) == NULL, "netrcv: bad log open");

	    fprintf (log, "Non-networked notefile: %s sent from %s at %s\n",
		    fn, fromsys, nowtime);
	    x (fclose (log) == EOF, "netrcv: bad close of log file");

	    gunlock (&io, LOGLOCK);		/* unlock */
	    continue;
	}

	chdir(fndir);

	if ((fp = fopen(fn,"r")) == NULL) {
	    continue;
	}

	loadem (&io, fp, LOCKIT, fromsys, NODETAIL);

	glock (&io, LOGLOCK);		/* make log entry */
	x ((log = fopen (fnlog, "a")) == NULL, "netrcv: bad log open");

	fprintf (log, "%s: received at %s from %s\n", fn,
		nowtime, fromsys);
	fprintf (log, "\tInserted: %d notes & %d responses. Dropped %d notes and %d responses\n",
		io.nnotrcvd, io.nrsprcvd, io.nnotdrop, io.nrspdrop);
	x (fclose (log) == EOF, "netrcv: bad close of log file");
	gunlock (&io, LOGLOCK);

	lock(&io, 'n');

	getdscr (&io, &io.descr);
	io.descr.netwrkins++;		/* bump the count */
	putdscr (&io, &io.descr);

	unlock(&io, 'n');

	finish (&io);
	chdir(fndir);
	unlink(fn);
    }
    closedir(dirp);			/* close the directory */
    chdir("/tmp");
#ifdef BSD4.1c
    rmdir(fndir);
#else
    sprintf(line,"rmdir %s",fndir);
    system(line);
#endif BSD4.1c
    exit (GOOD);
}
