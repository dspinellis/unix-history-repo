static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"
#include <sys/types.h>
#include <sys/stat.h>
#ifdef BSD4.1c
#include <sys/dir.h>
#endif BSD4.1c

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
 */

archiver(nfname, daysold, deleteonly, dirmsgflag)
char   *nfname;
{
    struct io_f io;
    struct when_f   zaptime;				/* boundary time */
    struct note_f   note;
    int     archfid,
            i,
            ncount;
    char    line[CMDLEN];
    char    archfile[WDLEN];
    char    timeline[DATELEN];
    long    current;
    struct stat buf;
    FILE *log, *farchfile;
#ifdef BSD4.1c
    DIR *dir;
    int fid;
#endif BSD4.1c

    if (init(&io, nfname) < 0)
	return(-1);					/* no notesfile */

    if ((allow(&io, DRCTOK) == 0) && (globuid != NOTESUID)) {
	closenf (&io);
	printf ("You are not allowed to archive the %s notesfile\n", io.nf);
	return(-1);
    }

    gettime(&zaptime);					/* make the old time */
    sprintf(archfile, "%02d.%02d.%02d:%02d%02d", zaptime.w_year % 100,
	    zaptime.w_month, zaptime.w_day, zaptime.w_hours, zaptime.w_mins);

    zaptime.w_hours = zaptime.w_mins = 0;		/* zilch these */
    zaptime.w_day -= daysold;				/* reset days */
    while (zaptime.w_day <= 0) {			/* fix month wrap */
	zaptime.w_day += 30;
	zaptime.w_month -= 1;
    }
    while (zaptime.w_month <= 0) {			/* fix year wrap */
	zaptime.w_month += 12;
	zaptime.w_year -= 1;
    }

    if (deleteonly == 0) {

#ifdef BSD4.1c
	if ((dir = opendir(ARCHDIR)) == NULL) {
	    printf("archiver: no achive directory");
	    exit(BAD);
	}
	closedir(dir);
#else
	x ((i = open(ARCHDIR, 0)) < 0, "archiver: no achive directory");
	close(i);
#endif BSD4.1c

	sprintf(line, "%s/%s", ARCHDIR, nfname);
#ifdef BSD4.1c
	if ((dir = opendir(line)) == NULL) {
#else
	if ((i = open(line, 0)) < 0) {
#endif BSD4.1c

	    sprintf(line, "%s/%s", ARCHDIR, nfname);
	    dounix(0, 0, "/bin/mkdir", line, 0, 0, 0); /* build directory */
	} else {
#ifdef BSD4.1c
	    closedir(dir);				/* close up */
#else
	    close (i);					/* close up */
#endif BSD4.1c
	}

	sprintf(line, "%s/%s/%s", ARCHDIR, nfname, archfile);
#ifdef BSD4.1c
	if ((archfid = open(line, O_RDONLY, 0)) >= 0) {
#else
	if ((archfid = open(line, 0)) >= 0) {
#endif BSD4.1c
	    printf("Archive of %s too recent, try again in a minute\n",nfname);
	    close(archfid);
	    return(-1);					/* did not archive */
	}
	x ((farchfile = fopen(line, "w")) == NULL, "archiver: bad create");
    }

    lock(&io, 'n');				/* would be a good idea */

#ifdef OLDGROUP

    /* delete inactive groups - RLS 1/8/83 */

    sprintf(line,"%s/%s",MSTDIR,nfname);
    stat(line,&buf);
    current = time(0);
    if (current - buf.st_mtime > 60*60*24*(OLDGROUP-daysold)) {

	unlock(&io, 'n');
	finish(&io);

	sprintf(line,"/bin/rm -rf %s/%s",MSTDIR,nfname);
	system(line);

	gettime(&zaptime);
	sprdate(&zaptime, timeline);

	/* message in nfmaint */
	sprintf(line,"Archiver: removed %s\n",nfname);
        nfcomment(NOSUCHWARN, line, line, 0, 0);

	glock(&io, LOGLOCK);				/* make log entry */
	sprintf(line, "%s/%s/%s", MSTDIR, UTILITY, NETLOG);
	x ((log = fopen(line, "a")) == NULL, "archiver: no logfile");
	fprintf(log,"Archiver: deleted %s at %s\n", nfname, timeline);
	printf("Archiver: deleted %s at %s\n", nfname, timeline);
	fclose(log);
	gunlock(&io, LOGLOCK);
	return(0);
    }
#endif

    /*
     * 	this open races with above check - can archive twice! 
     */
    getdscr(&io, &io.descr);
    ncount = 0;				/* count of notes archived */
    for (i = 1; i <= io.descr.d_nnote; i++) {
	getnrec(&io, i, &note);
	if (note.n_stat & DELETED) {
	    continue;			/* never archive deleted notes */
	}
	if (dirmsgflag == DIROFF && (note.n_stat & DIRMES)) {
	    continue;					/* don't if dir on */
	}
	if (dirmsgflag == DIRON && (note.n_stat & DIRMES) == 0) {
	    continue;					/* don't if dir off */
	}
	if (inorder(&zaptime, &note.n_lmod)) {
	    continue;				/* touched too recently */
	}
	if (deleteonly == 0) {
	    dmpnote(&io, &note, i, farchfile, NODETAIL);
							/* save base note */
	    dmprall(&io, &note, i, farchfile, NODETAIL);
							/* save resps */
	}
	delnote(&io, i, NOLOCKIT);			/* delete entry */
	ncount++;
    }

    if (deleteonly == 0)
    	x (fclose (farchfile) < 0, "archiver: close archive");

    /* dont' compress if the group is empty, saves time and allows auto
     * group expiration
     */
    sprintf(line,"%s/%s/text",MSTDIR,nfname);
    stat(line,&buf);
    if (buf.st_size > 4) {
	int nnotes, nresps;

	if (compress(&io, NOLOCKIT, &nnotes, &nresps) >= 0)
		printf("%s: %d notes and %d responses after compression.\n",
			nfname, nnotes, nresps);
	else
		printf("%s: notesfile already compressed\n", nfname);
    }

    unlock(&io, 'n');				/* all done with this */
    finish(&io);				/* and close the notesfile */

    /* fix by RLS (8/19/82) */
    gettime(&zaptime);
    sprdate(&zaptime, timeline);

    if (ncount) {				/* log only if did somethine */
	glock(&io, LOGLOCK);				/* make log entry */
	sprintf(line, "%s/%s/%s", MSTDIR, UTILITY, NETLOG);
	x ((log = fopen(line, "a")) == NULL, "archiver: no logfile");
	if (deleteonly == 0) {
	    fprintf(log, "%s: archived %d notes into %s at %s\n",
		    nfname, ncount, archfile, timeline);
	} else {
	    fprintf(log, "%s: Archiver deleted %d notes at %s\n",
		    nfname, ncount, timeline);
	}

	fclose(log);

	gunlock(&io, LOGLOCK);
    }

    if (deleteonly == 0) {
	printf("Archived: %s: %d notes into file %s at %s\n",
		nfname, ncount, archfile, timeline);
    } else {
	printf("Archiver deleted from %s %d notes at %s\n",
		nfname, ncount, timeline);
    }
    return(0);						/* and return */
}
