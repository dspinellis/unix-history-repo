static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"
#include "globs.h"

/*
 *	This program will initialize an empty notefile. It leaves the
 *	caller as sole director of the notefile and also as the only
 *	person with access to the notefile. 
 *
 *	Since a notefile does suck up a little disk space, the use of
 *	this program is limited to the user whose uid matches the
 *	NOTESUID constant in the file structs.h 
 *
 *	Original coding:	Rob Kolstad	Winter 1980
 *	Modified:		Ray Essick	November 1981
 */

main (argc, argv) char **argv;			/* create a new notesfile */
{

    struct io_f io;
    char   *p;						/* misc. pointer */
    int     k;						/* arg counter */
    int     j,
            Aflag,
            Oflag,
            Nflag;					/* option flags */
    int     i;						/* misc counter */
    int     fid;					/* misc file id */
    struct daddr_f  freetext;				/* for writing */
    struct descr_f  descr;				/* for writing, too */
    struct perm_f   perms[NPERMS];
    struct perm_f   pentry;				/* single entry */
    struct auth_f   me;					/* for access list */
    char    cmdline[CMDLEN];				/* for mkdir */
    FILE *seqfile, *fp;

#include "main.i"			/* common init code and such */

#ifndef AUTOCREATE
    if (globuid != NOTESUID) {
	printf("You are not allowed to build notefiles\n");
	exit(BAD);
    }
#endif

    if (argc == 1) {
	printf("Usage: %s [-aon] topic1 [...]\n", argv[0]);
	exit(BAD);
    }

    Aflag = 0;
    Oflag = 0;
    Nflag = 0;

    for (k = 1; k < argc; k++) {

	if (argv[k][0] == '-') {			/* options!!! */
	    j = 1;
	    while (argv[k][j])
		switch (argv[k][j++]) {
		    case 'a': 				/* anon notes ok */
			Aflag = 1;
			break;

		    case 'o': 				/* open notesfile */
			Oflag = 1;
			break;

		    case 'n': 				/* network available */
			Nflag = 1;
			break;

		    default: 				/* bad news */
			fprintf(stderr, "Bad switch: `%c'\n", argv[k][--j]);
			exit(BAD);
		}
	    continue;			/* on to the next arguement */
	}

	if (chkpath(argv[k])) {
	    printf("Bad notefile name: %s\n", argv[k]);
	    continue;
	}

	setuid(NOTESUID);

	printf ("%s\n", argv[k]);			/* show progress */
	x (chdir(MSTDIR) < 0, "mknf:  Cant get to MSTDIR");
	if (chdir(argv[k]) == 0) {		/* check for existence */
	    printf("%s: notefile already exists\n", argv[k]);
	    continue;
	}

	x (chdir(MSTDIR) < 0, "mknf: bad getting to MSTDIR");

#ifdef BSD4.1c
	x(mkdir(argv[k], 0700) < 0, "mknf: can't make directory");
#else
	dounix(0, 0, "/bin/mkdir", argv[k], 0, 0, 0);
	dounix(0, 0, "/bin/chmod", "0700", argv[k], 0, 0);
#endif BSD4.1c

	x (chdir(argv[k]) < 0, "mknf: can't chdir");

	/* make the text file now */
	x ((fid = creat(TEXT, 0600)) < 0, "mknf:  Cant create text");
	freetext.addr = sizeof(freetext);   /* point after the address table */
	x (write(fid, &freetext, sizeof(freetext)) != sizeof(freetext), "mknf:  Cant write text");

	x (close(fid) < 0, "mknf: bad text close");
					/* make the note index now */
	glock(&io, SEQLOCK);		/* grab next available sequence # */
	sprintf(cmdline, "../%s", SEQ);
	x ((seqfile = fopen(cmdline, "r")) == NULL, "mknf:open sequence file");

	x (fscanf(seqfile, "%d", &i) != 1, "mknf:sequence file read");
	descr.d_nfnum = i++;

	fclose(seqfile);				/* close it and then */
	x ((seqfile = fopen(cmdline, "w")) == NULL,
		"mknf: reopen sequence file");

	fprintf(seqfile, "%d\n", i);
	fclose(seqfile);

	gunlock(&io, SEQLOCK);			/* release unique file */

	for (i = 0; (i < NNLEN) && (argv[k][i]); i++) {
	    descr.d_title[i] = argv[k][i];		/* default title */
	}
	for (; i < NNLEN; i++) {
	    descr.d_title[i] = ' ';			/* space fill */
	}
	for (i = 0, p = "** director message **"; i < DMLEN && *p; i++, p++) {
	    descr.d_drmes[i] = *p;			/* copy default over */
	}
	for (; i < DMLEN; i++) {
	    descr.d_drmes[i] = ' ';			/* space fill */
	}
	descr.d_plcy = 0;		/* no policy note at this time */
	descr.d_id.uniqid = 0;		/* unique id number within nf */
	strmove(SYSTEM, descr.d_id.sys);		/* and system name */

	gettime(&descr.d_lstxmit);	/* last network transmission */
	gettime(&descr.d_lastm);	/* last modified now */
	gettime(&descr.d_created);
	gettime(&descr.d_lastuse);	/* for days online */
	descr.d_daysused = 1;		/* count making it ! */
	descr.d_stat = 0;		/* no special status now */
	if (Aflag) {
	    descr.d_stat |= ANONOK;	/* he said wanted this */
	}
	if (Oflag) {
	    descr.d_stat |= OPEN;
	}
	if (Nflag) {
	    descr.d_stat |= NETWRKD;
	}
	descr.d_nnote = 0;		/* no notes in file */

	descr.d_rspwrit = descr.d_notwrit = 0;		/* initialize stats */
	descr.d_rspread = descr.d_notread = 0;
	descr.d_notdrop = descr.d_rspdrop = 0;
	descr.d_orphans = 0;
	descr.netwrkouts = descr.netwrkins = 0;
	descr.entries = 0;
	descr.walltime = 0;
	descr.d_rspxmit = descr.d_notxmit = 0;
	descr.d_rsprcvd = descr.d_notrcvd = 0;

	x ((fid = creat(INDEXN, 0600)) < 0, "mknf:  Cant create note.indx");
	x (write(fid, &descr, sizeof(descr)) != sizeof(descr), "mknf:  Cant write note.indx");
	x (close(fid) < 0, "mknf: bad note.indx close");
				/* now make the response index, it's easy */
	x ((fid = creat(INDEXR, 0600)) < 0, "mknf:  Cant create resp.indx");
	i = 0;			/* resp 0 slot free */
	x (write(fid, &i, sizeof(i)) != sizeof(i), "mknf:  Cant write resp.indx");
	x (close(fid) < 0, "mknf: bad close of resp.indx");

	x ((fid = creat(ACCESS, 0600)) < 0, "mknf: couldnt create access");
	getname (&me, 0);	/* grab name for access */
	perms[0].ptype = PUSER;
	strmove (me.aname, perms[0].name);
	perms[0].perms = READOK + WRITOK + RESPOK + DRCTOK;
	perms[1].ptype = PGROUP;
	strmove ("Other", perms[1].name);
	perms[1].perms = DFLTPERMS;
	perms[2].ptype = PSYSTEM;
	strmove ("Other", perms[2].name);
	perms[2].perms = DFLTPERMS;

	x (write(fid, perms, 3 * sizeof(pentry)) != 3 * sizeof(pentry), "mknf: bad access write");
	x (close(fid) < 0, "mknf: close access broke");

	/* update the available notes list */
	/* may want to lock this file */
	sprintf(cmdline,"%s/%s/%s",MSTDIR,UTILITY,AVAILHLP);

	if ((fp = fopen(cmdline,"a")) == NULL) {
	    fprintf(stderr,"Can't open %s\n",cmdline);
	    exit(-1);
	}
	fprintf(fp,"%s\n",argv[k]);
	fclose(fp);
    }
    exit (GOOD);
}
