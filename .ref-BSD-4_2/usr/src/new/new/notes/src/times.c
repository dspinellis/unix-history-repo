static char *sccsid = "@(#)times.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
/*
 *	getlast(atime, nf, seqon, name)
 *	struct when_f *atime; char *nf, *name
 *		retrieve last time that user was in the notefile.
 *	fixlast(atime, nf, seqon, name)
 *	struct when_f *atime; char *nf, *name
 *		update users last access time.
 *
 *	Restructure of Original effort (Nov 1981 by R. Essick)
 *	Coding:	Ray Essick	January 1982
 */

long lseek ();						/* make sure gets right type */

getlast (atime, nf, seqon, name)
struct when_f  *atime;
char   *nf,
       *name;
{
    FILE * seqfile;
    FILE * fopen ();
    struct seq_f    entry;
    register int    retcode;
    char    fn[WDLEN];

    atime->w_year = 1970;				/* initialize to ancient history */
    atime->w_month = 1;
    atime->w_day = 1;
    atime->w_hours = 0;
    atime->w_mins = 0;

    if (seqon) {		/* grab entry only if the sequencer is on */
	sprintf (fn, "%s/%s/%s", MSTDIR, SEQUENCER, name);
	if ((seqfile = fopen (fn, "r")) == NULL) {
	    return(-1);			/* no file, return default */
	}
	do {
	    retcode = fread (&entry, sizeof entry, 1, seqfile);
	} while (retcode && strcmp (entry.nfname, nf) != 0);

	fclose (seqfile);				/* close the file */

	if (strcmp (entry.nfname, nf) == 0) {
	    copydate (&entry.lastin, atime);		/* give him time */
	    return(0);
	} else {
	    return(-1);
	}
    }
    return(0);
}

fixlast (atime, nf, seqon, name)
struct when_f  *atime;
char   *nf;
int    seqon;
char   *name;
{
    struct seq_f    entry;
    register int    fid;				/* file descriptor */
    register int    atend;
    char    fn[WDLEN];

    if (seqon) {
	sprintf (fn, "%s/%s/%s", MSTDIR, SEQUENCER, name);

#ifdef BSD4.1c
	if ((fid = open(fn, O_CREAT|O_RDWR, 0666)) < 0) {
	    printf("fixlast: could not open sequencer file\n");
	    exit(BAD);
	}
#else
	if ((fid = open(fn, 2)) < 0) {			/* open up */
	    x ((fid = creat(fn, 0666)) < 0, "fixlast: create");
	    x (close(fid) < 0, "fixlast: close I");
	    x ((fid = open(fn, 2)) < 0, "fixlast: open write");
	}
#endif BSD4.1c


	while ((atend = read(fid, &entry, sizeof(entry))) == sizeof(entry)) {
	    if (strcmp (entry.nfname, nf) == 0) {
		break;					/* found him */
	    }
	}

	x (atend < 0, "fixlast: read error");
	if (atend == sizeof(entry)) {			/* not at the end */ 
	    x (lseek (fid, -((long) sizeof(entry)), 1) < 0, "fixlast: reseek");
	} else {
	    strmove(nf, entry.nfname);			/* build the entry */
	    x (lseek(fid, 0L, 2) < 0, "fixlast: EOF seek");
							/* make sure at end */
	}
	copydate(atime, &entry.lastin);
	x (write(fid, &entry, sizeof(entry)) != sizeof(entry), "fixlast: write");
	x (close(fid) < 0, "fixlast: close II");
    }
}
