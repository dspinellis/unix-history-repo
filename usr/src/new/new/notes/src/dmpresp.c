static char *sccsid = "@(#)dmpresp.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
/*
 *	dmprsp(io, note, notenum, dumpfid, response number)
 *	prints logical response number to the note whose number is
 *	passed in. The note descriptor is also passed in. The dump if
 *	sent to the file pointed to by dumpfid
 *
 *	Original Coding:	Ray Essick December 1981
 *
 */

dmprsp (io, note, notenum, dmpfile, num, extensive)
struct io_f *io;
struct note_f  *note;
FILE * dmpfile;
{
    int     roffset,
            rrecnum;
    struct resp_f   rsprec;
    struct when_f  *zdate;
    struct txthead_f    txthead;

    if (lrsp (io, notenum, num, &rsprec, &roffset, &rrecnum) == -1) {
	return;						/* no response */
    }
    fprintf (dmpfile, "R:%s:%ld:%s:%ld:%d\n", note->n_id.sys,
	    note->n_id.uniqid, rsprec.r_id[roffset].sys,
	    rsprec.r_id[roffset].uniqid, num);

    fprintf (dmpfile, "%s:%d\n", rsprec.r_auth[roffset].aname, rsprec.r_auth[roffset].aid & UIDMASK);

    zdate = &rsprec.r_when[roffset];
    fprintf (dmpfile, "%d:%d:%d:%d:%d\n", zdate->w_year, zdate->w_month,
	    zdate->w_day, zdate->w_hours, zdate->w_mins);
    if (extensive) {
	zdate = &rsprec.r_rcvd[roffset];
	fprintf (dmpfile, "%d:%d:%d:%d:%d\n", zdate->w_year, zdate->w_month,
		zdate->w_day, zdate->w_hours, zdate->w_mins);

	fprintf (dmpfile, "%s\n", rsprec.r_from[roffset]);
    }


    gethrec (io, &rsprec.r_addr[roffset], &txthead);    /* get the body */

    fprintf (dmpfile, "%03o:%d\n", rsprec.r_stat[roffset], txthead.textlen);

    pageout (io, &rsprec.r_addr[roffset], dmpfile);

}

/*	dmpall - dump all the responses to a note.  Merely calls
 *	dmpresp repetitively to dump all of them 
 *
 *	Original Coding:	Ray Essick	December 1981
 */
dmprall (io, note, notenum, dmpfile, extensive)
struct io_f *io;
struct note_f  *note;
FILE * dmpfile;
{
    int     num;

    for (num = 1; num <= note->n_nresp; num++) {
	dmprsp (io, note, notenum, dmpfile, num, extensive);
    }
}
