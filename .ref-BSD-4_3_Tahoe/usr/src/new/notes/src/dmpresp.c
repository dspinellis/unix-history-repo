#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: dmpresp.c,v 1.7 85/01/18 15:09:02 notes Rel $";
#endif	RCSIDENT

/*
 *	dmprsp(io, note, notenum, dumpfid, response number)
 *	prints logical response number to the note whose number is
 *	passed in. The note descriptor is also passed in. The dump if
 *	sent to the file pointed to by dumpfid
 *
 *	Original Coding:	Ray Essick December 1981
 *
 */


dmprsp (io, note, notenum, dmpfile, num, extensive, protocol)
struct io_f *io;
struct note_f  *note;
FILE * dmpfile;
{
    int     roffset,
            rrecnum;
    int     i;
    struct resp_f   rsprec;
    struct when_f  *zdate;
    char    buf[256];					/* hold intermediate strings */

    if (lrsp (io, notenum, num, &rsprec, &roffset, &rrecnum) == -1)
	return;						/* no response */

    switch (protocol)
    {
	case 0: 					/* original protocol */
	    fprintf (dmpfile, "R:%s:%ld:%s:%ld:%d\n", note -> n_id.sys,
		    note -> n_id.uniqid, rsprec.r_id[roffset].sys,
		    rsprec.r_id[roffset].uniqid, num);

	    fprintf (dmpfile, "%s:%d:%s:\n", rsprec.r_auth[roffset].aname,
		    rsprec.r_auth[roffset].aid & UIDMASK,
		    rsprec.r_auth[roffset].asystem);

	    zdate = &rsprec.r_when[roffset];
	    fprintf (dmpfile, "%d:%d:%d:%d:%d:%ld:\n", zdate -> w_year, zdate -> w_month,
		    zdate -> w_day, zdate -> w_hours, zdate -> w_mins, zdate -> w_gmttime);
	    if (extensive)
	    {
		zdate = &rsprec.r_rcvd[roffset];
		fprintf (dmpfile, "%d:%d:%d:%d:%d:%ld:\n", zdate -> w_year, zdate -> w_month,
			zdate -> w_day, zdate -> w_hours, zdate -> w_mins, zdate -> w_gmttime);

		fprintf (dmpfile, "%s\n", rsprec.r_from[roffset]);
	    }



	    fprintf (dmpfile, "%03o:%ld\n", rsprec.r_stat[roffset],
		    ((long) rsprec.r_addr[roffset].textlen));/* make sure long */

	    pageout (io, &rsprec.r_addr[roffset], dmpfile);
	    break;

	case 1: 					/* protocol 1 */
	    fprintf (dmpfile, "Protocol: 1 Response\n");/* let it know */
	    fprintf (dmpfile, "Title: Re: %s\n", note -> ntitle);
	    fprintf (dmpfile, "Parent-ID: %s.%ld\n", note -> n_id.sys,
		    note -> n_id.uniqid);
	    fprintf (dmpfile, "Author: %s@%s\n",
		    rsprec.r_auth[roffset].aname, rsprec.r_auth[roffset].asystem);
	    if (extensive)
		fprintf (dmpfile, "Author-UID: %d\n", rsprec.r_auth[roffset].aid);
	    fprintf (dmpfile, "Response-ID: %s.%ld\n", rsprec.r_id[roffset].sys,
		    rsprec.r_id[roffset].uniqid);
	    sprdate (&rsprec.r_when[roffset], buf);	/* format date */
	    fprintf (dmpfile, "Date-Written: %s\n", buf);
	    if (extensive)				/* dump extra information */
	    {
		sprdate (&rsprec.r_rcvd[roffset], buf);
		fprintf (dmpfile, "Date-Received: %s\n", buf);
		fprintf (dmpfile, "Source-System: %s\n", rsprec.r_from[roffset]);
	    }
	    fprintf (dmpfile, "Status:");		/* do the bits */
	    if (rsprec.r_stat[roffset] & FRMNEWS)
		fprintf (dmpfile, " Thru-News");
	    if (rsprec.r_stat[roffset] & DIRMES)
		fprintf (dmpfile, " Director-Message");
	    if (rsprec.r_stat[roffset] & WRITONLY)
		fprintf (dmpfile, " Write-Only");
	    if (rsprec.r_stat[roffset] & ORPHND)
		fprintf (dmpfile, " Foster-Parent");
	    putc ('\n', dmpfile);
	    fprintf (dmpfile, "Text-Length: %ld bytes\n",
		    (long) rsprec.r_addr[roffset].textlen);
	    pageout (io, &rsprec.r_addr[roffset], dmpfile);/* dump text */
	    break;

	default: 
	    fprintf (stderr, "dmpresp: Unsupported Protocol (%d)\n",
		    protocol);
	    break;
    }

}

/*	dmpall - dump all the responses to a note.  Merely calls
 *	dmpresp repetitively to dump all of them 
 *
 *	Original Coding:	Ray Essick	December 1981
 */
dmprall (io, note, notenum, dmpfile, extensive, protocol)
struct io_f *io;
struct note_f  *note;
FILE * dmpfile;
{
    int     num;

    for (num = 1; num <= note -> n_nresp; num++)
    {
	dmprsp (io, note, notenum, dmpfile, num, extensive, protocol);
    }
}
