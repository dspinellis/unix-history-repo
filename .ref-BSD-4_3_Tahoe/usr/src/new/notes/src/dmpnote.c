#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: dmpnote.c,v 1.7 85/01/18 15:08:44 notes Rel $";
#endif	RCSIDENT

/*	dmpnote - dump a note in canonical form. This routine
 *	will take the note specified by the arguement and dump
 *	it in a canonical form..
 *
 *	fid - file to write the output to 
 *	note - the current descriptor for that note 
 *
 *	Original Coding:	Ray Essick	December 1981
 */


dmpnote (io, note, num, dmpfile, extensive, protocol)
struct io_f *io;
struct note_f  *note;
FILE * dmpfile;
{
    char    buf[256];					/* hold some strings */
    int     i;


    switch (protocol)
    {
	case 0: 					/* original */
	    fprintf (dmpfile, "N:%s:%ld:%d\n",
		    note -> n_id.sys, note -> n_id.uniqid, note -> n_nresp);
	    fprintf (dmpfile, "%s\n", note -> ntitle);

	    fprintf (dmpfile, "%s:%d:%s:\n", note -> n_auth.aname,
		    note -> n_auth.aid & UIDMASK, note -> n_auth.asystem);

	    fprintf (dmpfile, "%d:%d:%d:%d:%d:%ld:\n", note -> n_date.w_year, note -> n_date.w_month,
		    note -> n_date.w_day, note -> n_date.w_hours,
		    note -> n_date.w_mins, note -> n_date.w_gmttime);

	    if (extensive)
	    {
		fprintf (dmpfile, "%d:%d:%d:%d:%d:%ld:\n", note -> n_rcvd.w_year, note -> n_rcvd.w_month,
			note -> n_rcvd.w_day, note -> n_rcvd.w_hours,
			note -> n_rcvd.w_mins, note -> n_rcvd.w_gmttime);

		fprintf (dmpfile, "%d:%d:%d:%d:%d:%ld:\n", note -> n_lmod.w_year, note -> n_lmod.w_month,
			note -> n_lmod.w_day, note -> n_lmod.w_hours,
			note -> n_lmod.w_mins, note -> n_lmod.w_gmttime);
		fprintf (dmpfile, "%s\n", note -> n_from);/* dump who from */
	    }


	    if (note -> n_addr.addr == 0)		/* for orphans */
		note -> n_addr.textlen = 0;		/* used to not init */
	    fprintf (dmpfile, "%03o:%ld\n", note -> n_stat,
		    ((long) note -> n_addr.textlen));	/* make sure long */

	    pageout (io, &note -> n_addr, dmpfile);
	    break;

	case 1: 					/* protocol 1 */
	    fprintf (dmpfile, "Protocol: 1 Note\n");	/* for loadem */
	    fprintf (dmpfile, "Title: %s\n", note -> ntitle);
	    fprintf (dmpfile, "Author: %s@%s\n",
		    note -> n_auth.aname, note -> n_auth.asystem);
	    if (extensive)
		fprintf (dmpfile, "Author-UID: %d\n", note -> n_auth.aid);
	    fprintf (dmpfile, "Note-ID: %s.%ld\n", note -> n_id.sys,
		    note -> n_id.uniqid);
	    sprdate (&note -> n_date, buf);		/* format date */
	    fprintf (dmpfile, "Date-Written: %s\n", buf);
	    if (extensive)				/* dump extra information */
	    {
		sprdate (&note -> n_rcvd, buf);
		fprintf (dmpfile, "Date-Received: %s\n", buf);
		sprdate (&note -> n_lmod, buf);
		fprintf (dmpfile, "Date-Modified: %s\n", buf);
		fprintf (dmpfile, "Source-System: %s\n", note -> n_from);
	    }
	    fprintf (dmpfile, "Status:");		/* do the bits */
	    if (note -> n_stat & FRMNEWS)
		fprintf (dmpfile, " Thru-News");
	    if (note -> n_stat & DIRMES)
		fprintf (dmpfile, " Director-Message");
	    if (note -> n_stat & WRITONLY)
		fprintf (dmpfile, " Write-Only");
	    if (note -> n_stat & ORPHND)
		fprintf (dmpfile, " Foster-Parent");
	    putc ('\n', dmpfile);
	    if (note -> n_addr.addr == 0)		/* empty */
		note -> n_addr.textlen = 0;
	    fprintf (dmpfile, "Text-Length: %ld bytes\n",
		    (long) note -> n_addr.textlen);
	    pageout (io, &note -> n_addr, dmpfile);	/* dump text */
	    break;


	default: 
	    fprintf (stderr, "dmpnote: Unsupported Protocol (%d)\n",
		    protocol);
	    break;
    }

}
