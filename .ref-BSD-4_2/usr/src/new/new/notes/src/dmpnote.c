static char *sccsid = "@(#)dmpnote.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
/*	dmpnote - dump a note in canonical form. This routine
 *	will take the note specified by the arguement and dump
 *	it in a canonical form..
 *
 *	fid - file to write the output to 
 *	note - the current descriptor for that note 
 *
 *	Original Coding:	Ray Essick	December 1981
 */

dmpnote (io, note, num, dmpfile, extensive)
struct io_f *io;
struct note_f  *note;
FILE * dmpfile;
{
    struct txthead_f    txthead;


    fprintf (dmpfile, "N:%s:%ld:%d\n", note->n_id.sys, note->n_id.uniqid, note->n_nresp);
    fwrite (note->ntitle, TITLEN, 1, dmpfile);
    putc ('\n', dmpfile);

    fprintf (dmpfile, "%s:%d\n", note->n_auth.aname, note->n_auth.aid & UIDMASK);

    fprintf (dmpfile, "%d:%d:%d:%d:%d\n", note->n_date.w_year, note->n_date.w_month,
	    note->n_date.w_day, note->n_date.w_hours, note->n_date.w_mins);

    if (extensive) {
	fprintf (dmpfile, "%d:%d:%d:%d:%d\n", note->n_rcvd.w_year, note->n_rcvd.w_month,
		note->n_rcvd.w_day, note->n_rcvd.w_hours, note->n_rcvd.w_mins);

	fprintf (dmpfile, "%d:%d:%d:%d:%d\n", note->n_lmod.w_year, note->n_lmod.w_month,
		note->n_lmod.w_day, note->n_lmod.w_hours, note->n_lmod.w_mins);
	fprintf (dmpfile, "%s\n", note->n_from);      /* dump who from */
    }

    gethrec (io, &note->n_addr, &txthead);		/* get the body */

    fprintf (dmpfile, "%03o:%d\n", note->n_stat, txthead.textlen);

    pageout (io, &note->n_addr, dmpfile);

}
