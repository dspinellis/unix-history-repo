#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: next.c,v 1.7 85/01/18 15:23:59 notes Rel $";
#endif	RCSIDENT

/*
 *	next.c -	find the next note/response after a certain time
 *
 *	nxtnote(io, note,date)
 *		finds the next note whith number > note which has been 
 *	modified after date. If none exists, a -1 is returned, otherwise
 *	the note number is returned.
 *
 *	nxtresp(io,note,resp,date)
 *		same idea as above, except, we look through the responses 
 *	after resp to note # note. If there is no response, we return 
 *	a -1, else we return the response number.
 *
 *	inorder(date1, date2) struct when_f *date1, *date2)
 *		return 1 if date1 before date2, otherwise return 0.
 *
 *	Original coding:	Ray Essick	november 1981
 */

nxtnote (io, note, date)
struct io_f *io;
struct when_f  *date;
{
    struct note_f   znote;
    if (note < 0)
	note = 0;					/* start at beginning */

    note++;						/* find starts with next note ! */
    while (note <= io -> descr.d_nnote)
    {
	getnrec (io, note, &znote);			/* get note header */
	if (znote.n_stat & DELETED)
	{
	    note++;					/* go on to the next note */
	    continue;					/* try the next note */
	}
	if (inorder (&znote.n_lmod, date) == 0)		/* want eqauls to show */
	    return note;				/* modified after date */
	note++;						/* go try the next note */
    }
    return (-1);					/* no deal, wasn't a more recent note */
}

nxtresp (io, note, resp, date)
struct io_f *io;
struct when_f  *date;
{
    struct note_f   znote;
    struct resp_f   rrec;
    int     poffset,
            recnum;

    getnrec (io, note, &znote);
    if (resp < 0)
	resp = 0;					/* can't look at negative response */
    resp++;						/* start search at next response */
    while (resp <= znote.n_nresp)
    {
	if (lrsp (io, note, resp, &rrec, &poffset, &recnum) == -1)
	    break;					/* no response, drop out */
	if (inorder (&rrec.r_rcvd[poffset], date) == 0)
	    return resp;				/* return if date earlier or same */
	resp++;
    }
    return (-1);					/* no later responses to this note */
}

/*
 *	see if the time specified by "d1" is before "d2". 
 *	this is a STRICT test.  if d1 == d2, they are not
 *	in order.
 */
inorder (d1, d2)
struct when_f  *d1,
               *d2;
{
    if (d1 -> w_gmttime && d2 -> w_gmttime)		/* if both there */
	return (d1 -> w_gmttime < d2 -> w_gmttime);	/* relationship */
    if (d1 -> w_year < d2 -> w_year)
	return 1;
    if (d1 -> w_year > d2 -> w_year)
	return 0;

    if (d1 -> w_month < d2 -> w_month)
	return 1;
    if (d1 -> w_month > d2 -> w_month)
	return 0;

    if (d1 -> w_day < d2 -> w_day)
	return 1;
    if (d1 -> w_day > d2 -> w_day)
	return 0;

    if (d1 -> w_hours < d2 -> w_hours)
	return 1;
    if (d1 -> w_hours > d2 -> w_hours)
	return 0;

    if (d1 -> w_mins < d2 -> w_mins)
	return 1;
    if (d1 -> w_mins > d2 -> w_mins)
	return 0;

    return 0;						/* equal dates are not in order */
}
