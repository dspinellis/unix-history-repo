#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: delete.c,v 1.7 85/01/18 15:08:35 notes Rel $";
#endif	RCSIDENT

/*
 *	mdelete(io, first, last)
 *	deletes all the notes in the specified range in the notefile.
 *
 *	no verification of the desire to delete the notes is performed.
 *	It just goes and zaps them..
 *
 *	returns 0 if all goes well, -1 if it didn't like something
 *
 *	Original coding:	Ray Essick	Jan 1982
 */
mdelete (io, first, last, zapping)
struct io_f *io;
int     first,
        last,						/* range */
        zapping;					/* delete/undelete */
{
    int     which;
    struct note_f   note;				/* for undelete */

    x (first > last, "mdelete: nonsense first/last");
    locknf (io, DSCRLOCK);				/* lock for duration */
    getdscr (io, &io -> descr);
    if (last > io -> descr.d_nnote)
	last = io -> descr.d_nnote;
    if (first > io -> descr.d_nnote)
	first = io -> descr.d_nnote;
    for (which = first; which <= last; which++)
    {
	if (zapping)					/* deletion */
	{
	    delnote (io, which, NOLOCKIT);
	}
	else
	{
	    getnrec (io, which, &note);			/* get the note */
	    note.n_stat &= NOT DELETED;			/* un-deleted */
	    putnrec (io, which, &note);
	}
    }
    unlocknf (io, DSCRLOCK);				/* release the lock */
    return 0;
}
