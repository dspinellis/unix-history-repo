static char *sccsid = "@(#)delete.c	1.2 2/2/83";

#include "parms.h"
#include "structs.h"
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
mdelete (io, first, last)
struct io_f *io;
{
    int     which;

    lock(io, 'n');

    x (first > last, "mdelete: nonsense first/last");
    getdscr (io, &io->descr);
    if (last > io->descr.d_nnote) {
	last = io->descr.d_nnote;
    }
    if (first > io->descr.d_nnote) {
	first = io->descr.d_nnote;
    }
    for (which = first; which <= last; which++) {
	delnote (io, which, NOLOCKIT);
    }

    unlock(io, 'n');

    return(0);
}
