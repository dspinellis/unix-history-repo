static char *sccsid = "@(#)permit.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"

/*
 *	this package contains routines which tell whether a person is
 *	allowed various priviledges in the current notefile.
 *
 *	It is assumed that the notefile descriptor in the notefile base
 *	is up-to-date. No disk I/O is performed to get a more recent copy
 *	of the descriptor.
 *
 *	original author : Ray Essick may 29, 1981
 *	Modified:	Ray Essick	November 17, 1981
 *	Modified:	Ray Essick	January	1982
 *
 */

allow (io, mode)
struct io_f *io;
{
    if (globuid == NOTESUID) {				/* "owner" does all */
	return(1);
    }
    switch (mode) {

	case RESPOK: 

	    return(io->access & (RESPOK + WRITOK + DRCTOK));
	    break;

	case READOK: 

	    return(io->access & (READOK + DRCTOK));
	    break;

	case WRITOK: 

	    return(io->access & (WRITOK + DRCTOK));
	    break;

	case DRCTOK: 

	    return(io->access & (DRCTOK));
	    break;

	default: 

	    x (1, "permit: bad mode");

    }
    return(0);						/* to please lint */
}
