/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)tmps.c 1.1 %G%";

#include "whoami.h"
#include "0.h"

/*
 * allocate runtime temporary variables
 */
long
tmpalloc(size, type, loc)
	long size;
	struct nl *type;
	int loc;
{
	long offset;

	offset = sizes[ cbn ].curtmps.om_off -= size;
	if ( offset < sizes[ cbn ].om_max ) {
	    sizes[ cbn ].om_max = offset;
	}
#	ifdef PC
	    putlbracket( ftnno , -offset );
#	endif PC
	return offset;
}

/*
 * deallocate runtime temporary variables
 */
tmpfree(restore)
	struct tmps *restore;
{
	long offset;

	offset = restore->om_off;
	if (offset > sizes[cbn].curtmps.om_off) {
		sizes[cbn].curtmps.om_off = offset;
#		ifdef PC
		    putlbracket( ftnno , -offset );
#		endif PC
	}
}
