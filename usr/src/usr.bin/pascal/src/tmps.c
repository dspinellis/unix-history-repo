/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)tmps.c 1.4 %G%";

#include "whoami.h"
#include "0.h"
#ifdef PC
#   include "pc.h"
#endif PC

/*
 * This routine defines the register allocation strategy
 * All temporaries are allocated here, and this routines decides
 * where they are to be put.
 */
#ifdef PC
#ifdef VAX
#    define MAXREGS 6
#    define REGSIZ 4
#    define FIRSTREG 6
#else
#ifdef PDP11
#    define MAXREGS 3
#    define REGSIZ 2
#    define FIRSTREG 2
#else
#    define MAXREGS 0
#    define REGSIZ 0
#    define FIRSTREG 0
#endif PDP11
#endif VAX
#endif PC

/*
 * allocate runtime temporary variables
 */
long
tmpalloc(size, type, mode)
	long size;
	struct nl *type;
	int mode;
{
	register struct om *op = &sizes[ cbn ];
	register int offset;

#	ifdef PC
	    if (mode == REGOK && size <= REGSIZ &&
		op->curtmps.reg_off < MAXREGS) {
		    offset = op->curtmps.reg_off++;
		    if ( offset > op->reg_max ) {
			    op->reg_max = offset;
		    }
		    /*
		     * the register number is encoded as an odd negative number
		     * which can never appear as an address.
		     */
		    return -(((offset + FIRSTREG) << 1) + 1);
	    }
#	endif PC
	offset = op->curtmps.om_off -= leven( size );
	if ( offset < op->om_max ) {
	        op->om_max = offset;
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
	register struct tmps *restore;
{
	register struct om *op = &sizes[ cbn ];

#	ifdef PC
	    if (restore->reg_off < op->curtmps.reg_off) {
		    op->curtmps.reg_off = restore->reg_off;
	    }
#	endif PC
	if (restore->om_off > op->curtmps.om_off) {
		op->curtmps.om_off = restore->om_off;
#		ifdef PC
		    putlbracket( ftnno , -restore->om_off );
#		endif PC
	}
}

#ifdef PC
#ifdef VAX
/*
 * create a save mask for registers which have been used
 * in this level
 */
savmask()
{
	short mask;
	int i;

	mask = RSAVEMASK;
	if (opt('t'))
	        mask |= RUNCHECK;
	for (i = 0; i <= sizes[ cbn ].reg_max; i++)
		mask |= 1 << (FIRSTREG + i);
	return mask;
}
#endif VAX
#endif PC
