/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)tmps.c	5.1 (Berkeley) %G%";
#endif not lint

#include "whoami.h"
#include "0.h"
#include "objfmt.h"
#ifdef PC
#   include "pc.h"
#endif PC
#include "align.h"
#include "tmps.h"

/*
 * This routine defines the register allocation strategy
 * All temporaries are allocated here, and this routines decides
 * where they are to be put.
 */
#ifdef PC
    /*
     *	register temporaries
     *	- are allocated from highreg towards lowreg.
     *	- are of size regsize.
     *	- register numbers from the various register types are mapped to 
     *	  integer register numbers using the offsets.  (cf. pcc/mac2defs)
     *
     *	stack temporaries
     *	- are allocated on a downward growing stack.
     */

#ifdef vax
    /*
     *	first pass register declaration constants
     */
struct	regtype {
    long	lowreg;
    long	highreg;
    long	regsize;
} regtypes[NUMREGTYPES] = {
	{ 6, 11, 4 },		/* r6..r11 */
};
#endif vax

#ifdef mc68000
    /*
     *	first pass register declaration constants
     */
struct	regtype {
    long	lowreg;
    long	highreg;
    long	regsize;
} regtypes[NUMREGTYPES] = {
	{ 2, 7, 4 },		/* d2..d7 */
	{ 2, 5, 4 },		/* a2..a5 */
};
#endif mc68000
#endif PC

tmpinit(cbn)
	int	cbn;
{
	struct om	*sizesp = &sizes[cbn];
#	ifdef PC
	int	i;
#	endif PC

	sizesp->om_max = -DPOFF1;
	sizesp->curtmps.om_off = -DPOFF1;
#	ifdef PC
		for (i = 0; i < NUMREGTYPES; i++) {
			sizesp->low_water[i] = regtypes[i].highreg + 1;
			sizesp->curtmps.next_avail[i] = regtypes[i].highreg;
		}
#	endif PC
}

/*
 * allocate runtime temporary variables
 */
/*ARGSUSED*/
struct nl *
tmpalloc(size, type, mode)
	long size;
	struct nl *type;
	int mode;
{
	register struct om	*op = &sizes[ cbn ];
	register int		offset;
	register struct nl	*nlp;
	long			alignment;

#	ifdef PC
#	    ifdef vax
		if (  mode == REGOK
		   && size == regtypes[REG_GENERAL].regsize
		   && op->curtmps.next_avail[REG_GENERAL]
			    >= regtypes[REG_GENERAL].lowreg) {
			offset = op->curtmps.next_avail[REG_GENERAL]--;
			if (offset < op->low_water[REG_GENERAL]) {
				op->low_water[REG_GENERAL] = offset;
			}
			nlp -> extra_flags = NLOCAL | NREGVAR;
			putlbracket(ftnno, op);
			return nlp;
		}
#	    endif vax
#	    ifdef mc68000
		if (  mode == REGOK
		   && type != nl + TPTR
		   && size == regtypes[REG_DATA].regsize
		   && op->curtmps.next_avail[REG_DATA]
			    >= regtypes[REG_DATA].lowreg) {
			offset = op->curtmps.next_avail[REG_DATA]--;
			if (offset < op->low_water[REG_DATA]) {
				op->low_water[REG_DATA] = offset;
			}
			nlp = defnl(0, VAR, type, offset + DATA_REG_OFFSET );
			nlp -> extra_flags = NLOCAL | NREGVAR;
			putlbracket(ftnno, op);
			return nlp;
		}
		if (  mode == REGOK
		   && type == nl + TPTR
		   && size == regtypes[REG_ADDR].regsize
		   && op->curtmps.next_avail[REG_ADDR]
			    >= regtypes[REG_ADDR].lowreg) {
			offset = op->curtmps.next_avail[REG_ADDR]--;
			if (offset < op->low_water[REG_ADDR]) {
				op->low_water[REG_ADDR] = offset;
			}
			nlp = defnl(0, VAR, type, offset + ADDR_REG_OFFSET );
			nlp -> extra_flags = NLOCAL | NREGVAR;
			putlbracket(ftnno, op);
			return nlp;
		}
#	    endif mc68000
#	endif PC
	if (type == NIL) {
	    alignment = A_STACK;
	} else if (type == nl+TPTR) {
	    alignment = A_POINT;
	} else {
	    alignment = align(type);
	}
        op->curtmps.om_off =
	    roundup((int)(op->curtmps.om_off - size), alignment);
	offset = op->curtmps.om_off;
	if ( offset < op->om_max ) {
	        op->om_max = offset;
	}
	nlp = defnl( (char *) 0 , VAR , type , offset );
#	ifdef PC
	    nlp -> extra_flags = NLOCAL;
	    putlbracket(ftnno, op);
#	endif PC
	return nlp;
}

/*
 * deallocate runtime temporary variables
 */
/*ARGSUSED*/
tmpfree(restore)
    register struct tmps	*restore;
{
#   ifdef PC
    register struct om		*op = &sizes[ cbn ];
    bool			change = FALSE;

#	ifdef vax
	    if (restore->next_avail[REG_GENERAL]
		> op->curtmps.next_avail[REG_GENERAL]) {
		    op->curtmps.next_avail[REG_GENERAL]
			= restore->next_avail[REG_GENERAL];
		    change = TRUE;
	    }
#	endif vax
#	ifdef mc68000
	    if (restore->next_avail[REG_DATA]
		> op->curtmps.next_avail[REG_DATA]) {
		    op->curtmps.next_avail[REG_DATA]
			= restore->next_avail[REG_DATA];
		    change = TRUE;
	    }
	    if (restore->next_avail[REG_ADDR]
		> op->curtmps.next_avail[REG_ADDR]) {
		    op->curtmps.next_avail[REG_ADDR]
			= restore->next_avail[REG_ADDR];
		    change = TRUE;
	    }
#	endif mc68000
    if (restore->om_off > op->curtmps.om_off) {
	    op->curtmps.om_off = restore->om_off;
	    change = TRUE;
    }
	if (change) {
	    putlbracket(ftnno, op);
	}
#endif PC
}

#ifdef PC
#ifdef vax
/*
 * create a save mask for registers which have been used
 * in this level
 */
savmask()
{
	int mask;
	int i;

	mask = RSAVEMASK;
	if (opt('t'))
	        mask |= RUNCHECK;
	for (i = 0; i <= regtypes[REG_GENERAL].highreg; i++) {
	    if (i >= sizes[cbn].low_water[REG_GENERAL]) {
		mask |= 1 << i;
	    }
	}
	return mask;
}
#endif vax
#endif PC
