/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)tmps.c 1.8 %G%";

#include "whoami.h"
#include "0.h"
#include "objfmt.h"
#ifdef PC
#   include "pc.h"
#endif PC
#include "tmps.h"

/*
 * This routine defines the register allocation strategy
 * All temporaries are allocated here, and this routines decides
 * where they are to be put.
 */
#ifdef PC
    /*
     *	registers are allocated from highreg towards lowreg.
     *	registers are of size regsize.
     *	stack variables are allocated on a downward growing stack.
     */

#ifdef vax
    /*
     *	first pass register declaration constants
     */
#   define	LONGREGTYPE	0
struct	regtype {
    long	lowreg;
    long	highreg;
    long	regsize;
} regtypes[NUMREGTYPES] = {
	{ 6, 11, 4 },
};
#endif vax

#ifdef mc68000
    /*
     *	first pass register declaration constants
     */
#   define	DATAREGTYPE	0
#   define	ADDRREGTYPE	1
struct	regtype {
    long	lowreg;
    long	highreg;
    long	regsize;
} regtypes[NUMREGTYPES] = {
	{ 2, 7, 0 },		/* off for now */
	{ 2, 5, 0 },		/* off for now */
};
#endif mc68000
#endif PC

tmpinit(cbn)
	int	cbn;
{
	struct om	*sizesp = &sizes[cbn];
	int	i;

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
struct nl *
tmpalloc(size, type, mode)
	long size;
	struct nl *type;
	int mode;
{
	register struct om	*op = &sizes[ cbn ];
	register int		offset;
	register struct nl	*nlp;

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
			nlp = defnl( 0 , VAR , type , offset );
			nlp -> extra_flags = NLOCAL | NREGVAR;
			putlbracket(ftnno, op);
			return nlp;
		}
#	    endif vax
#	endif PC
        op->curtmps.om_off =
	    roundup((int)(op->curtmps.om_off - size), (long)align(type));
	offset = op->curtmps.om_off;
	if ( offset < op->om_max ) {
	        op->om_max = offset;
	}
	nlp = defnl( 0 , VAR , type , offset );
#	ifdef PC
	    nlp -> extra_flags = NLOCAL;
	    putlbracket(ftnno, op);
#	endif PC
	return nlp;
}

/*
 * deallocate runtime temporary variables
 */
tmpfree(restore)
    register struct tmps	*restore;
{
    register struct om		*op = &sizes[ cbn ];
    bool			change = FALSE;

#   ifdef PC
	    /* i think this never gives back storage!	... peter */
#	ifdef vax
	    if (restore->next_avail[REG_GENERAL]
		> op->curtmps.next_avail[REG_GENERAL]) {
		    op->curtmps.next_avail[REG_GENERAL]
			= restore->next_avail[REG_GENERAL];
		    change = TRUE;
	    }
#	endif vax
#   endif PC
    if (restore->om_off > op->curtmps.om_off) {
	    op->curtmps.om_off = restore->om_off;
	    change = TRUE;
    }
#   ifdef PC
	if (change) {
	    putlbracket(ftnno, op);
	}
#   endif PC
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
