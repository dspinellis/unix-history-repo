/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)tmps.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

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

#ifdef tahoe
    /*
     *	first pass register declaration constants
     */
struct	regtype {
    long	lowreg;
    long	highreg;
    long	regsize;
} regtypes[NUMREGTYPES] = {
	{ 6, 12, 4 },		/* r6..r12 */
};
#endif tahoe

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
#	    if defined(vax) || defined(tahoe)
		if (  mode == REGOK
		   && size == regtypes[REG_GENERAL].regsize
		   && op->curtmps.next_avail[REG_GENERAL]
			    >= regtypes[REG_GENERAL].lowreg) {
			offset = op->curtmps.next_avail[REG_GENERAL]--;
			if (offset < op->low_water[REG_GENERAL]) {
				op->low_water[REG_GENERAL] = offset;
			}
			nlp = defnl( (char *) 0 , VAR , type , offset );
			nlp -> extra_flags = NLOCAL | NREGVAR;
			putlbracket(ftnno, op);
			return nlp;
		}
#	    endif vax || tahoe
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

#	if defined(vax) || defined(tahoe)
	    if (restore->next_avail[REG_GENERAL]
		> op->curtmps.next_avail[REG_GENERAL]) {
		    op->curtmps.next_avail[REG_GENERAL]
			= restore->next_avail[REG_GENERAL];
		    change = TRUE;
	    }
#	endif vax || tahoe
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
#if defined(vax) || defined(tahoe)
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
#endif vax || tahoe
#endif PC
