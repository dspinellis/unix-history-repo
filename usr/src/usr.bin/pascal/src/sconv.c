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
static char sccsid[] = "@(#)sconv.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

    /*
     *	functions to help pi put out
     *	polish postfix binary portable c compiler intermediate code
     *	thereby becoming the portable pascal compiler
     */

#include	"whoami.h"
#ifdef PC
#include	"0.h"
#include	<pcc.h>

    /*
     *	this routine enforces ``the usual arithmetic conversions''
     *	all integral operands are converted to ints.
     *	if either operand is a double, both are made to be double.
     *	this routine takes struct nl *'s for the types,
     *	and returns both the struct nl * and the p2type for the result.
     */
tuac(thistype, thattype, resulttypep, resultp2typep)
    struct nl	*thistype;
    struct nl	*thattype;
    struct nl	**resulttypep;
    int		*resultp2typep;
{
    int		thisp2type = p2type(thistype);
    int		thatp2type = p2type(thattype);

    *resulttypep = thistype;
    *resultp2typep = thisp2type;
	/*
	 *	should only be passed scalars
	 */
    if (isnta(thistype,"sbcid") || isnta(thattype,"sbcid")) {
	return;
    }
    if (thisp2type == PCCT_CHAR || thisp2type == PCCT_SHORT) {
	*resultp2typep = PCCT_INT;
	*resulttypep = nl + T4INT;
    }
    if (*resultp2typep == PCCT_INT && thatp2type == PCCT_DOUBLE) {
	*resultp2typep = PCCT_DOUBLE;
	*resulttypep = nl + TDOUBLE;
    }
    sconv(thisp2type, *resultp2typep);
}
    
    /*
     *	this routine will emit sconv operators when it thinks they are needed.
     *	this is code generator specific, rather than machine-specific.
     *	this routine takes p2types for arguments, not struct nl *'s.
     */
#if defined(vax) || defined(tahoe)
    /*
     *	the vax code genrator is very good, this routine is extremely boring.
     */
sconv(fromp2type, top2type)
    int	fromp2type;
    int	top2type;
{

    switch (top2type) {
	case PCCT_CHAR:
	case PCCT_SHORT:
	case PCCT_INT:
	    switch (fromp2type) {
		case PCCT_CHAR:
		case PCCT_SHORT:
		case PCCT_INT:
		case PCCT_DOUBLE:
			return;	/* pass1 knows how to do these */
		default:
			return;
	    }
	case PCCT_DOUBLE:
	    switch (fromp2type) {
		case PCCT_CHAR:
		case PCCT_SHORT:
		case PCCT_INT:
			putop(PCC_SCONV, PCCT_DOUBLE);
			return;
		case PCCT_DOUBLE:
			return;
		default:
			return;
	    }
	default:
		return;
    }
}
#endif vax || tahoe
#ifdef mc68000
    /*
     *	i don't know how much to trust the mc68000 compiler,
     *	so this routine is full.
     */
sconv(fromp2type, top2type)
    int	fromp2type;
    int	top2type;
{

    switch (top2type) {
	case PCCT_CHAR:
	    switch (fromp2type) {
		case PCCT_CHAR:
			return;
		case PCCT_SHORT:
		case PCCT_INT:
		case PCCT_DOUBLE:
			putop(PCC_SCONV, PCCT_CHAR);
			return;
		default:
			return;
	    }
	case PCCT_SHORT:
	    switch (fromp2type) {
		case PCCT_SHORT:
			return;
		case PCCT_CHAR:
		case PCCT_INT:
		case PCCT_DOUBLE:
			putop(PCC_SCONV, PCCT_SHORT);
			return;
		default:
			return;
	    }
	case PCCT_INT:
	    switch (fromp2type) {
		case PCCT_INT:
			return;
		case PCCT_CHAR:
		case PCCT_SHORT:
		case PCCT_DOUBLE:
			putop(PCC_SCONV, PCCT_INT);
			return;
		default:
			return;
	    }
	case PCCT_DOUBLE:
	    switch (fromp2type) {
		case PCCT_DOUBLE:
			return;
		case PCCT_CHAR:
		case PCCT_SHORT:
		case PCCT_INT:
			putop(PCC_SCONV, PCCT_DOUBLE);
			return;
		default:
			return;
	    }
	default:
		return;
    }
}
#endif mc68000
#endif PC
