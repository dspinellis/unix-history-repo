/* Copyright (c) 1983 Regents of the University of California */

static	char sccsid[] = "@(#)sconv.c 1.2 %G%";

    /*
     *	functions to help pi put out
     *	polish postfix binary portable c compiler intermediate code
     *	thereby becoming the portable pascal compiler
     */

#include	"whoami.h"
#ifdef PC
#include	"0.h"
#include	"pcops.h"

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
    if (thisp2type == P2CHAR || thisp2type == P2SHORT) {
	*resultp2typep = P2INT;
	*resulttypep = nl + T4INT;
    }
    if (*resultp2typep == P2INT && thatp2type == P2DOUBLE) {
	*resultp2typep = P2DOUBLE;
	*resulttypep = nl + TDOUBLE;
    }
    sconv(thisp2type, *resultp2typep);
}
    
    /*
     *	this routine will emit sconv operators when it thinks they are needed.
     *	this is code generator specific, rather than machine-specific.
     *	this routine takes p2types for arguments, not struct nl *'s.
     */
#ifdef vax
    /*
     *	the vax code genrator is very good, this routine is extremely boring.
     */
sconv(fromp2type, top2type)
    int	fromp2type;
    int	top2type;
{

    switch (top2type) {
	case P2CHAR:
	case P2SHORT:
	case P2INT:
	    switch (fromp2type) {
		case P2CHAR:
		case P2SHORT:
		case P2INT:
		case P2DOUBLE:
			return;	/* pass1 knows how to do these */
		default:
			return;
	    }
	case P2DOUBLE:
	    switch (fromp2type) {
		case P2CHAR:
		case P2SHORT:
		case P2INT:
			putop(P2SCONV, P2DOUBLE);
			return;
		case P2DOUBLE:
			return;
		default:
			return;
	    }
	default:
		return;
    }
}
#endif vax
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
	case P2CHAR:
	    switch (fromp2type) {
		case P2CHAR:
			return;
		case P2SHORT:
		case P2INT:
		case P2DOUBLE:
			putop(P2SCONV, P2CHAR);
			return;
		default:
			return;
	    }
	case P2SHORT:
	    switch (fromp2type) {
		case P2SHORT:
			return;
		case P2CHAR:
		case P2INT:
		case P2DOUBLE:
			putop(P2SCONV, P2SHORT);
			return;
		default:
			return;
	    }
	case P2INT:
	    switch (fromp2type) {
		case P2INT:
			return;
		case P2CHAR:
		case P2SHORT:
		case P2DOUBLE:
			putop(P2SCONV, P2INT);
			return;
		default:
			return;
	    }
	case P2DOUBLE:
	    switch (fromp2type) {
		case P2DOUBLE:
			return;
		case P2CHAR:
		case P2SHORT:
		case P2INT:
			putop(P2SCONV, P2DOUBLE);
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
