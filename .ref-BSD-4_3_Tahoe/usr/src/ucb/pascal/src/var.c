/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)var.c	5.4 (Berkeley) 11/12/86";
#endif not lint

#include "whoami.h"
#include "0.h"
#include "objfmt.h"
#include "align.h"
#include "iorec.h"
#ifdef PC
#   include	"pc.h"
#endif PC
#include "tmps.h"
#include "tree_ty.h"

/*
 * Declare variables of a var part.  DPOFF1 is
 * the local variable storage for all prog/proc/func
 * modules aside from the block mark.  The total size
 * of all the local variables is entered into the
 * size array.
 */
/*ARGSUSED*/
varbeg( lineofyvar , r )
    int	lineofyvar;
{
    static bool	var_order = FALSE;
    static bool	var_seen = FALSE;

/* this allows for multiple declaration
 * parts except when the "standard"
 * option has been specified.
 * If routine segment is being compiled,
 * do level one processing.
 */

#ifndef PI1
	if (!progseen)
		level1();
	line = lineofyvar;
	if ( parts[ cbn ] & RPRT ) {
	    if ( opt( 's' ) ) {
		standard();
		error("Variable declarations should precede routine declarations");
	    } else {
		if ( !var_order ) {
		    var_order = TRUE;
		    warning();
		    error("Variable declarations should precede routine declarations");
		}
	    }
	}
	if ( parts[ cbn ] & VPRT ) {
	    if ( opt( 's' ) ) {
		standard();
		error("All variables should be declared in one var part");
	    } else {
		if ( !var_seen ) {
		    var_seen = TRUE;
		    warning();
		    error("All variables should be declared in one var part");
		}
	    }
	}
	parts[ cbn ] |= VPRT;
#endif
    /*
     *  #ifndef PI0
     *      sizes[cbn].om_max = sizes[cbn].curtmps.om_off = -DPOFF1;
     *  #endif
     */
	forechain = NIL;
#ifdef PI0
	send(REVVBEG);
#endif
}

var(vline, vidl, vtype)
#ifdef PI0
	int vline;
	struct tnode *vidl, *vtype;
{
	register struct nl *np;
	register struct tnode *vl;

	np = gtype(vtype);
	line = vline;
	/* why is this here? */
	for (vl = vidl; vl != TR_NIL; vl = vl->list_node.next) {
		}
	}
	send(REVVAR, vline, vidl, vtype);
}
#else
	int vline;
	register struct tnode *vidl;
	struct tnode *vtype;
{
	register struct nl *np;
	register struct om *op;
	long w;
	int o2;
#ifdef PC
	struct nl	*vp;
#endif

	np = gtype(vtype);
	line = vline;
	w = lwidth(np);
	op = &sizes[cbn];
	for (; vidl != TR_NIL; vidl = vidl->list_node.next) {
#		ifdef OBJ
		    op->curtmps.om_off =
			roundup((int)(op->curtmps.om_off-w), (long)align(np));
		    o2 = op -> curtmps.om_off;
#		endif OBJ
#		ifdef PC
		    if ( cbn == 1 ) {
				/*
				 * global variables are not accessed off the fp
				 * but rather by their names.
				 */
			    o2 = 0;
		    } else {
				/*
				 * locals are aligned, too.
				 */
			    op->curtmps.om_off =
				roundup((int)(op->curtmps.om_off - w),
				(long)align(np));
			    o2 = op -> curtmps.om_off;
		    }
#		endif PC
#		ifdef PC
		vp = enter(defnl((char *) vidl->list_node.list, VAR, np, o2));
#		else
		(void) enter(defnl((char *) vidl->list_node.list, VAR, np, o2));
#		endif
		if ( np != NLNIL && (np -> nl_flags & NFILES) ) {
		    dfiles[ cbn ] = TRUE;
		}
#		ifdef PC
		    if ( cbn == 1 ) {
			putprintf( "	.data" , 0 );
			aligndot(align(np));
			putprintf( "	.comm	" , 1 );
			putprintf( EXTFORMAT , 1 , (int) vidl->list_node.list );
			putprintf( ",%d" , 0 , (int) w );
			putprintf( "	.text" , 0 );
			stabgvar( vp , w , line );
			vp -> extra_flags |= NGLOBAL;
		    } else {
			vp -> extra_flags |= NLOCAL;
		    }
#		endif PC
	}
#	ifdef PTREE
	    {
		pPointer	*Vars;
		pPointer	Var = VarDecl( ovidl , vtype );

		pSeize( PorFHeader[ nesting ] );
		Vars = &( pDEF( PorFHeader[ nesting ] ).PorFVars );
		*Vars = ListAppend( *Vars , Var );
		pRelease( PorFHeader[ nesting ] );
	    }
#	endif
}
#endif

varend()
{

	foredecl();
#ifndef PI0
	sizes[cbn].om_max = sizes[cbn].curtmps.om_off;
#else
	send(REVVEND);
#endif
}

/*
 * Find the width of a type in bytes.
 */
width(np)
	struct nl *np;
{

	return (lwidth(np));
}

long
lwidth(np)
	struct nl *np;
{
	register struct nl *p;

	p = np;
	if (p == NIL)
		return (0);
loop:
	switch (p->class) {
		default:
			panic("wclass");
		case TYPE:
			switch (nloff(p)) {
				case TNIL:
					return (2);
				case TSTR:
				case TSET:
					panic("width");
				default:
					p = p->type;
					goto loop;
			}
		case ARRAY:
			return (aryconst(p, 0));
		case PTR:
			return ( sizeof ( int * ) );
		case FILET:
			return ( sizeof(struct iorec) + lwidth( p -> type ) );
		case CRANGE:
			p = p->type;
			goto loop;
		case RANGE:
			if (p->type == nl+TDOUBLE)
#ifdef DEBUG
				return (hp21mx ? 4 : 8);
#else
				return (8);
#endif
		case SCAL:
			return (bytes(p->range[0], p->range[1]));
		case SET:
			setran(p->type);
			/*
			 * Sets are some multiple of longs
			 */
			return roundup((int)((set.uprbp >> 3) + 1),
				(long)(sizeof(long)));
		case STR:
		case RECORD:
			return ( p->value[NL_OFFS] );
	}
}

    /*
     *	round up x to a multiple of y
     *	for computing offsets of aligned things.
     *	y had better be positive.
     *	rounding is in the direction of x.
     */
long
roundup( x , y )
    int			x;
    register long	y;
    {
	
	if ( y == 0 ) {
	    return x;
	}
	if ( x >= 0 ) {
		return ( ( ( x + ( y - 1 ) ) / y ) * y );
	} else {
		return ( ( ( x - ( y - 1 ) ) / y ) * y );
	}
    }

    /*
     *	alignment of an object using the c alignment scheme
     */
int
align( np )
    struct nl	*np;
    {
	register struct nl *p;
	long elementalign;

	p = np;
	if ( p == NIL ) {
	    return 0;
	}
alignit:
	switch ( p -> class ) {
	    default:
		    panic( "align" );
	    case TYPE:
		    switch ( nloff( p ) ) {
			case TNIL:
				return A_POINT;
			case TSTR:
				return A_STRUCT;
			case TSET:
				return A_SET;
			default:
				p = p -> type;
				goto alignit;
		    }
	    case ARRAY:
			/*
			 * arrays are structures, since they can get
			 * assigned form/to as structure assignments.
			 * preserve internal alignment if it is greater.
			 */
		    elementalign = align(p -> type);
		    return elementalign > A_STRUCT ? elementalign : A_STRUCT;
	    case PTR:
		    return A_POINT;
	    case FILET:
		    return A_FILET;
	    case CRANGE:
	    case RANGE:
		    if ( p -> type == nl+TDOUBLE ) {
			return A_DOUBLE;
		    }
		    /* else, fall through */
	    case SCAL:
		    switch ( bytes( p -> range[0] , p -> range[1] ) ) {
			case 4:
			    return A_LONG;
			case 2:
			    return A_SHORT;
			case 1:
			    return A_CHAR;
			default:
			    panic( "align: scal" );
		    }
	    case SET:
		    return A_SET;
	    case STR:
			/*
			 * arrays of chars are structs
			 */
		    return A_STRUCT;
	    case RECORD:
			/*
			 * the alignment of a record is in its align_info field
			 * why don't we use this for the rest of the namelist?
			 */
		    return p -> align_info;
	}
    }

#ifdef PC
    /*
     *	output an alignment pseudo-op.
     */
aligndot(alignment)
    int	alignment;
#if defined(vax) || defined(tahoe)
{
    switch (alignment) {
	case 1:
	    return;
	case 2:
	    putprintf("	.align 1", 0);
	    return;
	default:
	case 4:
	    putprintf("	.align 2", 0);
	    return;
    }
}
#endif vax || tahoe
#ifdef mc68000
{
    switch (alignment) {
	case 1:
	    return;
	default:
	    putprintf("	.even", 0);
	    return;
    }
}
#endif mc68000
#endif PC
    
/*
 * Return the width of an element
 * of a n time subscripted np.
 */
long aryconst(np, n)
	struct nl *np;
	int n;
{
	register struct nl *p;
	long s, d;

	if ((p = np) == NIL)
		return (NIL);
	if (p->class != ARRAY)
		panic("ary");
	/*
	 * If it is a conformant array, we cannot find the width from
	 * the type.
	 */
	if (p->chain->class == CRANGE)
		return (NIL);
	s = lwidth(p->type);
	/*
	 * Arrays of anything but characters are word aligned.
	 */
	if (s & 1)
		if (s != 1)
			s++;
	/*
	 * Skip the first n subscripts
	 */
	while (n >= 0) {
		p = p->chain;
		n--;
	}
	/*
	 * Sum across remaining subscripts.
	 */
	while (p != NIL) {
		if (p->class != RANGE && p->class != SCAL)
			panic("aryran");
		d = p->range[1] - p->range[0] + 1;
		s *= d;
		p = p->chain;
	}
	return (s);
}

/*
 * Find the lower bound of a set, and also its size in bits.
 */
setran(q)
	struct nl *q;
{
	register lb, ub;
	register struct nl *p;

	p = q;
	if (p == NIL)
		return;
	lb = p->range[0];
	ub = p->range[1];
	if (p->class != RANGE && p->class != SCAL)
		panic("setran");
	set.lwrb = lb;
	/* set.(upperbound prime) = number of bits - 1; */
	set.uprbp = ub-lb;
}

/*
 * Return the number of bytes required to hold an arithmetic quantity
 */
bytes(lb, ub)
	long lb, ub;
{

#ifndef DEBUG
	if (lb < -32768 || ub > 32767)
		return (4);
	else if (lb < -128 || ub > 127)
		return (2);
#else
	if (!hp21mx && (lb < -32768 || ub > 32767))
		return (4);
	if (lb < -128 || ub > 127)
		return (2);
#endif
	else
		return (1);
}
