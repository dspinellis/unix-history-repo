/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)var.c 1.13 %G%";

#include "whoami.h"
#include "0.h"
#include "align.h"
#include "iorec.h"
#ifdef PC
#   include	"pc.h"
#   include	"pcops.h"
#endif PC

/*
 * Declare variables of a var part.  DPOFF1 is
 * the local variable storage for all prog/proc/func
 * modules aside from the block mark.  The total size
 * of all the local variables is entered into the
 * size array.
 */
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
	int vline, *vidl, *vtype;
{
	register struct nl *np;
	register int *vl;

	np = gtype(vtype);
	line = vline;
	for (vl = vidl; vl != NIL; vl = vl[2]) {
		}
	}
	send(REVVAR, vline, vidl, vtype);
}
#else
	int vline;
	register int *vidl;
	int *vtype;
{
	register struct nl *np;
	register struct om *op;
	long w;
	int o2;
	int *ovidl = vidl;
	struct nl	*vp;

	np = gtype(vtype);
	line = vline;
	w = lwidth(np);
	op = &sizes[cbn];
	for (; vidl != NIL; vidl = vidl[2]) {
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
		vp = enter(defnl(vidl[1], VAR, np, o2));
		if ( np -> nl_flags & NFILES ) {
		    dfiles[ cbn ] = TRUE;
		}
#		ifdef PC
		    if ( cbn == 1 ) {
			putprintf( "	.data" , 0 );
			putprintf( "	.align	%d" , 0 , dotalign(align(np)));
			putprintf( "	.comm	" , 1 );
			putprintf( EXTFORMAT , 1 , vidl[1] );
			putprintf( ",%d" , 0 , w );
			putprintf( "	.text" , 0 );
			stabgvar( vidl[1] , p2type( np ) , o2 , w , line );
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
 * Evening
 */
long
leven(w)
	register long w;
{
	if (w < 0)
		return (w & 0xfffffffe);
	return ((w+1) & 0xfffffffe);
}

int
even(w)
	register int w;
{
	return leven((long)w);
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
	long w;

	p = np;
	if (p == NIL)
		return (0);
loop:
	switch (p->class) {
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
			return roundup((int)((set.uprbp >> 3) + 1),
				(long)(A_SET));
		case STR:
		case RECORD:
			return ( p->value[NL_OFFS] );
		default:
			panic("wclass");
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

	p = np;
	if ( p == NIL ) {
	    return 0;
	}
alignit:
	switch ( p -> class ) {
	    case TYPE:
		    switch ( nloff( p ) ) {
			case TNIL:
				return A_POINT;
			case TSTR:
				return A_CHAR;
			case TSET:
				return A_SET;
			default:
				p = p -> type;
				goto alignit;
		    }
	    case ARRAY:
			/*
			 * arrays are aligned as their component types
			 */
		    p = p -> type;
		    goto alignit;
	    case PTR:
		    return A_POINT;
	    case FILET:
		    return A_FILET;
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
		    return A_CHAR;
	    case RECORD:
			/*
			 * the alignment of a record is in its align_info field
			 * why don't we use this for the rest of the namelist?
			 */
		    return p -> align_info;
	    default:
		    panic( "align" );
	}
    }

    /*
     *	given an alignment, return power of two for .align pseudo-op
     */
dotalign( alignment )
    int	alignment;
{
    
    switch ( alignment ) {
	case A_CHAR:		/*
				 * also
				 *	A_STRUCT
				 */
		return 0;
	case A_SHORT:
		return 1;
	case A_LONG:		/*
				 * also
				 *	A_POINT, A_INT, A_FLOAT, A_DOUBLE,
				 *	A_STACK, A_FILET, A_SET
				 */
		return 2;
    }
}

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
		return (NIL);
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
