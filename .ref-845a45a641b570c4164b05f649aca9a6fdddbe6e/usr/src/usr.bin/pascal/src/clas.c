/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)clas.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "tree_ty.h"

/*
 * This is the array of class
 * names for the classes returned
 * by classify.  The order of the 
 * classes is the same as the base
 * of the namelist, with special
 * negative index entries for structures,
 * scalars, pointers, sets and strings
 * to be collapsed into.
 */
char	*clnxxxx[] =
{
	"file",			/* -7	TFILE */
	"record",		/* -6	TREC */
	"array",		/* -5	TARY */
	"scalar",		/* -4	TSCAL */
	"pointer",		/* -3	TPTR */
	"set",			/* -2	TSET */
	"string",		/* -1	TSTR */
	"SNARK",		/*  0	NIL */
	"Boolean",		/*  1	TBOOL */
	"char",			/*  2	TCHAR */
	"integer",		/*  3	TINT */
	"real",			/*  4	TREAL */
	"\"nil\"",		/*  5	TNIL */
};

char **clnames	= &clnxxxx[-(TFIRST)];

/*
 * Classify takes a pointer
 * to a type and returns one
 * of several interesting group
 * classifications for easy use.
 */
classify(p1)
	struct nl *p1;
{
	register struct nl *p;

	p = p1;
swit:
	if (p == NLNIL) {
		nocascade();
		return (NIL);
	}
	if (p == &nl[TSTR])
		return (TSTR);
	if ( p == &nl[ TSET ] ) {
	    return TSET;
	}
	switch (p->class) {
		case PTR:
			return (TPTR);
		case ARRAY:
			if (p->type == nl+T1CHAR)
				return (TSTR);
			return (TARY);
		case STR:
			return (TSTR);
		case SET:
			return (TSET);
		case CRANGE:
		case RANGE:
			p = p->type;
			goto swit;
		case TYPE:
			if (p <= nl+TLAST)
				return (p - nl);
			panic("clas2");
		case FILET:
			return (TFILE);
		case RECORD:
			return (TREC);
		case SCAL:
			return (TSCAL);
		default:
			{
			    panic("clas");
			    return(NIL);
			}
	}
}

#ifndef	PI0
/*
 * Is p a text file?
 */
text(p)
	struct nl *p;
{

	return (p != NIL && p->class == FILET && p->type == nl+T1CHAR);
}
#endif

/*
 * Scalar returns a pointer to
 * the the base scalar type of
 * its argument if its argument
 * is a SCALar else NIL.
 */
struct nl *
scalar(p1)
	struct nl *p1;
{
	register struct nl *p;

	p = p1;
	if (p == NLNIL)
		return (NLNIL);
	if (p->class == RANGE || p->class == CRANGE)
		p = p->type;
	if (p == NLNIL)
		return (NLNIL);
	return (p->class == SCAL ? p : NLNIL);
}

/*
 * Isa tells whether p
 * is one of a group of
 * namelist classes.  The
 * classes wanted are specified
 * by the characters in s.
 * (Note that s would more efficiently,
 * if less clearly, be given by a mask.)
 */
isa(p, s)
	register struct nl *p;
	char *s;
{
	register i;
	register char *cp;

	if (p == NIL)
		return (NIL);
	/*
	 * map ranges down to
	 * the base type
	 */
	if (p->class == RANGE) {
		p = p->type;
	}
	/*
	 * the following character/class
	 * associations are made:
	 *
	 *	s	scalar
	 *	b	Boolean
	 *	c	character
	 *	i	integer
	 *	d	double (real)
	 *	t	set
	 */
	switch (p->class) {
		case SET:
			i = TDOUBLE+1;
			break;
		case SCAL:
			i = 0;
			break;
		case CRANGE:
			/*
			 * find the base type of a conformant array range
			 */
			switch (classify(p->type)) {
				case TBOOL: i = 1; break;
				case TCHAR: i = 2; break;
				case TINT: i = 3; break;
				case TSCAL: i = 0; break;
				default:
					panic( "isa" );
			}
			break;
		default:
			i = p - nl;
	}
	if (i >= 0 && i <= TDOUBLE+1) {
		i = "sbcidt"[i];
		cp = s;
		while (*cp)
			if (*cp++ == i)
				return (1);
	}
	return (NIL);
}

/*
 * Isnta is !isa
 */
isnta(p, s)
    struct nl *p;
    char *s;
{

	return (!isa(p, s));
}

/*
 * "shorthand"
 */
char *
nameof(p)
struct nl *p;
{

	return (clnames[classify(p)]);
}

#ifndef PI0
/* find out for sure what kind of node this is being passed
   possibly several different kinds of node are passed to it */
int nowexp(r)
	struct tnode *r;
{
	if (r->tag == T_WEXP) {
		if (r->var_node.cptr == NIL)
			error("Oct/hex allowed only on writeln/write calls");
		else
			error("Width expressions allowed only in writeln/write calls");
		return (1);
	}
	return (NIL);
}
#endif

    /*
     *	is a variable a local, a formal parameter, or a global?
     *	all this from just the offset:
     *	    globals are at levels 0 or 1
     *	    positives are parameters
     *	    negative evens are locals
     */
/*ARGSUSED*/
whereis( offset , other_flags )
    int		offset;
    char	other_flags;
{
    
#   ifdef OBJ
	return ( offset >= 0 ? PARAMVAR : LOCALVAR );
#   endif OBJ
#   ifdef PC
	switch ( other_flags & ( NGLOBAL | NPARAM | NLOCAL | NNLOCAL) ) {
	    default:
		panic( "whereis" );
	    case NGLOBAL:
		return GLOBALVAR;
	    case NPARAM:
		return PARAMVAR;
	    case NNLOCAL:
		return NAMEDLOCALVAR;
	    case NLOCAL:
		return LOCALVAR;
	}
#   endif PC
}
