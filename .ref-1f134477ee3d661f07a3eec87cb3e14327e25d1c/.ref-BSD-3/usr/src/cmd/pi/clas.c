/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 November 1978
 */

#include "whoami"
#include "0.h"
#include "tree.h"

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
	if (p == NIL) {
		nocascade();
		return (NIL);
	}
	if (p == &nl[TSTR])
		return (TSTR);
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
			panic("clas");
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
scalar(p1)
	struct nl *p1;
{
	register struct nl *p;

	p = p1;
	if (p == NIL)
		return (NIL);
	if (p->class == RANGE)
		p = p->type;
	if (p == NIL)
		return (NIL);
	return (p->class == SCAL ? p : NIL);
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
	if (p->class == RANGE)
		p = p->type;
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
{

	return (!isa(p, s));
}

/*
 * "shorthand"
 */
nameof(p)
{

	return (clnames[classify(p)]);
}

#ifndef PI0
nowexp(r)
	int *r;
{
	if (r[0] == T_WEXP) {
		if (r[2] == NIL)
			error("Oct/hex allowed only on writeln/write calls");
		else
			error("Width expressions allowed only in writeln/write calls");
		return (1);
	}
	return (NIL);
}
#endif
