/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)yyid.c	5.2 (Berkeley) %G%";
#endif not lint

#include "whoami.h"
#include <0.h>
#include "tree_ty.h"	/* must be included for yy.h */
#include "yy.h"

#ifdef PI
extern	union semstack *yypv;
/*
 * Determine whether the identifier whose name
 * is "cp" can possibly be a kind, which is a
 * namelist class.  We look through the symbol
 * table for the first instance of cp as a non-field,
 * and at all instances of cp as a field.
 * If any of these are ok, we return true, else false.
 * It would be much better to handle with's correctly,
 * even to just know whether we are in a with at all.
 *
 * Note that we don't disallow constants on the lhs of assignment.
 */
identis(cp, kind)
	register char *cp;
	int kind;
{
	register struct nl *p;
	int i;

	/*
	 * Cp is NIL when error recovery inserts it.
	 */
	if (cp == NIL)
		return (1);

	/*
	 * Record kind we want for possible later use by yyrecover
	 */
	yyidwant = kind;
	yyidhave = NIL;
	i = ( (int) cp ) & 077;
	for (p = disptab[i]; p != NIL; p = p->nl_next)
		if (p->symbol == cp) {
			if (yyidok(p, kind))
				goto gotit;
			if (p->class != FIELD && p->class != BADUSE)
				break;
		}
	if (p != NIL)
		for (p = p->nl_next; p != NIL; p = p->nl_next)
			if (p->symbol == cp && p->class == FIELD && yyidok(p, kind))
				goto gotit;
	return (0);
gotit:
	if (p->class == BADUSE && !Recovery) {
		yybadref(p, OY.Yyeline);
		yypv[0].i_entry = NIL;
	}
	return (1);
}

/*
 * A bad reference to the identifier cp on line
 * line and use implying the addition of kindmask
 * to the mask of kind information.
 */
struct nl *
yybaduse(cp, line, kindmask)
	register char *cp;
	int line, kindmask;
{
	register struct nl *p, *oldp;
	int i;

	i = ( (int) cp ) & 077;
	for (p = disptab[i]; p != NIL; p = p->nl_next)
		if (p->symbol == cp)
			break;
	oldp = p;
	if (p == NIL || p->class != BADUSE)
		p = enter(defnl(cp, BADUSE, NLNIL, 0));
	p->value[NL_KINDS] |= kindmask;
	yybadref(p, line);
	return (oldp);
}

    /*
     *	ud is initialized so that esavestr will allocate
     *	sizeof ( struct udinfo ) bytes for the 'real' struct udinfo
     */
struct	udinfo ud = { ~0 , (struct udinfo *) ~0 , 0};
/*
 * Record a reference to an undefined identifier,
 * or one which is improperly used.
 */
yybadref(p, line)
	register struct nl *p;
	int line;
{
	register struct udinfo *udp;

	if (p->chain != NIL && ((struct udinfo *) p->chain)->ud_line == line)
		return;
	udp = (struct udinfo *) esavestr((char *) &ud);
	udp->ud_line = line;
	udp->ud_next = (struct udinfo *) p->chain;
	p->chain = (struct nl *) udp;
}

#define	varkinds	((1<<CONST)|(1<<VAR)|(1<<REF)|(1<<ARRAY)|(1<<PTR) \
			|(1<<RECORD)|(1<<FIELD)|(1<<FUNC)|(1<<FVAR) \
			|(1<<FFUNC)|(1<<PROC)|(1<<FPROC))
/*
 * Is the symbol in the p entry of the namelist
 * even possibly a kind kind?  If not, update
 * what we have based on this encounter.
 */
yyidok(p, kind)
	register struct nl *p;
	int kind;
{

	if (p->class == BADUSE) {
		if (kind == VAR)
			return (p->value[0] & varkinds);
		return (p->value[0] & (1 << kind));
	}
	if (yyidok1(p, kind))
		return (1);
	if (yyidhave != NIL)
		yyidhave = IMPROPER;
	else
		yyidhave = p->class;
	return (0);
}

yyidok1(p, kind)
	register struct nl *p;
	int kind;
{

	switch (kind) {
		default:
		case FUNC:
			return (   p -> class == FUNC
				|| p -> class == FVAR
				|| p -> class == FFUNC );
		case PROC:
			return ( p -> class == PROC || p -> class == FPROC );
		case CONST:
		case TYPE:
		case FIELD:
			return (p->class == kind);
		case VAR:
			return (p->class == CONST || yyisvar(p, NIL));
		case ARRAY:
		case RECORD:
			return (yyisvar(p, kind));
		case PTRFILE:
			return (yyisvar(p, PTR) || yyisvar(p, FILET));
	}
}

yyisvar(p, varclass)
	register struct nl *p;
	int varclass;
{

	switch (p->class) {
		case FIELD:
		case VAR:
		case REF:
		case FVAR:
		/*
		 * We would prefer to return
		 * parameterless functions only.
		 */
		case FUNC:
		case FFUNC:
			return (varclass == NIL || (p->type != NIL && p->type->class == varclass));
		case PROC:
		case FPROC:
			return ( varclass == NIL );
	}
	return (0);
}
#endif
#ifdef PXP
#ifndef DEBUG
identis()
{

	return (1);
}
#endif
#ifdef DEBUG
extern	char *classes[];

char	kindchars[]	"UCTVAQRDPF";
/*
 * Fake routine "identis" for pxp when testing error recovery.
 * Looks at letters in variable names to answer questions
 * about attributes.  Mapping is
 *	C	const_id
 *	T	type_id
 *	V	var_id		also if any of AQRDF
 *	A	array_id
 *	Q	ptr_id
 *	R	record_id
 *	D	field_id	D for "dot"
 *	P	proc_id
 *	F	func_id
 */
identis(cp, kind)
	register char *cp;
	int kind;
{
	register char *dp;
	char kindch;

	/*
	 * Don't do anything unless -T
	 */
	if (!typetest)
		return (1);

	/*
	 * Inserted symbols are always correct
	 */
	if (cp == NIL)
		return (1);
	/*
	 * Set up the names for error messages
	 */
	yyidwant = classes[kind];
	for (dp = kindchars; *dp; dp++)
		if (any(cp, *dp)) {
			yyidhave = classes[dp - kindchars];
			break;
		}

	/*
	 * U in the name means undefined
	 */
	if (any(cp, 'U'))
		return (0);

	kindch = kindchars[kind];
	if (kindch == 'V')
		for (dp = "AQRDF"; *dp; dp++)
			if (any(cp, *dp))
				return (1);
	return (any(cp, kindch));
}
#endif
#endif
