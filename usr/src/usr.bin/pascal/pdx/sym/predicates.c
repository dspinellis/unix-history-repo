/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)predicates.c	5.1 (Berkeley) %G%";
#endif not lint
/*
 * The basic tests on a symbol.
 */

#include "defs.h"
#include "sym.h"
#include "symtab.h"
#include "btypes.h"
#include "classes.h"
#include "sym.rep"

/*
 * Test if a symbol is a parameter.  This is true if there
 * is a cycle from s->func to s via chain pointers.
 */

BOOLEAN isparam(s)
SYM *s;
{
    register SYM *t;

    for (t = s->func; t != NIL; t = t->chain) {
	if (t == s) {
	    return(TRUE);
	}
    }
    return(FALSE);
}

/*
 * Test if a symbol is a var parameter, i.e. has class REF.
 */

BOOLEAN isvarparam(s)
SYM *s;
{
    return (BOOLEAN) s->class == REF;
}

/*
 * Test if a symbol is a variable (actually any addressible quantity
 * with do).
 */

BOOLEAN isvariable(s)
SYM *s;
{
    return s->class == VAR || s->class == FVAR || s->class == REF;
}

/*
 * Test if a symbol is a block, e.g. function, procedure, or the
 * main program.
 */

BOOLEAN isblock(s)
register SYM *s;
{
    return(s->class == FUNC || s->class == PROC || s->class == PROG);
}

/*
 * Test if a symbol is builtin, that is, a predefined type or
 * reserved word.
 */

BOOLEAN isbuiltin(s)
SYM *s;
{
    return(s->blkno == 0 && s->class != PROG && s->class != VAR);
}

/*
 * Compatible tests if two types are compatible.  The issue
 * is complicated a bit by ranges.
 *
 * Integers and reals are not compatible since they cannot always be mixed.
 */

BOOLEAN compatible(t1, t2)
register SYM *t1, *t2;
{
    register BOOLEAN b;

    if (isvariable(t1)) {
	t1 = t1->type;
    }
    if (isvariable(t2)) {
	t2 = t2->type;
    }
    if (t1 == t2) {
	b = TRUE;
    } else {
	t1 = rtype(t1);
	t2 = rtype(t2);
	if (t1->type == t2->type) {
	    if (t1->class == RANGE && t2->class == RANGE) {
		b = TRUE;
	    } else if ((t1->class == SCAL || t1->class == CONST) &&
	      (t2->class == SCAL || t2->class == CONST)) {
		b = TRUE;
	    } else if (t1->type == t_char &&
	      t1->class == ARRAY && t2->class == ARRAY) {
		b = TRUE;
	    } else {
		b = FALSE;
	    }
    /*
     * A kludge here for "nil".  Should be handled better.
     * Opens a pandora's box for integer/pointer compatibility.
     */
	} else if ((t1->class == RANGE && t2->class == PTR) ||
	  (t2->class == RANGE && t1->class == PTR)) {
	    b = TRUE;
	} else {
	    b = FALSE;
	}
    }
    return b;
}

/*
 * Predicate to test if a symbol should be printed.  We don't print
 * files, for example, simply because there's no good way to do it.
 * The symbol must be within the given function.
 */

BOOLEAN should_print(s, f)
SYM *s;
SYM *f;
{
    SYM *t;

    if (s->func != f || (s->class != VAR && s->class != FVAR)) {
	return(FALSE);
    } else if (s->chain != NIL) {
	return(FALSE);
    } else {
	t = rtype(s->type);
	if (t == NIL || t->class == FILET || t->class == SET) {
	    return(FALSE);
	} else {
	    return(TRUE);
	}
    }
}

/*
 * Test if the name of a symbol is uniquely defined or not.
 */

BOOLEAN isambiguous(s)
SYM *s;
{
    SYM *t;

    t = st_lookup(symtab, s->symbol);
    if (t == NIL) {
	panic("symbol name vanished");
    }
    while (t != NIL && (s == t || !streq(t->symbol, s->symbol))) {
	t = t->next_sym;
    }
    return t != NIL;
}
