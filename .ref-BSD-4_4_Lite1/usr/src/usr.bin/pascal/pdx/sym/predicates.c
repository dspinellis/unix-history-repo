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
static char sccsid[] = "@(#)predicates.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

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
