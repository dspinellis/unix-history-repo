/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)readsym.c	5.1 (Berkeley) %G%";
#endif not lint
/*
 * SYM representation dependent routines for reading in the
 * symbol information from the object file.
 */

#include "defs.h"
#include "sym.h"
#include "symtab.h"
#include "object.h"
#include "objfmt.h"
#include "process.h"
#include "sym/classes.h"
#include "objsym.rep"
#include "sym/sym.rep"

LOCAL SYM *findblock();
LOCAL SYM *enterblock();
LOCAL SYM *findfunc();

/*
 * Read the information on a symbol from the object file, return a
 * SYM with the info.
 */

SYM *readsym(fp)
FILE *fp;
{
    register SYM *s, *t;
    SYM cursym;
    static SYM *func;

    t = &cursym;
    getsym(fp, t);
    if (isblock(t)) {
#       if (isvaxpx)
	    if (t->class == PROG) {
		t->symvalue.funcv.codeloc = HEADER_BYTES;
	    }
#       endif
	s = findblock(t);
	if (s->class == PROG) {
	    program = s;
	    s->func = NIL;
	} else {
	    s->func = func;
	}
    } else if (t->class == BADUSE) {
	func = enterblock(t);
	return(func);
    } else {
	s = st_insert(symtab, t->symbol);
	t->next_sym = s->next_sym;
	*s = *t;
	if (s->class == FVAR) {
	    s->func = findfunc(s);
	} else {
	    s->func = func;
	}
    }

/*
 * This glitch is pi's fault.  It gives string constants
 * a type whose symbol number is -1.  For what reason, I know not.
 */
    if (s->type == (SYM *) -1) {
	s->type = NIL;
    } else {
	chkpatch(&s->type);
    }
    chkpatch(&s->chain);
    if (s->class == RECORD || s->class == VARNT) {
	chkpatch(&s->symvalue.varnt.vtorec);
	chkpatch(&s->symvalue.varnt.vtag);
    }
    if (isblock(s)) {
	fixparams(s);
    }
    return(s);
}

/*
 * Read the SYM information in the object file.
 */

LOCAL getsym(fp, t)
FILE *fp;
SYM *t;
{
    OBJSYM osym;
    register OBJSYM *o;

    get(fp, osym);
    o = &osym;
    if (o->strindex == 0) {
	t->symbol = NIL;
    } else {
	t->symbol = &stringtab[o->strindex];
    }
    t->class = o->oclass;
    t->blkno = o->oblkno;
    t->type = (SYM *) o->typno;
    t->chain = (SYM *) o->chno;
    t->symvalue.rangev.lower = o->osymvalue.orangev.lower;
    t->symvalue.rangev.upper = o->osymvalue.orangev.upper;
    if (t->class == RECORD || t->class == VARNT) {
	t->symvalue.varnt.vtorec = (SYM *) o->osymvalue.ovarnt.vtorecno;
	t->symvalue.varnt.vtag = (SYM *) o->osymvalue.ovarnt.vtagno;
    }
}

/*
 * The symbol read in is a real block so we find it's entry,
 * copy the information, and return a pointer to it.
 */

LOCAL SYM *findblock(t)
SYM *t;
{
    SYM *s;

    s = st_lookup(symtab, t->symbol);
    while (s != NIL &&
	(s->class != FUNC || s->type != NIL ||
	strcmp(s->symbol, t->symbol) != 0)) {
	s = s->next_sym;
    }
    if (s == NIL) {
	panic("can't find %s", t->symbol);
    }
    t->next_sym = s->next_sym;
    *s = *t;
    s->symvalue.funcv.codeloc -= HEADER_BYTES;
    findbeginning(s);
    newfunc(s);
    return(s);
}

/*
 * Found a "fake" block symbol, enter it.
 */

LOCAL SYM *enterblock(t)
SYM *t;
{
    SYM *s;

    s = st_insert(symtab, t->symbol);
    t->next_sym = s->next_sym;
    *s = *t;
    backpatch();
    s->class = FUNC;
    s->type = NIL;
    return(s);
}

/*
 * This kludge is brought to you by the pi symbol table.
 * Parameters appear with the function in which they reside,
 * messing up the way the "func" field is calculated.
 *
 * The assumption here is that parameters appear before the function.
 */

LOCAL fixparams(f)
SYM *f;
{
    register SYM *s;

    for (s = f->chain; s != NIL; s = s->chain) {
	s->func = f;
    }
}

/*
 * Find the function entry associated with a function variable.
 * Function variables come out a bit strangely in the symbol table;
 * if we didn't do this here, a function variable would have a func
 * field that referred to the outer block.
 */

#define notfunc(f, fv) (\
    f->class != FUNC || f->type != NIL || \
    strcmp(f->symbol, fv->symbol) != 0 \
    )

LOCAL SYM *findfunc(fv)
SYM *fv;
{
    register SYM *t;

    t = st_lookup(symtab, fv->symbol);
    while (t != NIL && notfunc(t, fv)) {
	t = t->next_sym;
    }
    if (t == NIL) {
	panic("no func for funcvar %s", fv->symbol);
    }
    return(t);
}
