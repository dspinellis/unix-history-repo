/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)which.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Routines to distinguish symbols of the same name.
 */

#include "defs.h"
#include "sym.h"
#include "classes.h"
#include "symtab.h"
#include "mappings.h"
#include "machine.h"
#include "sym.rep"

/*
 * Figure out the "current" symbol being referred to,
 * this is either the active one or the most visible from the
 * current scope.
 *
 * Fields are purposely ignored; these can be gotten to via "findclass".
 */

SYM *which(s)
SYM *s;
{
	register SYM *p, *t, *f;

	if (s == program || isbuiltin(s)) {
		return(s);
	}
	if (!isactive(program)) {
		f = program;
	} else {
		f = whatblock(pc);
		if (f == NIL) {
			panic("no block for addr 0x%x", pc);
		}
	}
	for (p = f; p != NIL; p = p->func) {
		if ((t = findsym(s, p)) != NIL) {
			break;
		}
	}
	if (t == NIL) {
		error("\"%s\" is not known in \"%s\"", s->symbol, f->symbol);
	}
	return(t);
}

/*
 * Find a (non-field) symbol with name s->symbol belonging to block f.
 *
 * Parameters to the main program are purposely "not found" because
 * pi gives them no type.
 */

SYM *findsym(s, f)
SYM *s;
SYM *f;
{
	register SYM *t;

	if (!isblock(f)) {
		error("%s is not a block", f->symbol);
	}
	for (t = s; t != NIL; t = t->next_sym) {
		if (t->func == f && !(f == program && isparam(t)) &&
		  t->class != FIELD && streq(t->symbol, s->symbol)) {
			break;
		}
	}
	return(t);
}

/*
 * Find the symbol which is has the same name and scope as the
 * given symbol but is of the given field.  Return NIL if there is none.
 */

SYM *findclass(s, cl)
SYM *s;
char cl;
{
	register SYM *t;

	if (s->class == cl) {
		return(s);
	}
	t = st_lookup(symtab, s->symbol);
	while (t != NIL && (t->class != cl || t->func != s->func ||
	  !streq(s->symbol, t->symbol))) {
		t = t->next_sym;
	}
	return(t);
}
