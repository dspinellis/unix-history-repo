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
static char sccsid[] = "@(#)which.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

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
