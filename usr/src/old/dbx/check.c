/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
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
static char sccsid[] = "@(#)check.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * Check a tree for semantic correctness.
 */

#include "defs.h"
#include "tree.h"
#include "operators.h"
#include "events.h"
#include "symbols.h"
#include "scanner.h"
#include "source.h"
#include "object.h"
#include "mappings.h"
#include "process.h"
#include <signal.h>

#ifndef public
#endif

/*
 * Check that the nodes in a tree have the correct arguments
 * in order to be evaluated.  Basically the error checking here
 * frees the evaluation routines from worrying about anything
 * except dynamic errors, e.g. subscript out of range.
 */

public check(p)
register Node p;
{
    Node p1, p2;
    Address addr;
    Symbol f;

    checkref(p);
    switch (p->op) {
	case O_ASSIGN:
	    p1 = p->value.arg[0];
	    p2 = p->value.arg[1];
	    if (varIsSet("$unsafeassign")) {
		if (size(p1->nodetype) != size(p2->nodetype)) {
		    error("incompatible sizes");
		}
	    } else if (not compatible(p1->nodetype, p2->nodetype)) {
		error("incompatible types");
	    }
	    break;

	case O_CATCH:
	case O_IGNORE:
	    if (p->value.lcon < 0 or p->value.lcon > NSIG) {
		error("invalid signal number");
	    }
	    break;

	case O_CONT:
	    if (p->value.lcon != DEFSIG and (
		p->value.lcon < 0 or p->value.lcon > NSIG)
	    ) {
		error("invalid signal number");
	    }
	    break;

	case O_DUMP:
	    if (p->value.arg[0] != nil) {
		if (p->value.arg[0]->op == O_SYM) {
		    f = p->value.arg[0]->value.sym;
		    if (not isblock(f)) {
			error("\"%s\" is not a block", symname(f));
		    }
		} else {
		    beginerrmsg();
		    fprintf(stderr, "expected a symbol, found \"");
		    prtree(stderr, p->value.arg[0]);
		    fprintf(stderr, "\"");
		    enderrmsg();
		}
	    }
	    break;

	case O_LIST:
	    if (p->value.arg[0]->op == O_SYM) {
		f = p->value.arg[0]->value.sym;
		if (not isblock(f) or ismodule(f)) {
		    error("\"%s\" is not a procedure or function", symname(f));
		}
		addr = firstline(f);
		if (addr == NOADDR) {
		    error("\"%s\" is empty", symname(f));
		}
	    }
	    break;

	case O_TRACE:
	case O_TRACEI:
	    chktrace(p);
	    break;

	case O_STOP:
	case O_STOPI:
	    chkstop(p);
	    break;

	case O_CALLPROC:
	case O_CALL:
	    if (not isroutine(p->value.arg[0]->nodetype)) {
		beginerrmsg();
		fprintf(stderr, "\"");
		prtree(stderr, p->value.arg[0]);
		fprintf(stderr, "\" not call-able");
		enderrmsg();
	    }
	    break;

	case O_WHEREIS:
	    if (p->value.arg[0]->op == O_SYM and
	      p->value.arg[0]->value.sym == nil) {
		error("symbol not defined");
	    }
	    break;

	default:
	    break;
    }
}

/*
 * Check arguments to a trace command.
 */

private chktrace(p)
Node p;
{
    Node exp, place, cond;

    exp = p->value.arg[0];
    place = p->value.arg[1];
    cond = p->value.arg[2];
    if (exp == nil) {
	chkblock(place);
    } else if (exp->op == O_LCON or exp->op == O_QLINE) {
	if (place != nil) {
	    error("unexpected \"at\" or \"in\"");
	}
	if (p->op == O_TRACE) {
	    chkline(exp);
	} else {
	    chkaddr(exp);
	}
    } else if (place != nil and (place->op == O_QLINE or place->op == O_LCON)) {
	if (p->op == O_TRACE) {
	    chkline(place);
	} else {
	    chkaddr(place);
	}
    } else {
	if (exp->op != O_RVAL and exp->op != O_SYM and exp->op != O_CALL) {
	    error("can't trace expressions");
	}
	chkblock(place);
    }
}

/*
 * Check arguments to a stop command.
 */

private chkstop(p)
Node p;
{
    Node exp, place, cond;

    exp = p->value.arg[0];
    place = p->value.arg[1];
    cond = p->value.arg[2];
    if (exp != nil) {
	if (exp->op != O_RVAL and exp->op != O_SYM and exp->op != O_LCON) {
	    beginerrmsg();
	    fprintf(stderr, "expected variable, found ");
	    prtree(stderr, exp);
	    enderrmsg();
	}
	chkblock(place);
    } else if (place != nil) {
	if (place->op == O_SYM) {
	    chkblock(place);
	} else {
	    if (p->op == O_STOP) {
		chkline(place);
	    } else {
		chkaddr(place);
	    }
	}
    }
}

/*
 * Check to see that the given node specifies some subprogram.
 * Nil is ok since that means the entire program.
 */

private chkblock(b)
Node b;
{
    Symbol p, outer;

    if (b != nil) {
	if (b->op != O_SYM) {
	    beginerrmsg();
	    fprintf(stderr, "expected subprogram, found ");
	    prtree(stderr, b);
	    enderrmsg();
	} else if (ismodule(b->value.sym)) {
	    outer = b->value.sym;
	    while (outer != nil) {
		find(p, outer->name) where p->block == outer endfind(p);
		if (p == nil) {
		    outer = nil;
		    error("\"%s\" is not a subprogram", symname(b->value.sym));
		} else if (ismodule(p)) {
		    outer = p;
		} else {
		    outer = nil;
		    b->value.sym = p;
		}
	    }
	} else if (
	    b->value.sym->class == VAR and
	    b->value.sym->name == b->value.sym->block->name and
	    b->value.sym->block->class == FUNC
	) {
	    b->value.sym = b->value.sym->block;
	} else if (not isblock(b->value.sym)) {
	    error("\"%s\" is not a subprogram", symname(b->value.sym));
	}
    }
}

/*
 * Check to make sure a node corresponds to a source line.
 */

private chkline(p)
Node p;
{
    if (p == nil) {
	error("missing line");
    } else if (p->op != O_QLINE and p->op != O_LCON) {
	error("expected source line number, found \"%t\"", p);
    }
}

/*
 * Check to make sure a node corresponds to an address.
 */

private chkaddr(p)
Node p;
{
    if (p == nil) {
	error("missing address");
    } else if (p->op != O_LCON and p->op != O_QLINE) {
	beginerrmsg();
	fprintf(stderr, "expected address, found \"");
	prtree(stderr, p);
	fprintf(stderr, "\"");
	enderrmsg();
    }
}
