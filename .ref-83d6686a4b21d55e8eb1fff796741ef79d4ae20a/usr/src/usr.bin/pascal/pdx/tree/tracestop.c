/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)tracestop.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * Handle trace and stop commands.
 */

#include "defs.h"
#include "breakpoint.h"
#include "sym.h"
#include "tree.h"
#include "runtime.h"
#include "source.h"
#include "object.h"
#include "mappings.h"
#include "machine.h"
#include "tree.rep"

LOCAL SYM *tcontainer();

/*
 * Process a trace/untrace command, basically checking arguments
 * and translate to a call of the appropriate routine.
 */

trace(cmd, exp, where, cond)
int cmd;
NODE *exp;
NODE *where;
NODE *cond;
{
	if (exp == NIL) {
		traceall(cmd, where, cond);
	} else if (exp->op == O_LCON || exp->op == O_QLINE) {
		traceinst(cmd, exp, where, cond);
	} else if (where!=NIL && (where->op==O_QLINE || where->op==O_LCON)) {
		traceat(cmd, exp, where, cond);
	} else {
		tracedata(cmd, exp, where, cond);
	}
	if (where != NIL) {
		tfree(where);
	}
}

/*
 * Set a breakpoint that will turn on tracing.
 *
 * A line number of 0 in the breakpoint information structure
 * means it's a normal trace.
 *
 * A line number of -1 indicates that we want to trace at the instruction
 * rather than source line level.
 *
 * If location is NIL, turn on tracing because if the user
 * has the program stopped somewhere and says "trace",
 * he/she wants to see tracing after continuing execution.
 */

LOCAL traceall(cmd, where, cond)
int cmd;
NODE *where;
NODE *cond;
{
	SYM *s;
	LINENO line;

	if (where != NIL && where->op != O_NAME) {
		error("bad location for trace");
	}
	if (cmd == O_TRACE) {
		line = 0;
	} else {
		line = -1;
	}
	if (where == NIL) {
		switch (cmd) {
			case O_TRACE:
				if (tracing != 0) {
					error("already tracing lines");
				}
				tracing++;
				addcond(TRPRINT, cond);
				break;

			case O_TRACEI:
				if (inst_tracing != 0) {
					error("already tracing instructions");
				}
				inst_tracing++;
				addcond(TRPRINT, cond);
				break;

			default:
				panic("bad cmd in traceall");
				break;
		}
		s = program;
	} else if (where->op != O_NAME) {
		trerror("found %t, expected procedure or function", where);
	} else {
		s = where->nameval;
		if (!isblock(s)) {
			error("\"%s\" is not a procedure or function", name(s));
		}
	}
	addbp(codeloc(s), ALL_ON, s, cond, NIL, line);
}

/*
 * Set up the appropriate breakpoint for tracing an instruction.
 */

LOCAL traceinst(cmd, exp, where, cond)
int cmd;
NODE *exp;
NODE *where;
NODE *cond;
{
	LINENO line;
	ADDRESS addr;

	if (where != NIL) {
		error("unexpected \"at\" or \"in\"");
	}
	if (cmd == O_TRACEI) {
		if (exp->op == O_QLINE) {
			addr = (ADDRESS) exp->right->lconval;
		} else if (exp->op == O_LCON) {
			addr = (ADDRESS) exp->lconval;
		} else {
			trerror("expected integer constant, found %t", exp);
		}
		line = -1;
	} else {
		if (exp->op == O_QLINE) {
			line = (LINENO) exp->right->lconval;
			addr = objaddr(line, exp->left->sconval);
		} else {
			line = (LINENO) exp->lconval;
			addr = objaddr(line, cursource);
		}
		if (addr == (ADDRESS) -1) {
			error("can't trace line %d", line);
		}
	}
	tfree(exp);
	addbp(addr, INST, NIL, cond, NIL, line);
}

/*
 * set a breakpoint to print an expression at a given line or address
 */

LOCAL traceat(cmd, exp, where, cond)
int cmd;
NODE *exp;
NODE *where;
NODE *cond;
{
	LINENO line;
	ADDRESS addr;

	if (cmd == O_TRACEI) {
		if (where->op != O_LCON) {
			trerror("expected integer constant, found %t", where);
		}
		line = -1;
		addr = (ADDRESS) where->lconval;
	} else {
		line = (LINENO) where->right->lconval;
		addr = objaddr(line, where->left->sconval);
		if (addr == (ADDRESS) -1) {
			error("can't trace at line %d", line);
		}
	}
	addbp(addr, AT_BP, NIL, cond, exp, line);
}

/*
 * Set up breakpoint for tracing data.
 *
 * The tracing of blocks lies somewhere between instruction and data;
 * it's here since a block cannot be distinguished from other terms.
 *
 * As in "traceall", if the "block" is the main program then the
 * user didn't actually specify a block.  This means we want to
 * turn tracing on ourselves because if the program is stopped
 * we want to be on regardless of whether they say "cont" or "run".
 */

LOCAL tracedata(cmd, exp, block, cond)
int cmd;
NODE *exp;
NODE *block;
NODE *cond;
{
	SYM *s, *t;

#ifdef lint
	cmd = cmd;
#endif
	if (exp->op != O_RVAL && exp->op != O_CALL) {
		error("can't trace expressions");
	}
	if (block == NIL) {
		t = tcontainer(exp->left);
	} else if (block->op == O_NAME) {
		t = block->nameval;
	} else {
		trerror("found %t, expected procedure or function", block);
	}
	if (exp->left->op == O_NAME) {
		s = exp->left->nameval;
		if (isblock(s)) {
			addbp(codeloc(t), BLOCK_ON, t, cond, exp->left, 0);
			if (t == program) {
				addbp(codeloc(s), CALL, s, cond, NIL, 0);
			}
			return;
		}
	}
	addbp(codeloc(t), TERM_ON, t, cond, exp, 0);
	if (curfunc == t) {
		var_tracing++;
		addvar(TRPRINT, exp, cond);
		addbp(return_addr(), TERM_OFF, t, cond, exp, 0);
	}
}

/*
 * Setting and unsetting of stops.
 */

stop(cmd, exp, where, cond)
int cmd;
NODE *exp;
NODE *where;
NODE *cond;
{
	SYM *s;
	LINENO n;

	if (exp != NIL) {
		stopvar(cmd, exp, where, cond);
	} else if (cond != NIL) {
		if (where == NIL) {
			s = program;
		} else if (where->op == O_NAME) {
			s = where->nameval;
		} else {
			error("bad location for stop");
		}
		n = codeloc(s);
		addbp(n, STOP_ON, s, cond, NIL, n);
		addcond(TRSTOP, cond);
		var_tracing++;
	} else if (where->op == O_NAME) {
		s = where->nameval;
		if (!isblock(s)) {
			error("\"%s\" is not a procedure or function", name(s));
		}
		n = codeloc(s);
		addbp(n, STOP_BP, s, cond, NIL, srcline(firstline(s)));
	} else {
		stopinst(cmd, where, cond);
	}
	if (where != NIL) {
		tfree(where);
	}
}

LOCAL stopinst(cmd, where, cond)
int cmd;
NODE *where;
NODE *cond;
{
	LINENO line;
	ADDRESS addr;

	if (where->op != O_QLINE) {
		error("expected line number");
	}
	if (cmd == O_STOP) {
		line = (LINENO) where->right->lconval;
		addr = objaddr(line, where->left->sconval);
		if (addr == (ADDRESS) -1) {
			error("can't stop at that line");
		}
	} else {
		line = -1;
		addr = (ADDRESS) where->right->lconval;
	}
	addbp(addr, STOP_BP, NIL, cond, NIL, line);
}

/*
 * Implement stopping on assignment to a variable by adding it to
 * the variable list.
 */

LOCAL stopvar(cmd, exp, where, cond)
int cmd;
NODE *exp;
NODE *where;
NODE *cond;
{
	SYM *s;

	if (exp->op != O_RVAL) {
		trerror("found %t, expected variable", exp);
	}
	if (cmd == O_STOPI) {
		inst_tracing++;
	}
	var_tracing++;
	addvar(TRSTOP, exp, cond);
	if (where == NIL) {
		s = program;
	} else if (where->op == O_NAME) {
		s = where->nameval;
	} else {
		error("bad location for stop");
	}
	addbp(codeloc(s), STOP_ON, s, cond, exp, 0);
}

/*
 * Figure out the block that contains the symbols
 * in the given variable expression.
 */

LOCAL SYM *tcontainer(var)
NODE *var;
{
	NODE *p;

	p = var;
	while (p->op != O_NAME) {
		if (isleaf(p->op)) {
			panic("unexpected op %d in tcontainer", p->op);
			/* NOTREACHED */
		}
		p = p->left;
	}
	return container(p->nameval);
}
