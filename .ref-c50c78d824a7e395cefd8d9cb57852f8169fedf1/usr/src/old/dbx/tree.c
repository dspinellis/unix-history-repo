/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)tree.c	5.3 (Berkeley) %G%";
#endif not lint

static char rcsid[] = "$Header: tree.c,v 1.3 87/07/08 21:38:59 donn Exp $";

/*
 * Parse tree management.
 */

#include "defs.h"
#include "tree.h"
#include "operators.h"
#include "debug.h"
#include "eval.h"
#include "events.h"
#include "symbols.h"
#include "scanner.h"
#include "source.h"
#include "object.h"
#include "mappings.h"
#include "process.h"
#include "machine.h"

#ifndef public
#include "lists.h"

typedef struct Node *Node;
typedef Node Command;
typedef List Cmdlist;

#include "operators.h"
#include "symbols.h"
#include "events.h"

#define MAXNARGS 5

struct Node {
    Operator op;
    Symbol nodetype;
    union treevalue {
	Symbol sym;
	Name name;
	long lcon;
	double fcon;
	String scon;
	Node arg[MAXNARGS];
	struct {
	    Node cond;
	    Cmdlist actions;
	} event;
	struct {
	    Boolean inst;
	    Event event;
	    Cmdlist actions;
	} trace;
	struct {
	    Boolean source;
	    Boolean skipcalls;
	} step;
	struct {
	    String mode;
	    Node beginaddr;
	    Node endaddr;
	    Integer count;
	} examine;
    } value;
};

#define evalcmd(cmd) eval(cmd)
#define cmdlist_append(cmd, cl) list_append(list_item(cmd), nil, cl)

#endif

typedef char *Arglist;

#define nextarg(type)  ((type *) (ap += sizeof(type)))[-1]

/*
 * Build a tree.
 */

/* VARARGS1 */
public Node build(op, args)
Operator op;
{
    register Node p, q;
    register Arglist ap;
    Integer i;

    p = new(Node);
    p->op = op;
    p->nodetype = nil;
    ap = (Arglist) &args;
    switch (op) {
	case O_NAME:
	    p->value.name = nextarg(Name);
	    break;

	case O_SYM:
	case O_PRINTCALL:
	case O_PRINTRTN:
	case O_PROCRTN:
	    p->value.sym = nextarg(Symbol);
	    break;

	case O_DEBUG:
	case O_LCON:
	case O_CCON:
	case O_CONT:
	case O_CATCH:
	case O_IGNORE:
	case O_TRACEOFF:
	    p->value.lcon = nextarg(long);
	    break;

	case O_FCON:
	    p->value.fcon = nextarg(double);
	    break;

	case O_SCON:
	case O_CHFILE:
	case O_EDIT:
	case O_SOURCE:
	    p->value.scon = nextarg(String);
	    break;

	case O_RVAL:
	case O_INDIR:
	    p->value.arg[0] = nextarg(Node);
	    break;

	case O_CALL:
	    q = nextarg(Node);
	    if (q->op == O_SYM and
		(q->value.sym->class == TYPE or q->value.sym->class == TAG)
	    ) {
		p->op = O_TYPERENAME;
		p->value.arg[0] = nextarg(Node);
		p->value.arg[1] = q;
		q = p->value.arg[0];
		if (q->value.arg[1] != nil) {
		    error("too many arguments to type rename");
		}
		p->value.arg[0] = q->value.arg[0];
	    } else {
		p->value.arg[0] = q;
		p->value.arg[1] = nextarg(Node);
	    }
	    break;

	case O_ADDEVENT:
	case O_ONCE:
	case O_IF:
	    p->value.event.cond = nextarg(Node);
	    p->value.event.actions = nextarg(Cmdlist);
	    break;

	case O_TRACEON:
	    p->value.trace.inst = nextarg(Boolean);
	    p->value.trace.event = nil;
	    p->value.trace.actions = nextarg(Cmdlist);
	    break;

	case O_STEP:
	    p->value.step.source = nextarg(Boolean);
	    p->value.step.skipcalls = nextarg(Boolean);
	    break;

	case O_EXAMINE:
	    p->value.examine.mode = nextarg(String);
	    p->value.examine.beginaddr = nextarg(Node);
	    p->value.examine.endaddr = nextarg(Node);
	    p->value.examine.count = nextarg(Integer);
	    break;

	default:
	    for (i = 0; i < nargs(op); i++) {
		p->value.arg[i] = nextarg(Node);
	    }
	    break;
    }
    check(p);
    assigntypes(p);
    if (tracetree) {     
	printf("built %s node 0x%x with arg[0] 0x%x arg[1] 0x%x\n",
	    opname(p->op), p, p->value.arg[0], p->value.arg[1]);
	fflush(stdout);
    }
    return p;
}

/*
 * Strip away indirection from a node, thus returning a node for
 * interpreting the expression as an lvalue.
 */

public Node unrval (exp)
Node exp;
{
    Node p;
    Symbol t;

    if (exp->op == O_RVAL) {
	p = exp->value.arg[0];
	dispose(exp);
    } else if (exp->op == O_INDIR) {
	p = exp->value.arg[0];
	if (p->op == O_RVAL) {
	    p->op = O_INDIR;
	    p->nodetype = exp->nodetype;
	}
	dispose(exp);
    } else {
	p = exp;
    }
    return p;
}

/*
 * Create a node for renaming a node to a pointer type.
 */

public Node renameptr (p, t)
Node p;
Node t;
{
    t->nodetype = newSymbol(nil, 0, PTR, t->nodetype, nil);
    p = build(O_TYPERENAME, p, t);
}

/*
 * Return the tree for a unary ampersand operator.
 */

public Node amper(p)
Node p;
{
    Node r;

    checkref(p);
    switch (p->op) {
	case O_RVAL:
	case O_INDIR:
	    r = p->value.arg[0];
	    r->nodetype = t_addr;
	    dispose(p);
	    break;

	case O_TYPERENAME:
	    r = p;
	    r->nodetype = newSymbol(nil, 0, PTR, r->nodetype, nil);
	    r->nodetype->language = p->nodetype->language;
	    break;

	case O_SYM:
	    if (isblock(p->value.sym)) {
		r = build(O_LCON, codeloc(p->value.sym));
	    } else {
		r = build(O_LCON, address(p->value.sym, nil));
	    }
	    r->nodetype = t_addr;
	    dispose(p);
	    break;

	case O_DOT:
	    r = p;
	    r->nodetype = t_addr;
	    break;

	default:
	    beginerrmsg();
	    fprintf(stderr, "expected variable, found \"");
	    prtree(stderr, p);
	    fprintf(stderr, "\"");
	    tfree(p);
	    enderrmsg();
	    /* NOTREACHED */
    }
    return r;
}

/*
 * Create a "concrete" version of a node.
 * This is necessary when the type of the node contains
 * an unresolved type reference.
 */

public Node concrete(p)
Node p;
{
    findtype(p->nodetype);
    return build(O_INDIR, p);
}

/*
 * Create a command list from a single command.
 */

public Cmdlist buildcmdlist(cmd)
Command cmd;
{
    Cmdlist cmdlist;

    cmdlist = list_alloc();
    cmdlist_append(cmd, cmdlist);
    return cmdlist;
}

/*
 * Print out a command.
 */

public printcmd(f, cmd)
File f;
Command cmd;
{
    register Integer i;
    register Command c;
    register Node p;

    switch (cmd->op) {
	case O_PRINTIFCHANGED:
	case O_PRINTSRCPOS:
	case O_STOPIFCHANGED:
	case O_TRACEON:
	    break;

	case O_STEP:
	    if (cmd->value.step.skipcalls) {
		fprintf(f, "next");
	    } else {
		fprintf(f, "step");
	    }
	    if (not cmd->value.step.source) {
		fprintf(f, "i");
	    }
	    break;

	default:
	    fprintf(f, "%s", opinfo[ord(cmd->op)].opstring);
	    if (nargs(cmd->op) != 0) {
		fprintf(f, " ");
	    }
	    break;
    }
    switch (cmd->op) {
	case O_PRINTCALL:
	case O_PRINTRTN:
	case O_PROCRTN:
	    fprintf(f, "%s", symname(cmd->value.sym));
	    break;

	case O_PRINTSRCPOS:
	    p = cmd->value.arg[0];
	    if (p != nil and p->op != O_QLINE) {
		printf("trace ");
		prtree(f, p);
	    }
	    break;

	case O_CHFILE:
	case O_EDIT:
	case O_SOURCE:
	    fprintf(f, "%s", cmd->value.scon);
	    break;

	case O_CATCH:
	case O_IGNORE:
	case O_TRACEOFF:
	    fprintf(f, "%d", cmd->value.lcon);
	    break;

	case O_ADDEVENT:
	case O_ONCE:
	case O_IF:
	    fprintf(f, " ");
	    prtree(f, cmd->value.event.cond);
	    fprintf(f, " { ");
	    foreach (Command, c, cmd->value.event.actions)
		printcmd(f, c);
		if (not list_islast()) {
		    fprintf(f, ";");
		}
	    endfor
	    fprintf(f, "%s }", opinfo[ord(cmd->op)].opstring);
	    break;

	case O_TRACEON:
	    print_tracestop(f, cmd);
	    break;

	case O_EXAMINE:
	    prtree(f, cmd->value.examine.beginaddr);
	    if (cmd->value.examine.endaddr != nil) {
		fprintf(f, ",");
		prtree(f, cmd->value.examine.endaddr);
	    }
	    fprintf(f, "/");
	    if (cmd->value.examine.count > 1) {
		fprintf(f, "%d", cmd->value.examine.count);
	    }
	    fprintf("%s", cmd->value.examine.mode);
	    break;

	default:
	    if (nargs(cmd->op) != 0) {
		i = 0;
		for (;;) {
		    prtree(f, cmd->value.arg[i]);
		    ++i;
		if (i >= nargs(cmd->op)) break;
		    fprintf(f, " ");
		}
	    }
	    break;
    }
}

/*
 * Print out a trace/stop command name.
 */

#define fprintI(f, b) { if (b) fprintf(f, "i"); }

private print_tracestop(f, cmd)
File f;
Command cmd;
{
    register Command c, ifcmd, stopcmd;
    Boolean done;

    done = false;
    ifcmd = list_element(Command, list_head(cmd->value.trace.actions));
    checkref(ifcmd);
    if (ifcmd->op == O_IF) {
	stopcmd = list_element(Command, list_head(ifcmd->value.event.actions));
	checkref(stopcmd);
	if (stopcmd->op == O_STOPX) {
	    fprintf(f, "stop");
	    fprintI(f, cmd->value.trace.inst);
	    fprintf(f, " if ");
	    prtree(f, ifcmd->value.event.cond);
	    done = true;
	}
    } else if (ifcmd->op == O_STOPIFCHANGED) {
	fprintf(f, "stop");
	fprintI(f, cmd->value.trace.inst);
	fprintf(f, " ");
	prtree(f, ifcmd->value.arg[0]);
	done = true;
    }
    if (not done) {
	fprintf(f, "%s ", cmd->value.trace.inst ? "tracei" : "trace");
	foreach (Command, c, cmd->value.trace.actions)
	    printcmd(f, c);
	    if (not list_islast()) {
		fprintf(f, ";");
	    }
	endfor
    }
}

/*
 * Print out a tree.
 */

public prtree(f, p)
File f;
register Node p;
{
    register Node q;
    Operator op;

    if (p != nil) {
	op = p->op;
	if (ord(op) > ord(O_LASTOP)) {
	    panic("bad op %d in prtree", p->op);
	}
	switch (op) {
	    case O_NAME:
		fprintf(f, "%s", ident(p->value.name));
		break;

	    case O_SYM:
		printname(f, p->value.sym);
		break;

	    case O_QLINE:
		if (nlhdr.nfiles > 1) {
		    prtree(f, p->value.arg[0]);
		    fprintf(f, ":");
		}
		prtree(f, p->value.arg[1]);
		break;

	    case O_LCON:
		fprintf(f, "%d", p->value.lcon);
		break;

	    case O_CCON:
		fprintf(f, "'%c'", p->value.lcon);
		break;

	    case O_FCON:
		fprintf(f, "%g", p->value.fcon);
		break;

	    case O_SCON:
		fprintf(f, "\"%s\"", p->value.scon);
		break;

	    case O_INDEX:
		prtree(f, p->value.arg[0]);
		fprintf(f, "[");
		prtree(f, p->value.arg[1]);
		fprintf(f, "]");
		break;

	    case O_COMMA:
		prtree(f, p->value.arg[0]);
		if (p->value.arg[1] != nil) {
		    fprintf(f, ", ");
		    prtree(f, p->value.arg[1]);
		}
		break;

	    case O_RVAL:
	    case O_ITOF:
		prtree(f, p->value.arg[0]);
		break;

	    case O_CALL:
		prtree(f, p->value.arg[0]);
		if (p->value.arg[1]!= nil) {
		    fprintf(f, "(");
		    prtree(f, p->value.arg[1]);
		    fprintf(f, ")");
		}
		break;

	    case O_INDIR:
		prtree(f, p->value.arg[0]);
		fprintf(f, "^");
		break;

	    case O_DOT:
		prtree(f, p->value.arg[0]);
		fprintf(f, ".%s", symname(p->value.arg[1]->value.sym));
		break;

	    case O_TYPERENAME:
		prtree(f, p->value.arg[1]);
		fprintf(f, "(");
		prtree(f, p->value.arg[0]);
		fprintf(f, ")");
		break;

	    default:
		switch (degree(op)) {
		    case BINARY:
			prtree(f, p->value.arg[0]);
			fprintf(f, "%s", opinfo[ord(op)].opstring);
			prtree(f, p->value.arg[1]);
			break;

		    case UNARY:
			fprintf(f, "%s", opinfo[ord(op)].opstring);
			prtree(f, p->value.arg[0]);
			break;

		    default:
			if (opinfo[ord(op)].opstring == nil) {
			    fprintf(f, "[op %d]", ord(op));
			} else {
			    fprintf(f, "%s", opinfo[ord(op)].opstring);
			}
			break;
		}
		break;
	}
    }
}

/*
 * Free storage associated with a tree.
 */

public tfree(p)
Node p;
{
    Integer i;

    if (p == nil) {
	return;
    }
    switch (p->op) {
	case O_QLINE:
	    dispose(p->value.arg[0]->value.scon);
	    dispose(p->value.arg[0]);
	    tfree(p->value.arg[1]);
	    break;

	case O_SCON:
	    unmkstring(p->nodetype);
	    dispose(p->nodetype);
	    dispose(p->value.scon);
	    break;

	default:
	    for (i = 0; i < nargs(p->op); i++) {
		tfree(p->value.arg[i]);
	    }
	    break;
    }
    dispose(p);
}
