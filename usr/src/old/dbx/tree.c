/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)@(#)tree.c 1.1 %G%";

/*
 * Parse tree management.
 */

#include "defs.h"
#include "tree.h"
#include "operators.h"
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

	case O_LCON:
	case O_DELETE:
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
	    q = nextarg(Node);
	    if (q->op == O_CALL) {
		*p = *q;
		dispose(q);
	    } else {
		p->value.arg[0] = q;
	    }
	    break;

	case O_INDIR:
	    q = nextarg(Node);
	    if (q != nil and q->op == O_RVAL) {
		p->value.arg[0] = q->value.arg[0];
		dispose(q);
	    } else {
		p->value.arg[0] = q;
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
    return p;
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
 * Return the tree for a unary ampersand operator.
 */

public Node amper(p)
Node p;
{
    Node r;

    checkref(p);
    switch (p->op) {
	case O_RVAL:
	    r = p->value.arg[0];
	    break;

	case O_CALL:
	    r = build(O_LCON, codeloc(p->value.arg[0]->value.sym));
	    tfree(p);
	    break;

	case O_SYM:
	    if (isblock(p->value.sym)) {
		r = build(O_LCON, codeloc(p->value.sym));
	    } else {
		r = build(O_LCON, address(p->value.sym, nil));
	    }
	    tfree(p);
	    break;

	case O_DOT:
	    r = p;
	    break;

	case O_INDIR:
	    r = p->value.arg[0];
	    dispose(p);
	    break;

	default:
	    beginerrmsg();
	    fprintf(stderr, "expected variable, found ");
	    prtree(stderr, p);
	    tfree(p);
	    enderrmsg();
	    /* NOTREACHED */
    }
    r->nodetype = t_int;
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

	case O_DELETE:
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
	    fprintf(f, " }", opinfo[ord(cmd->op)].opstring);
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
	    fprintf(f, "%s if ", cmd->value.trace.inst ? "stopi" : "stop");
	    prtree(f, ifcmd->value.event.cond);
	    done = true;
	}
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
 * Print a tree back out in Pascal form.
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
		if (compatible(p->nodetype, t_char)) {
		    fprintf(f, "'%c'", p->value.lcon);
		} else {
		    fprintf(f, "%d", p->value.lcon);
		}
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
		if (p->value.arg[0]->op == O_SYM) {
		    printname(f, p->value.arg[0]->value.sym);
		} else {
		    prtree(f, p->value.arg[0]);
		}
		break;

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
		q = p->value.arg[0];
		if (isvarparam(q->nodetype)) {
		    prtree(f, q);
		} else {
		    if (q->op == O_SYM or q->op == O_LCON or q->op == O_DOT) {
			prtree(f, q);
			fprintf(f, "^");
		    } else {
			fprintf(f, "*(");
			prtree(f, q);
			fprintf(f, ")");
		    }
		}
		break;

	    case O_DOT:
		q = p->value.arg[0];
		if (q->op == O_INDIR) {
		    prtree(f, q->value.arg[0]);
		} else {
		    prtree(f, q);
		}
		fprintf(f, ".%s", symname(p->value.arg[1]->value.sym));
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
			error("internal error: bad op %d in prtree", op);
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

/*
 * A recursive tree search routine to test if two trees * are equivalent.
 */

public Boolean tr_equal(t1, t2)
register Node t1;
register Node t2;
{
    register Boolean b;

    if (t1 == nil and t2 == nil) {
	b = true;
    } else if (t1 == nil or t2 == nil) {
	b = false;
    } else if (t1->op != t2->op or degree(t1->op) != degree(t2->op)) {
	b = false;
    } else {
	switch (degree(t1->op)) {
	    case LEAF:
		switch (t1->op) {
		    case O_NAME:
			b = (Boolean) (t1->value.name == t2->value.name);
			break;

		    case O_SYM:
			b = (Boolean) (t1->value.sym == t2->value.sym);
			break;

		    case O_LCON:
			b = (Boolean) (t1->value.lcon == t2->value.lcon);
			break;

		    case O_FCON:
			b = (Boolean) (t1->value.fcon == t2->value.fcon);
			break;

		    case O_SCON:
			b = (Boolean) (t1->value.scon == t2->value.scon);
			break;

		    default:
			panic("tr_equal: leaf %d\n", t1->op);
		}
		/*NOTREACHED*/

	    case BINARY:
		if (not tr_equal(t1->value.arg[0], t2->value.arg[0])) {
		    b = false;
		} else {
		    b = tr_equal(t1->value.arg[1], t2->value.arg[1]);
		}
		break;

	    case UNARY:
		b = tr_equal(t1->value.arg[0], t2->value.arg[0]);
		break;

	    default:
		panic("tr_equal: bad degree for op %d\n", t1->op);
	}
    }
    return b;
}
