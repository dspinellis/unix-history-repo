/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)eval.c 1.3 %G%";

/*
 * Parse tree evaluation.
 */

#include "defs.h"
#include "tree.h"
#include "sym.h"
#include "process.h"
#include "source.h"
#include "mappings.h"
#include "breakpoint.h"
#include "machine.h"
#include "tree.rep"

/*
 * Evaluate a parse tree using a stack; value is left at top.
 */

#define STACKSIZE 2000

STACK stack[STACKSIZE];
STACK *sp = &stack[0];

eval(p)
register NODE *p;
{
    long r0, r1;
    double fr0, fr1;

    if (p == NULL) {
	return;
    }
    switch(degree(p->op)) {
	case BINARY:
	    eval(p->right);
	    if (isreal(p->op)) {
		fr1 = pop(double);
	    } else if (isint(p->op)) {
		r1 = pop(long);
	    }
	    /* fall through */
	case UNARY:
	    eval(p->left);
	    if (isreal(p->op)) {
		fr0 = pop(double);
	    } else if (isint(p->op)) {
		r0 = pop(long);
	    }
	    break;

	default:
	    /* do nothing */;
	}
    switch(p->op) {
	case O_NAME: {
	    SYM *s, *f;

	    s = p->nameval;
	    f = container(s);
	    if (!isactive(f)) {
		error("\"%s\" is not active", name(f));
	    }
	    push(int, address(s, NIL));
	    break;
	}

	case O_LCON:
	    switch (size(p->nodetype)) {
		case sizeof(char):
		    push(char, p->lconval);
		    break;

		case sizeof(short):
		    push(short, p->lconval);
		    break;

		case sizeof(long):
		    push(long, p->lconval);
		    break;

		default:
		    panic("bad size %d for LCON", size(p->nodetype));
	    }
	    break;

	case O_FCON:
	    push(double, p->fconval);
	    break;

	case O_SCON: {
	    int len;

	    len = size(p->nodetype);
	    mov(p->sconval, sp, len);
	    sp += len;
	    break;
	}

	case O_INDEX: {
	    int n;
	    long i;

	    n = pop(int);
	    i = evalindex(p->left->nodetype, popsmall(p->right->nodetype));
	    push(int, n + i*size(p->nodetype));
	    break;
	}

	case O_INDIR: {
	    ADDRESS a;

	    a = pop(ADDRESS);
	    if (a == 0) {
		error("reference through nil pointer");
	    }
	    dread(sp, a, sizeof(ADDRESS));
	    sp += sizeof(ADDRESS);
	    break;
	}

	/*
	 * Get the value of the expression addressed by the top of the stack.
	 * Push the result back on the stack.  Never push less than a long.
	 */

	case O_RVAL: {
	    ADDRESS addr, len;
	    long i;

	    addr = pop(int);
	    if (addr == 0) {
		error("reference through nil pointer");
	    }
	    len = size(p->nodetype);
	    if (!rpush(addr, len)) {
		error("expression too large to evaluate");
	    }
	    break;
	}

	case O_COMMA:
	    break;

	case O_ITOF:
	    push(double, (double) r0);
	    break;

	case O_ADD:
	    push(long, r0+r1);
	    break;

	case O_ADDF:
	    push(double, fr0+fr1);
	    break;

	case O_SUB:
	    push(long, r0-r1);
	    break;

	case O_SUBF:
	    push(double, fr0-fr1);
	    break;

	case O_NEG:
	    push(long, -r0);
	    break;

	case O_NEGF:
	    push(double, -fr0);
	    break;

	case O_MUL:
	    push(long, r0*r1);
	    break;

	case O_MULF:
	    push(double, fr0*fr1);
	    break;

	case O_DIVF:
	    if (fr1 == 0) {
		error("error: division by 0");
	    }
	    push(double, fr0/fr1);
	    break;

	case O_DIV:
	    if (r1 == 0) {
		error("error: div by 0");
	    }
	    push(long, r0/r1);
	    break;

	case O_MOD:
	    if (r1 == 0) {
		error("error: mod by 0");
	    }
	    push(long, r0%r1);
	    break;

	case O_LT:
	    push(BOOLEAN, r0 < r1);
	    break;

	case O_LTF:
	    push(BOOLEAN, fr0 < fr1);
	    break;

	case O_LE:
	    push(BOOLEAN, r0 <= r1);
	    break;

	case O_LEF:
	    push(BOOLEAN, fr0 <= fr1);
	    break;

	case O_GT:
	    push(BOOLEAN, r0 > r1);
	    break;

	case O_GTF:
	    push(BOOLEAN, fr0 > fr1);
	    break;

	case O_EQ:
	    push(BOOLEAN, r0 == r1);
	    break;

	case O_EQF:
	    push(BOOLEAN, fr0 == fr1);
	    break;

	case O_NE:
	    push(BOOLEAN, r0 != r1);
	    break;

	case O_NEF:
	    push(BOOLEAN, fr0 != fr1);
	    break;

	case O_AND:
	    push(BOOLEAN, r0 && r1);
	    break;

	case O_OR:
	    push(BOOLEAN, r0 || r1);
	    break;

	case O_ASSIGN:
	    assign(p->left, p->right);
	    break;

	case O_CHFILE:
	    if (p->sconval == NIL) {
		printf("%s\n", cursource);
	    } else {
		skimsource(p->sconval);
	    }
	    break;

	case O_CONT:
	    cont();
	    printnews();
	    break;

	case O_LIST: {
	    SYM *b;

	    if (p->left->op == O_NAME) {
		b = p->left->nameval;
		if (!isblock(b)) {
		    error("\"%s\" is not a procedure or function", name(b));
		}
		r0 = srcline(firstline(b));
		r1 = r0 + 5;
		if (r1 > lastlinenum) {
		    r1 = lastlinenum;
		}
		r0 = r0 - 5;
		if (r0 < 1) {
		    r0 = 1;
		}
	    } else {
		eval(p->left->right);
		eval(p->left->left);
		r0 = pop(long);
		r1 = pop(long);
	    }
	    printlines((LINENO) r0, (LINENO) r1);
	    break;
	}

	case O_XI:
	case O_XD:
	{
	    SYM *b;

	    if (p->left->op == O_CALL) {
		b = p->left->left->nameval;
		r0 = codeloc(b);
		r1 = firstline(b);
	    } else {
		eval(p->left->right);
		eval(p->left->left);
		r0 = pop(long);
		r1 = pop(long);
	    }
	    if (p->op == O_XI)  {
		printinst((ADDRESS) r0, (ADDRESS) r1);
	    } else {
		printdata((ADDRESS) r0, (ADDRESS) r1);
	    }
	    break;
	}

	case O_NEXT:
	    next();
	    printnews();
	    break;

	case O_PRINT: {
	    NODE *o;

	    for (o = p->left; o != NIL; o = o->right) {
		eval(o->left);
		printval(o->left->nodetype);
		putchar(' ');
	    }
	    putchar('\n');
	    break;
	}

	case O_STEP:
	    stepc();
	    printnews();
	    break;

	case O_WHATIS:
	    if (p->left->op == O_NAME) {
		printdecl(p->left->nameval);
	    } else {
		printdecl(p->left->nodetype);
	    }
	    break;

	case O_WHICH:
	    printwhich(p->nameval);
	    putchar('\n');
	    break;

	case O_WHERE:
	    where();
	    break;

	case O_ALIAS:
	    alias(p->left->sconval, p->right->sconval);
	    break;

	case O_CALL:
	    callproc(p->left, p->right);
	    break;

	case O_EDIT:
	    edit(p->sconval);
	    break;

	case O_DUMP:
	    dump();
	    break;

	case O_GRIPE:
	    gripe();
	    break;

	case O_HELP:
	    help();
	    break;

	case O_REMAKE:
	    remake();
	    break;

	case O_RUN:
	    run();
	    break;

	case O_SOURCE:
	    setinput(p->sconval);
	    break;

	case O_STATUS:
	    status();
	    break;

	case O_TRACE:
	case O_TRACEI:
	    trace(p->op, p->what, p->where, p->cond);
	    if (isstdin()) {
		status();
	    }
	    break;

	case O_STOP:
	case O_STOPI:
	    stop(p->op, p->what, p->where, p->cond);
	    if (isstdin()) {
		status();
	    }
	    break;

	case O_DELETE:
	    eval(p->left);
	    delbp((unsigned int) pop(long));
	    break;

	default:
	    panic("eval: bad op %d", p->op);
    }
}

/*
 * Push "len" bytes onto the expression stack from address "addr"
 * in the process.  Normally TRUE is returned, however if there
 * isn't enough room on the stack, rpush returns FALSE.
 */

BOOLEAN rpush(addr, len)
ADDRESS addr;
int len;
{
    BOOLEAN success;

    if (sp + len >= &stack[STACKSIZE]) {
	success = FALSE;
    } else {
	dread(sp, addr, len);
	sp += len;
	success = TRUE;
    }
    return success;
}

/*
 * Pop an item of the given type which is assumed to be no larger
 * than a long and return it expanded into a long.
 */

long popsmall(t)
SYM *t;
{
    long r;

    switch (size(t)) {
	case sizeof(char):
	    r = (long) pop(char);
	    break;

	case sizeof(short):
	    r = (long) pop(short);
	    break;

	case sizeof(long):
	    r = pop(long);
	    break;

	default:
	    panic("popsmall: size is %d", size(t));
    }
    return r;
}

/*
 * evaluate a conditional expression
 */

BOOLEAN cond(p)
NODE *p;
{
    if (p == NIL) {
	return(TRUE);
    }
    eval(p);
    return(pop(BOOLEAN));
}

/*
 * Return the address corresponding to a given tree.
 */

ADDRESS lval(p)
NODE *p;
{
    eval(p);
    return(pop(ADDRESS));
}
