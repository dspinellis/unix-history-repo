/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)build.c	5.2 (Berkeley) %G%";
#endif not lint
/*
 * parse tree building routines
 *
 * Semantics is not checked here, this is done by the "treetype" routine
 * in the SYM directory which returns the type of the newly built tree.
 */

#include "defs.h"
#include "tree.h"
#include "sym.h"
#include "source.h"
#include "tree.rep"

/*
 * header for using routines with unknown number and types of arguments
 * I didn't like the looks of the standard varargs.h.
 */

typedef char *ARGLIST;

#define nextarg(arglist, type)	((type *) (arglist += sizeof(type)))[-1]

/*
 * build a tree
 */

/*VARARGS1*/
NODE *build(op, args)
OP op;
{
	register NODE *p;
	register ARGLIST ap;

	p = alloc(1, NODE);
	p->op = op;
	ap = (ARGLIST) &args;
	switch(degree(op)) {
		case BINARY:
			p->left = nextarg(ap, NODE *);
			p->right = nextarg(ap, NODE *);
			break;

		case UNARY:
			p->left = nextarg(ap, NODE *);
			p->right = NIL;
			break;

	}
	switch(op) {
		case O_NAME:
		case O_WHICH:
			p->nameval = nextarg(ap, SYM *);
			break;

		case O_LCON:
			p->lconval = nextarg(ap, long);
			break;

		case O_FCON:
			p->fconval = nextarg(ap, double);
			break;

		case O_SCON:
			p->sconval = nextarg(ap, char *);
			break;

		case O_CALL:
			p->left = nextarg(ap, NODE *);
			p->right = nextarg(ap, NODE *);
			break;

		case O_CHFILE:
			p->sconval = nextarg(ap, char *);
			break;

		case O_EDIT:
			p->sconval = nextarg(ap, char *);
			if (p->sconval == NIL) {
				p->sconval = cursource;
			}
			break;

		case O_SOURCE:
			p->sconval = nextarg(ap, char *);
			break;

		case O_PRINT:
		case O_WHATIS:
		case O_LIST:
		case O_XI:
		case O_XD:
			p->left = nextarg(ap, NODE *);
			break;

		case O_TRACE:
		case O_TRACEI:
		case O_STOP:
		case O_STOPI:
			p->what = nextarg(ap, NODE *);
			p->where = nextarg(ap, NODE *);
			p->cond = nextarg(ap, NODE *);
			break;

		case O_DELETE:
			p->left = build(O_LCON, nextarg(ap, long));
			break;

		case O_QLINE: {
			char *s;

			s = nextarg(ap, char *);
			p->left = alloc(1, NODE);
			p->left->op = O_SCON;
			if (s != cursource) {
				p->left->sconval = s;
				s[strlen(s) - 1] = '\0';
			} else {
				p->left->sconval = strdup(s);
			}
			p->right = nextarg(ap, NODE *);
			break;
		}

		case O_ALIAS:
			p->left = alloc(1, NODE);
			p->left->op = O_SCON;
			p->left->sconval = nextarg(ap, char *);
			p->right = alloc(1, NODE);
			p->right->op = O_SCON;
			p->right->sconval = nextarg(ap, char *);
			break;
			
		default:
			if (op < O_NOP || op > O_LASTOP) {
				panic("build: bad op %d", op);
			}
			break;
	}
	p->nodetype = treetype(p, (ARGLIST) &args);
	return(p);
}
