/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)func.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "whoami.h"
#ifdef OBJ
    /*
     *	the rest of the file
     */
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "tree_ty.h"

/*
 * Funccod generates code for
 * built in function calls and calls
 * call to generate calls to user
 * defined functions and procedures.
 */
struct nl
*funccod(r)
	struct tnode *r;
{
	struct nl *p;
	register struct nl *p1;
	struct nl *tempnlp;
	register struct tnode *al;
	register op;
	int argc;
	struct tnode *argv, tr, tr2;

	/*
	 * Verify that the given name
	 * is defined and the name of
	 * a function.
	 */
	p = lookup(r->pcall_node.proc_id);
	if (p == NLNIL) {
		rvlist(r->pcall_node.arg);
		return (NLNIL);
	}
	if (p->class != FUNC && p->class != FFUNC) {
		error("%s is not a function", p->symbol);
		rvlist(r->pcall_node.arg);
		return (NLNIL);
	}
	argv = r->pcall_node.arg;
	/*
	 * Call handles user defined
	 * procedures and functions
	 */
	if (bn != 0)
		return (call(p, argv, FUNC, bn));
	/*
	 * Count the arguments
	 */
	argc = 0;
	for (al = argv; al != TR_NIL; al = al->list_node.next)
		argc++;
	/*
	 * Built-in functions have
	 * their interpreter opcode
	 * associated with them.
	 */
	op = p->value[0] &~ NSTAND;
	if (opt('s') && (p->value[0] & NSTAND)) {
		standard();
		error("%s is a nonstandard function", p->symbol);
	}
	switch (op) {
		/*
		 * Parameterless functions
		 */
		case O_CLCK:
		case O_SCLCK:
		case O_WCLCK:
		case O_ARGC:
			if (argc != 0) {
				error("%s takes no arguments", p->symbol);
				rvlist(argv);
				return (NLNIL);
			}
			(void) put(1, op);
			return (nl+T4INT);
		case O_EOF:
		case O_EOLN:
			if (argc == 0) {
				argv = (&tr);
				tr.list_node.list = (&tr2);
				tr2.tag = T_VAR;
				tr2.var_node.cptr = input->symbol;
				tr2.var_node.line_no = NIL;
				tr2.var_node.qual = TR_NIL;
				argc = 1;
			} else if (argc != 1) {
				error("%s takes either zero or one argument", p->symbol);
				rvlist(argv);
				return (NLNIL);
			}
		}
	/*
	 * All other functions take
	 * exactly one argument.
	 */
	if (argc != 1) {
		error("%s takes exactly one argument", p->symbol);
		rvlist(argv);
		return (NLNIL);
	}
	/*
	 * Evaluate the argmument
	 */
	if (op == O_EOF || op == O_EOLN)
		p1 = stklval(argv->list_node.list, NIL );
	else
		p1 = stkrval(argv->list_node.list, NLNIL , (long) RREQ );
	if (p1 == NLNIL)
		return (NLNIL);
	switch (op) {
		case 0:
			error("%s is an unimplemented 6000-3.4 extension", p->symbol);
		default:
			panic("func1");
		case O_EXP:
		case O_SIN:
		case O_COS:
		case O_ATAN:
		case O_LN:
		case O_SQRT:
		case O_RANDOM:
		case O_EXPO:
		case O_UNDEF:
			if (isa(p1, "i"))
				convert( nl+T4INT , nl+TDOUBLE);
			else if (isnta(p1, "d")) {
				error("%s's argument must be integer or real, not %s", p->symbol, nameof(p1));
				return (NLNIL);
			}
			(void) put(1, op);
			if (op == O_UNDEF)
				return (nl+TBOOL);
			else if (op == O_EXPO)
				return (nl+T4INT);
			else
				return (nl+TDOUBLE);
		case O_SEED:
			if (isnta(p1, "i")) {
				error("seed's argument must be an integer, not %s", nameof(p1));
				return (NLNIL);
			}
			(void) put(1, op);
			return (nl+T4INT);
		case O_ROUND:
		case O_TRUNC:
			if (isnta(p1, "d"))  {
				error("%s's argument must be a real, not %s", p->symbol, nameof(p1));
				return (NLNIL);
			}
			(void) put(1, op);
			return (nl+T4INT);
		case O_ABS2:
		case O_SQR2:
			if (isa(p1, "d")) {
				(void) put(1, op + O_ABS8-O_ABS2);
				return (nl+TDOUBLE);
			}
			if (isa(p1, "i")) {
				(void) put(1, op + (width(p1) >> 2));
				return (nl+T4INT);
			}
			error("%s's argument must be an integer or real, not %s", p->symbol, nameof(p1));
			return (NLNIL);
		case O_ORD2:
			if (isa(p1, "bcis")) {
				return (nl+T4INT);
			}
			if (classify(p1) == TPTR) {
			    if (!opt('s')) {
				return (nl+T4INT);
			    }
			    standard();
			}
			error("ord's argument must be of scalar type, not %s",
				nameof(p1));
			return (NLNIL);
		case O_SUCC2:
		case O_PRED2:
			if (isa(p1, "d")) {
				error("%s is forbidden for reals", p->symbol);
				return (NLNIL);
			}
			if ( isnta( p1 , "bcsi" ) ) {
				error("%s's argument must be of scalar type, not %s", p->symbol, nameof(p1));
				return NIL;
			}
			tempnlp = p1 -> class == TYPE ? p1 -> type : p1;
			if (isa(p1, "i")) {
				if (width(p1) <= 2) {
					op += O_PRED24 - O_PRED2;
					(void) put(3, op, (int)tempnlp->range[0],
						(int)tempnlp->range[1]);
				} else {
					op++;
					(void) put(3, op, tempnlp->range[0],
						tempnlp->range[1]);
				}
				return nl + T4INT;
			} else {
				(void) put(3, op, (int)tempnlp->range[0],
					(int)tempnlp->range[1]);
				return p1;
			}
		case O_ODD2:
			if (isnta(p1, "i")) {
				error("odd's argument must be an integer, not %s", nameof(p1));
				return (NLNIL);
			}
			(void) put(1, op + (width(p1) >> 2));
			return (nl+TBOOL);
		case O_CHR2:
			if (isnta(p1, "i")) {
				error("chr's argument must be an integer, not %s", nameof(p1));
				return (NLNIL);
			}
			(void) put(1, op + (width(p1) >> 2));
			return (nl+TCHAR);
		case O_CARD:
			if (isnta(p1, "t")) {
			    error("Argument to card must be a set, not %s", nameof(p1));
			    return (NLNIL);
			}
			(void) put(2, O_CARD, width(p1));
			return (nl+T2INT);
		case O_EOLN:
			if (!text(p1)) {
				error("Argument to eoln must be a text file, not %s", nameof(p1));
				return (NLNIL);
			}
			(void) put(1, op);
			return (nl+TBOOL);
		case O_EOF:
			if (p1->class != FILET) {
				error("Argument to eof must be file, not %s", nameof(p1));
				return (NLNIL);
			}
			(void) put(1, op);
			return (nl+TBOOL);
	}
}
#endif OBJ
