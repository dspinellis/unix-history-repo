/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)func.c 1.3 10/19/80";

#include "whoami.h"
#ifdef OBJ
    /*
     *	the rest of the file
     */
#include "0.h"
#include "tree.h"
#include "opcode.h"

/*
 * Funccod generates code for
 * built in function calls and calls
 * call to generate calls to user
 * defined functions and procedures.
 */
funccod(r)
	int *r;
{
	struct nl *p;
	register struct nl *p1;
	register int *al;
	register op;
	int argc, *argv;
	int tr[2], tr2[4];

	/*
	 * Verify that the given name
	 * is defined and the name of
	 * a function.
	 */
	p = lookup(r[2]);
	if (p == NIL) {
		rvlist(r[3]);
		return (NIL);
	}
	if (p->class != FUNC && p->class != FFUNC) {
		error("%s is not a function", p->symbol);
		rvlist(r[3]);
		return (NIL);
	}
	argv = r[3];
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
	for (al = argv; al != NIL; al = al[2])
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
				return (NIL);
			}
			put1(op);
			return (nl+T4INT);
		case O_EOF:
		case O_EOLN:
			if (argc == 0) {
				argv = tr;
				tr[1] = tr2;
				tr2[0] = T_VAR;
				tr2[2] = input->symbol;
				tr2[1] = tr2[3] = NIL;
				argc = 1;
			} else if (argc != 1) {
				error("%s takes either zero or one argument", p->symbol);
				rvlist(argv);
				return (NIL);
			}
		}
	/*
	 * All other functions take
	 * exactly one argument.
	 */
	if (argc != 1) {
		error("%s takes exactly one argument", p->symbol);
		rvlist(argv);
		return (NIL);
	}
	/*
	 * Evaluate the argmument
	 */
	p1 = stkrval((int *) argv[1], NLNIL , RREQ );
	if (p1 == NIL)
		return (NIL);
	switch (op) {
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
				convert(p1, nl+TDOUBLE);
			else if (isnta(p1, "d")) {
				error("%s's argument must be integer or real, not %s", p->symbol, nameof(p1));
				return (NIL);
			}
			put1(op);
			if (op == O_UNDEF)
				return (nl+TBOOL);
			else if (op == O_EXPO)
				return (nl+T4INT);
			else
				return (nl+TDOUBLE);
		case O_SEED:
			if (isnta(p1, "i")) {
				error("seed's argument must be an integer, not %s", nameof(p1));
				return (NIL);
			}
			put1(op);
			return (nl+T4INT);
		case O_ROUND:
		case O_TRUNC:
			if (isnta(p1, "d"))  {
				error("%s's argument must be a real, not %s", p->symbol, nameof(p1));
				return (NIL);
			}
			put1(op);
			return (nl+T4INT);
		case O_ABS2:
		case O_SQR2:
			if (isa(p1, "d")) {
				put1(op + O_ABS8-O_ABS2);
				return (nl+TDOUBLE);
			}
			if (isa(p1, "i")) {
				put1(op + (width(p1) >> 2));
				return (nl+T4INT);
			}
			error("%s's argument must be an integer or real, not %s", p->symbol, nameof(p1));
			return (NIL);
		case O_ORD2:
			if (isa(p1, "bcis") || classify(p1) == TPTR) {
				return (nl+T4INT);
			}
			error("ord's argument must be of scalar type or a pointer, not %s", nameof(p1));
			return (NIL);
		case O_SUCC2:
		case O_PRED2:
			if (isa(p1, "bcs")) {
				put1(op);
				return (p1);
			}
			if (isa(p1, "i")) {
				if (width(p1) <= 2)
					op += O_PRED24-O_PRED2;
				else
					op++;
				put1(op);
				return (nl+T4INT);
			}
			if (isa(p1, "id")) {
				error("%s is forbidden for reals", p->symbol);
				return (NIL);
			}
			error("%s's argument must be of scalar type, not %s", p->symbol, nameof(p1));
			return (NIL);
		case O_ODD2:
			if (isnta(p1, "i")) {
				error("odd's argument must be an integer, not %s", nameof(p1));
				return (NIL);
			}
			put1(op + (width(p1) >> 2));
			return (nl+TBOOL);
		case O_CHR2:
			if (isnta(p1, "i")) {
				error("chr's argument must be an integer, not %s", nameof(p1));
				return (NIL);
			}
			put1(op + (width(p1) >> 2));
			return (nl+TCHAR);
		case O_CARD:
			if (isnta(p1, "t")) {
			    error("Argument to card must be a set, not %s", nameof(p1));
			    return (NIL);
			}
			put2(O_CARD, width(p1));
			return (nl+T2INT);
		case O_EOLN:
			if (!text(p1)) {
				error("Argument to eoln must be a text file, not %s", nameof(p1));
				return (NIL);
			}
			put1(op);
			return (nl+TBOOL);
		case O_EOF:
			if (p1->class != FILET) {
				error("Argument to eof must be file, not %s", nameof(p1));
				return (NIL);
			}
			put1(op);
			return (nl+TBOOL);
		case 0:
			error("%s is an unimplemented 6000-3.4 extension", p->symbol);
		default:
			panic("func1");
	}
}
#endif OBJ
