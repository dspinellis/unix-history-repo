/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)stklval.c 1.1 8/27/80";

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"

/*
 * Lvalue computes the address
 * of a qualified name and
 * leaves it on the stack.
 */
struct nl *
stklval(r, modflag)
	int *r, modflag;
{
	/*
	 * For the purposes of the interpreter stklval
	 * is the same as an lvalue.
	 */

	return(lvalue(r, modflag , LREQ ));
}
