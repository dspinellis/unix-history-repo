/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)stklval.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#include "tree_ty.h"

/*
 * Lvalue computes the address
 * of a qualified name and
 * leaves it on the stack.
 */
struct nl *
stklval(r, modflag)
	struct  tnode *r;
	int	modflag;
{
	/*
	 * For the purposes of the interpreter stklval
	 * is the same as an lvalue.
	 */

	return(lvalue(r, modflag , LREQ ));
}
