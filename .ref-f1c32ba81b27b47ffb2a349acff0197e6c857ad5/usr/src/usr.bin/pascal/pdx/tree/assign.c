/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)assign.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*
 * assign the value of an expression to a variable (or term)
 */

#include "defs.h"
#include "tree.h"
#include "sym.h"
#include "process.h"
#include "tree.rep"
#include "process/process.rep"
#include "process/pxinfo.h"

assign(var, exp)
NODE *var;
NODE *exp;
{
	ADDRESS addr;
	int varsize;
	char cvalue;
	short svalue;
	long lvalue;

	if (!compatible(var->nodetype, exp->nodetype)) {
		error("incompatible types");
	}
	addr = lval(var);
	eval(exp);
	varsize = size(var->nodetype);
	if (varsize < sizeof(long)) {
		lvalue = pop(long);
		switch (varsize) {
			case sizeof(char):
				cvalue = lvalue;
				dwrite(&cvalue, addr, varsize);
				break;

			case sizeof(short):
				svalue = lvalue;
				dwrite(&svalue, addr, varsize);
				break;

			default:
				goto othersize;
				/*
				panic("bad size %d", varsize);
				*/
		}
	} else {
	    othersize:
		sp -= varsize;
		dwrite(sp, addr, varsize);
#ifdef tahoe
		downalignstack();
#endif
	}
}
