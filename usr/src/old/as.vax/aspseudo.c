/*
 *	Copyright (c) 1982 Regents of the University of California
 */
#ifndef lint
static char sccsid[] = "@(#)aspseudo.c 4.4 %G%";
#endif not lint

#include <stdio.h>
#include "as.h"

#define	OP(name, eopcode, popcode, nargs, arg1, arg2, arg3, arg4, arg5, arg6) \
	{ \
		name, popcode, nargs, arg1, arg2, arg3, arg4, arg5, arg6, \
		(nargs == 0 ? INST0:INSTn), eopcode \
	}
#define	PSEUDO(name, type, tag) \
	{ \
		name, type, 0,   0, 0, 0, 0, 0, 0, \
		tag, CORE \
	}

readonly struct Instab instab[] = {
#include "instrs.as"
0
};
