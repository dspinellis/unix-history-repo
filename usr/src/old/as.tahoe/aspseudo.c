/*
 *	Copyright (c) 1982 Regents of the University of California
 */
#ifndef lint
static char sccsid[] = "@(#)aspseudo.c 4.5 6/30/83";
#endif not lint

#include <stdio.h>
#include "as.h"

#define	OP(name, opcode, nargs, arg1, arg2, arg3, arg4, arg5, arg6) \
	{ \
		name, opcode, nargs, arg1, arg2, arg3, arg4, arg5, arg6, \
		(nargs == 0 ? INST0:INSTn) \
	}
#define	PSEUDO(name, type, tag) \
	{ \
		name, type, 0,   0, 0, 0, 0, 0, 0, \
		tag \
	}

readonly struct Instab instab[] = {
#include "instrs.as"
PSEUDO("\0\0\0\0\0\0\0\0\0\0", 0, 0)
};
