/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)opinfo.c	5.1 (Berkeley) 6/6/85";
#endif not lint

/*
 * Operator information structure.
 */

#include "defs.h"
#include "opinfo.h"

OPINFO opinfo[] ={
/* O_NOP */		0,		0,
/* O_NAME */		LEAF,		0,
/* O_QNAME */		LEAF,		"$",
/* O_LCON */		LEAF,		0,
/* O_FCON */		LEAF,		0,
/* O_SCON */		LEAF,		0,
/* O_INDEX */		UNARY,		0,
/* O_INDIR */		UNARY,		"^",
/* O_RVAL */		UNARY,		0,
/* O_COMMA */		BINARY,		",",
/* O_ITOF */		UNARY|INTOP,	0,
/* O_ADD */		BINARY|INTOP,	"+",
/* O_ADDF */		BINARY|REALOP,	"+",
/* O_SUB */		BINARY|INTOP,	"-",
/* O_SUBF */		BINARY|REALOP,	"-",
/* O_NEG */		UNARY|INTOP,	"-",
/* O_NEGF */		UNARY|REALOP,	"-",
/* O_MUL */		BINARY|INTOP,	"*",
/* O_MULF */		BINARY|REALOP,	"*",
/* O_DIVF */		BINARY|REALOP,	"/",
/* O_DIV */		BINARY|INTOP,	" div ",
/* O_MOD */		BINARY|INTOP,	" mod ",
/* O_AND */		BINARY|INTOP,	" and ",
/* O_OR */		BINARY|INTOP,	" or ",
/* O_LT */		BINARY|INTOP,	" < ",
/* O_LTF */		BINARY|REALOP,	" < ",
/* O_LE */		BINARY|INTOP,	" <= ",
/* O_LEF */		BINARY|REALOP,	" <= ",
/* O_GT */		BINARY|INTOP,	" > ",
/* O_GTF */		BINARY|REALOP,	" > ",
/* O_GE */		BINARY|INTOP,	" >= ",
/* O_GEF */		BINARY|REALOP,	" >= ",
/* O_EQ */		BINARY|INTOP,	" = ",
/* O_EQF */		BINARY|REALOP,	" = ",
/* O_NE */		BINARY|INTOP,	" <> ",
/* O_NEF */		BINARY|REALOP,	" <> ",
/* O_ASSIGN */		BINARY,		" := ",
/* O_CHFILE */		0,		NIL,
/* O_CONT */		0,		NIL,
/* O_LIST */		0,		NIL,
/* O_NEXT */		0,		NIL,
/* O_PRINT */		0,		NIL,
/* O_STEP */		0,		NIL,
/* O_WHATIS */		0,		NIL,
/* O_WHERE */		0,		NIL,
/* O_XI */			0,		NIL,
/* O_XD */			0,		NIL,
/* O_CALL */		0,		NIL,
/* O_EDIT */		0,		NIL,
/* O_DUMP */		0,		NIL,
/* O_HELP */		0,		NIL,
/* O_REMAKE */		0,		NIL,
/* O_RUN */		0,		NIL,
/* O_SOURCE */		0,		NIL,
/* O_STATUS */		0,		NIL,
/* O_TRACE */		0,		NIL,
/* O_TRACEI */		0,		NIL,
/* O_STOP */		0,		NIL,
/* O_STOPI */		0,		NIL,
/* O_DELETE */		0,		NIL,
/* O_WHICH */		0,		NIL,
/* O_QLINE */		LEAF,		NIL,
/* O_ALIAS */		LEAF,		NIL,
/* O_GRIPE */		0,		NIL,
};
