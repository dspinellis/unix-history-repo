/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3tra.c,v 1.4 85/08/22 16:59:43 timo Exp $
*/

/* Trace facility for interpreter */

#include "b.h"
#include "b0fea.h"
#include "b1obj.h"
#include "b2nod.h"
#include "b3err.h" /* For 'tracing' */
#include "b3int.h"

#ifdef EXT_COMMAND

Visible Procedure tr_on() {
	tracing= Yes;
}

Visible Procedure tr_off() {
	tracing= No;
}

#endif EXT_COMMAND

Visible string opcodes[] = {
	"HOW_TO",
	"YIELD",
	"TEST",
	"REFINEMENT",

/* Commands */

	"SUITE",
	"PUT",
	"INSERT",
	"REMOVE",
	"CHOOSE",
	"DRAW",
	"SET_RANDOM",
	"DELETE",
	"CHECK",
	"SHARE",

	"WRITE",
	"READ",
	"READ_RAW",

	"IF",
	"WHILE",
	"FOR",

	"SELECT",
	"TEST_SUITE",
	"ELSE",

	"QUIT",
	"RETURN",
	"REPORT",
	"SUCCEED",
	"FAIL",

	"USER_COMMAND",
	"EXTENDED_COMMAND",

/* Expressions, targets, tests */

	"TAG",
	"COMPOUND",

/* Expressions, targets */

	"COLLATERAL",
	"SELECTION",
	"BEHEAD",
	"CURTAIL",

/* Expressions, tests */

	"UNPARSED",

/* Expressions */

	"MONF",
	"DYAF",
	"NUMBER",
	"TEXT_DIS",
	"TEXT_LIT",
	"TEXT_CONV",
	"ELT_DIS",
	"LIST_DIS",
	"RANGE_DIS",
	"TAB_DIS",

/* Tests */

	"AND",
	"OR",
	"NOT",
	"SOME_IN",
	"EACH_IN",
	"NO_IN",
	"SOME_PARSING",
	"EACH_PARSING",
	"NO_PARSING",
	"MONPRD",
	"DYAPRD",
	"LESS_THAN",
	"AT_MOST",
	"GREATER_THAN",
	"AT_LEAST",
	"EQUAL",
	"UNEQUAL",
	"Nonode",

	"TAGformal",
	"TAGlocal",
	"TAGglobal",
	"TAGmystery",
	"TAGrefinement",
	"TAGzerfun",
	"TAGzerprd",
};

#define NOPCODES ((sizeof opcodes) / (sizeof opcodes[0]))

Visible Procedure tr_node(p) parsetree p; {
	int n;
	fprintf(stderr, "*** %8x ", p);
	if (p == Halt)
		fprintf(stderr, "Halt\r\n");
	else if (p == Stop)
		fprintf(stderr, "Stop\r\n");
	else if (!Is_parsetree(p)) {
		if (IsSmallInt(p))
			fprintf(stderr, "Error %d\r\n", SmallIntVal(p));
		else
			fprintf(stderr, "Trace bad node\r\n");
	}
	else {
		n= Nodetype(p);
		if (n < 0 || n >= NOPCODES)
			fprintf(stderr, "Opcode %d", n);
		else
			fprintf(stderr, "%s", opcodes[n]);
		if (Thread2(p))
			fprintf(stderr, " [*]");
		fprintf(stderr, "\r\n");
	}
}

Visible Procedure tr_jump() {
	fprintf(stderr, "*** Jump\r\n");
}

Visible Procedure tr_call() {
	fprintf(stderr, "*** Call\r\n");
}

Visible Procedure tr_ret() {
	fprintf(stderr, "*** Return\r\n");
}
