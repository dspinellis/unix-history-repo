/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)optab.h 1.1 %G%";

/*
 * px opcode information structure
 */

#define MAXNARGS 10

/*
 * argument types
 */

typedef int ARGTYPE;

#define ADDR2	 1
#define ADDR4	 2
#define DISP	 3
#define PSUBOP	 5
#define SUBOP	 6
#define VLEN	 7
#define HWORD	 8
#define LWORD	 9
#define STRING	10

typedef struct {
	char *opname;
	ARGTYPE argtype[MAXNARGS];
} OPTAB;

OPTAB optab[];
