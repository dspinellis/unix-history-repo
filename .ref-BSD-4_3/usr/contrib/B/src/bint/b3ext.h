/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3ext.h,v 1.4 85/08/22 16:43:52 timo Exp $
*/

#define MAXEARGS 10

typedef struct ext {
	string e_name;
	string e_args[MAXEARGS];
	int (*e_exec)(); /* should be void, but portability... */
} ext;

extern ext extensions[];
