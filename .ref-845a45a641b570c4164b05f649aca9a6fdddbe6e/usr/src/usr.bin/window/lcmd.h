/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lcmd.h	3.9 (Berkeley) %G%
 */

#define LCMD_NARG 20			/* maximum number of arguments */

struct lcmd_tab {
	char *lc_name;
	int lc_minlen;
	int (*lc_func)();
	struct lcmd_arg *lc_arg;
};

struct lcmd_arg {
	char *arg_name;
	int arg_minlen;
	int arg_flags;
};

	/* arg_flags bits */
#define ARG_TYPE	0x0f		/* type of arg */
#define ARG_ANY		0x00		/* any type */
#define ARG_NUM		0x01		/* must be a number */
#define ARG_STR		0x02		/* must be a string */
#define ARG_LIST	0x10		/* this arg can be a list */

struct lcmd_tab *lcmd_lookup();
