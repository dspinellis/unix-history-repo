/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)chpass.h	5.2 (Berkeley) %G%
 */

struct entry {
	char *prompt;
	int (*func)(), restricted, len;
	char *except, *save;
};

extern uid_t uid;
