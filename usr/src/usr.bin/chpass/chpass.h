/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)chpass.h	5.3 (Berkeley) %G%
 */

typedef struct _entry {
	char *prompt;
	int (*func)(), restricted, len;
	char *except, *save;
} ENTRY;

/* Field numbers. */
#define	E_BPHONE	8
#define	E_HPHONE	9
#define	E_LOCATE	10
#define	E_NAME		7
#define	E_SHELL		12

extern ENTRY list[];
extern uid_t uid;
