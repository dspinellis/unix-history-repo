/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dir.h	5.5 (Berkeley) %G%
 */

/*
 * Structure for entries in directory stack.
 */
struct directory {
    struct directory *di_next;	/* next in loop */
    struct directory *di_prev;	/* prev in loop */
    unsigned short *di_count;	/* refcount of processes */
    Char   *di_name;		/* actual name */
};
struct directory *dcwd;		/* the one we are in now */
