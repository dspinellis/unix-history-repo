/*-
 * Copyright (c) 1992 Keith Muller.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ftree.h	8.1 (Berkeley) %G%
 */

/*
 * Data structure used by the ftree.c routines to store the file args to be
 * handed to fts(). It keeps a reference count of which args generated a
 * "selected" member
 */

typedef struct ftree {
	char		*fname;		/* file tree name */
	int		refcnt;		/* has tree had a selected file? */
	struct ftree	*fow;		/* pointer to next entry on list */
} FTREE;
