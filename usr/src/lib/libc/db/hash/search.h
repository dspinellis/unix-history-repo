/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)search.h	8.1 (Berkeley) %G%
 */

/* Backward compatibility to hsearch interface. */
typedef struct entry {
	char *key;
	char *data;
} ENTRY;

typedef enum {
	FIND, ENTER
} ACTION;

int	 hcreate __P((unsigned int));
void	 hdestroy __P((void));
ENTRY	*hsearch __P((ENTRY, ACTION));
