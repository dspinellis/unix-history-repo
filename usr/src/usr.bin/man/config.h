/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)config.h	8.2 (Berkeley) %G%
 */

typedef struct _entry {
	struct queue_entry	list;		/* List of items. */
	struct queue_entry	tags;		/* List of tags. */
	char *s;				/* Associated string. */
	size_t len;				/* Length of 's'. */
} ENTRY;

extern struct queue_entry head;

ENTRY	*addlist __P((char *));
void	 config __P((char *));
void	 debug __P((char *));
ENTRY	*getlist __P((char *));
