/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)config.h	8.3 (Berkeley) %G%
 */

typedef struct _tag {
	TAILQ_ENTRY(_tag) q;		/* Queue of tags. */

	TAILQ_HEAD(tqh, _entry) list;	/* Queue of entries. */
	char *s;			/* Associated string. */
	size_t len;			/* Length of 's'. */
} TAG;
typedef struct _entry {
	TAILQ_ENTRY(_entry) q;		/* Queue of entries. */

	char *s;			/* Associated string. */
	size_t len;			/* Length of 's'. */
} ENTRY;

TAILQ_HEAD(_head, _tag);
extern struct _head head;

TAG	*addlist __P((char *));
void	 config __P((char *));
void	 debug __P((char *));
TAG	*getlist __P((char *));
