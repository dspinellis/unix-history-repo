/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)rand.c	5.7 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <stdlib.h>

static u_long next = 1;

int
rand()
{
	return ((next = next * 1103515245 + 12345) % ((u_long)RAND_MAX + 1));
}

void
srand(seed)
u_int seed;
{
	next = seed;
}
