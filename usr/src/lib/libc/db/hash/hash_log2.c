/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)hash_log2.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <db.h>
#include "hash.h"
#include "page.h"
#include "extern.h"

int
__log2(num)
	int num;
{
	register int i, limit;

	limit = 1;
	for (i = 0; limit < num; limit = limit << 1, i++);
	return (i);
}
