/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)eprintf.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>

/* This is used by the `assert' macro.  */
void
__eprintf(string, line, filename)
	char *string;
	int line;
	char *filename;
{
	(void)fprintf(stderr, string, line, filename);
}
