/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)error.c	1.3 (Berkeley) %G%";
#endif /* not lint */


#include <stdio.h>
#include <errno.h>

void
#ifdef __STDC__
error(char *msg, ...) {
#else
error(msg)
      char *msg;
      {
#endif
      fprintf(stderr, "test: %s\n", msg);
      exit(2);
}
