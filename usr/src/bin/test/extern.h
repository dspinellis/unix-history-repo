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
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)extern.h	1.1 (Berkeley) %G%";
#endif /* not lint */


#include <sys/cdefs.h>

__BEGIN_DECLS
void error __P((char *));
void *stalloc __P((int));

__END_DECLS
