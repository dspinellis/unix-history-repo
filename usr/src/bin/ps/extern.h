/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

#include <sys/cdefs.h>

extern VAR var[];
extern struct varent *vhead, *vtail;

__BEGIN_DECLS
void	 err __P((const char *, ...));
__END_DECLS
