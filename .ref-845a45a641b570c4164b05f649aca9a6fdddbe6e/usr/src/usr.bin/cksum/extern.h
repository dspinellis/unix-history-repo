/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

#include <sys/cdefs.h>

__BEGIN_DECLS
int	crc __P((int, unsigned long *, unsigned long *));
void	pcrc __P((char *, unsigned long, unsigned long));
void	psum1 __P((char *, unsigned long, unsigned long));
void	psum2 __P((char *, unsigned long, unsigned long));
int	csum1 __P((int, unsigned long *, unsigned long *));
int	csum2 __P((int, unsigned long *, unsigned long *));
__END_DECLS
