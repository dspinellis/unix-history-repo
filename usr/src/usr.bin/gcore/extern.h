/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

void	err __P((int, const char *, ...));
void	md_core __P((kvm_t *, int, struct kinfo_proc *));
