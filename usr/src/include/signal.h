/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)signal.h	5.1 (Berkeley) %G%
 */

#include <sys/signal.h>

#if !defined(_ANSI_SOURCE) && !defined(_POSIX_SOURCE)
extern char *sys_signame[NSIG];
extern char *sys_siglist[NSIG];
#endif
