/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)telnetd.h	8.1 (Berkeley) %G%
 */


#include "defs.h"
#include "ext.h"

#ifdef	DIAGNOSTICS
#define	DIAG(a,b)	if (diagnostic & (a)) b
#else
#define	DIAG(a,b)
#endif

/* other external variables */
extern	char **environ;
extern	int errno;

