/*-
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pathnames.h	8.1 (Berkeley) %G%
 */

#include <paths.h>

#ifdef sgi
#define	_PATH_MASTERLOG	"/usr/adm/timed.masterlog"
#define	_PATH_TIMEDLOG	"/usr/adm/timed.log"
#else
#define	_PATH_MASTERLOG	"/var/log/timed.masterlog"
#define	_PATH_TIMEDLOG	"/var/log/timed.log"
#endif
