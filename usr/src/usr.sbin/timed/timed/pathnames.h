/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#include <paths.h>

#ifdef sgi
#define	_PATH_MASTERLOG	"/usr/adm/timed.masterlog"
#define	_PATH_TIMEDLOG	"/usr/adm/timed.log"
#else
#define	_PATH_MASTERLOG	"/var/log/timed.masterlog"
#define	_PATH_TIMEDLOG	"/var/log/timed.log"
#endif
