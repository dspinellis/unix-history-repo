/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pathnames.h	5.4 (Berkeley) %G%
 */

#ifndef COMPAT
#include <paths.h>
#else
#define	_PATH_DEVNULL	"/dev/null"
#endif

#define	_PATH_ACCESS	"/etc/sliphome/slip.hosts"
#define	_PATH_LOGIN	"/etc/sliphome/slip.login"
#define	_PATH_LOGOUT	"/etc/sliphome/slip.logout"
#define	_PATH_DEBUG	"/tmp/sliplogin.XXXXXX"

