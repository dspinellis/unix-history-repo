/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pathnames.h	5.3 (Berkeley) %G%
 */

#ifndef COMPAT
#include <paths.h>
#else
#define	_PATH_DEVNULL	"/dev/null"
#endif

#define	_PATH_ACCESS	"/etc/slip.hosts"
#define	_PATH_LOGIN	"/etc/slip.login"
#define	_PATH_LOGOUT	"/etc/slip.logout"
#define	_PATH_DEBUG	"/tmp/sliplogin.XXXXXX"

