/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pathnames.h	8.2 (Berkeley) %G%
 */

#ifdef DEBUG
#define	_PATH_MANCONF	"./man.conf"
#else
#define	_PATH_MANCONF	"/etc/man.conf"
#endif
#define	_PATH_PAGER	"/usr/bin/more -s"
#define	_PATH_WHATIS	"whatis.db"
#define	_PATH_TMP	"/tmp/man.XXXXXX"
