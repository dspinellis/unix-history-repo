/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pathnames.h	5.5 (Berkeley) %G%
 */

#if BSD > 43

# include <paths.h>

# define	_PATH_LOGIN	"/usr/bin/login"

#else
 
# define	_PATH_TTY	"/dev/tty"
# define	_PATH_LOGIN	"/bin/login"

#endif

#ifdef BFTPDAEMON
#define		BFTPPATH	"/usr/ucb/bftp"
#endif  /* BFTPDAEMON */
