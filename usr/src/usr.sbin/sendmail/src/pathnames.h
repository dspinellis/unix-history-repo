/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pathnames.h	6.4 (Berkeley) %G%
 */

#define	_PATH_SENDMAILCF	"/etc/sendmail.cf"
#define	_PATH_SENDMAILFC	"/etc/sendmail.fc"
#ifndef _PATH_SENDMAILPID
# ifdef BSD4_4
#  define _PATH_SENDMAILPID	"/var/run/sendmail.pid"
# else
#  define _PATH_SENDMAILPID	"/etc/sendmail.pid"
# endif
#endif
