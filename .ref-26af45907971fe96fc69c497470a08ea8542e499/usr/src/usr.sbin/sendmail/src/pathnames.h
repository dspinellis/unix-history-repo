/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pathnames.h	6.5 (Berkeley) %G%
 */

#ifndef _PATH_SENDMAILCF
# define _PATH_SENDMAILCF	"/etc/sendmail.cf"
#endif

#ifndef _PATH_SENDMAILFC
# define _PATH_SENDMAILFC	"/etc/sendmail.fc"
#endif

#ifndef _PATH_SENDMAILPID
# ifdef BSD4_4
#  define _PATH_SENDMAILPID	"/var/run/sendmail.pid"
# else
#  define _PATH_SENDMAILPID	"/etc/sendmail.pid"
# endif
#endif
