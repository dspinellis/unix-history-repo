/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)inet.h	5.8 (Berkeley) %G%
 */

#ifndef _INET_H_
#define	_INET_H_

/* External definitions for functions in inet(3) */

#include <sys/cdefs.h>

__BEGIN_DECLS
unsigned long	 inet_addr __P((const char *));
unsigned long	 inet_aton __P((const char *, struct in_addr *));
unsigned long	 inet_lnaof __P((struct in_addr));
struct in_addr	 inet_makeaddr __P((u_long , u_long));
unsigned long	 inet_netof __P((struct in_addr));
unsigned long	 inet_network __P((const char *));
char		*inet_ntoa __P((struct in_addr));
__END_DECLS

#endif /* !_INET_H_ */
