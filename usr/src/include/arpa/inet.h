/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)inet.h	5.6 (Berkeley) %G%
 */

/* External definitions for functions in inet(3) */

#include <sys/cdefs.h>

__BEGIN_DECLS
extern unsigned long	 inet_addr __P((const char *));
extern unsigned long	 inet_lnaof __P((struct in_addr));
extern struct in_addr	 inet_makeaddr __P((u_long , u_long));
extern unsigned long	 inet_netof __P((struct in_addr));
extern unsigned long	 inet_network __P((const char *));
extern char		*inet_ntoa __P((struct in_addr));
__END_DECLS
