/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)inet.h	5.4 (Berkeley) %G%
 */

/* External definitions for functions in inet(3) */

#ifdef __STDC__
extern unsigned long inet_addr(const char *);
extern char *inet_ntoa(struct in_addr);
extern struct in_addr inet_makeaddr(int , int);
extern unsigned long inet_network(const char *);
extern unsigned long inet_lnaof(struct in_addr);
extern unsigned long inet_netof(struct in_addr);
#else
extern unsigned long inet_addr();
extern char *inet_ntoa();
extern struct in_addr inet_makeaddr();
extern unsigned long inet_network();
extern unsigned long inet_lnaof();
extern unsigned long inet_netof();
#endif
