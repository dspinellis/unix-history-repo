/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)htable.h	5.2 (Berkeley) %G%
 */

#include <sys/types.h>
#include <netinet/in.h>

/*
 * common definitions for htable
 */

struct addr {
	struct	in_addr addr_val;
	struct	addr *addr_link;
};

struct name {
	char	*name_val;
	struct	name *name_link;
};

struct gateway {
	struct	gateway *g_link;
	struct	gateway *g_dst;		/* connected gateway if metric > 0 */
	struct	gateway *g_firstent;	/* first entry for this gateway */
	struct	name	*g_name;
	int	g_net;
	struct	in_addr g_addr;		/* address on g_net */
	int	g_metric;		/* hops to this net */
};

#define	NOADDR			((struct addr *)0)
#define	NONAME			((struct name *)0)

#define	KW_NET		1
#define	KW_GATEWAY	2
#define	KW_HOST		3

struct name *newname();
char *malloc();

char *infile;			/* Input file name */
