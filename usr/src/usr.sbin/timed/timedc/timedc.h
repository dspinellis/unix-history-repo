/*-
 * Copyright (c) 1985, 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)timedc.h	5.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/time.h>
#ifdef sgi
#include <sys/uio.h>
#endif
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <errno.h>
#include <netdb.h>
#include <stdio.h>

extern int errno;

#define ON		1
#define OFF		0

#define GOOD		1
#define UNREACHABLE	2
#define NONSTDTIME	3
#define HOSTDOWN 	0x7fffffff

struct	cmd {
	char	*c_name;		/* command name */
	char	*c_help;		/* help message */
	void	(*c_handler)();		/* routine to do the work */
	int	c_priv;			/* privileged command */
};

#include "extern.h"
