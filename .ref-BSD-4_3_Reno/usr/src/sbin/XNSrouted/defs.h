/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)defs.h	5.8 (Berkeley) 6/1/90
 */

#include <sys/types.h>
#include <sys/socket.h>

#include <net/route.h>
#include <netns/ns.h>
#include <netns/idp.h>
#if defined(vax) || defined(pdp11)
#define xnnet(x) ((u_long) (x)->rip_dst[1] << 16 | (u_long) (x)->rip_dst[0] )
#else
#define xnnet(x) ((u_long) (x)->rip_dst[0] << 16 | (u_long) (x)->rip_dst[1] )
#endif
#define	IDPPORT_RIF	1

#include <stdio.h>
#include <syslog.h>

#include "protocol.h"
#include "trace.h"
#include "interface.h"
#include "table.h"
#include "af.h"


/*
 * When we find any interfaces marked down we rescan the
 * kernel every CHECK_INTERVAL seconds to see if they've
 * come up.
 */
#define	CHECK_INTERVAL	(5*60)

#define equal(a1, a2) \
	(bcmp((caddr_t)(a1), (caddr_t)(a2), sizeof (struct sockaddr)) == 0)
#define	min(a,b)	((a)>(b)?(b):(a))

struct	sockaddr_ns addr;	/* Daemon's Address */
int	s;			/* Socket to listen on */
int	kmem;
int	supplier;		/* process should supply updates */
int	install;		/* if 1 call kernel */
int	lookforinterfaces;	/* if 1 probe kernel for new up interfaces */
int	performnlist;		/* if 1 check if /vmunix has changed */
int	externalinterfaces;	/* # of remote and local interfaces */
int	timeval;		/* local idea of time */
int	noteremoterequests;	/* squawk on requests from non-local nets */

char	packet[MAXPACKETSIZE+sizeof(struct idp)+1];
struct	rip *msg;

char	**argv0;

extern	int errno;

char	*malloc();
int	exit();
int	sendmsg();
int	supply();
int	timer();
int	cleanup();
