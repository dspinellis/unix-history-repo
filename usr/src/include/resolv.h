
/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)resolv.h	5.3 (Berkeley) %G%
 */

/*
 * Global defines and variables for resolver stub.
 */


#define	MAXNS		3		/* max # name servers we'll track */


struct state {
	int	retrans;	 	/* retransmition time interval */
	int	retry;			/* number of times to retransmit */
	int	options;		/* option flags - see below. */
	int	nscount;		/* number of name servers */
	struct	sockaddr_in nsaddr_list[MAXNS];	/* address of name server */
#define	nsaddr	nsaddr_list[0]		/* for backward compatibility */
	u_short	id;			/* current packet id */
	char	defdname[MAXDNAME];	/* default domain */
};

/*
 * Resolver options
 */
#define RES_INIT	0x001		/* address initialized */
#define RES_DEBUG	0x002		/* print debug messages */
#define RES_AAONLY	0x004		/* authoritative answers only */
#define RES_USEVC	0x008		/* use virtual circuit */
#define RES_PRIMARY	0x010		/* query primary server only */
#define RES_IGNTC	0x020		/* ignore trucation errors */
#define RES_RECURSE	0x040		/* recursion desired */
#define RES_DEFNAMES	0x080		/* use default domain name */

extern struct state _res;
extern char *p_cdname(), *p_rr(), *p_type(), *p_class();
