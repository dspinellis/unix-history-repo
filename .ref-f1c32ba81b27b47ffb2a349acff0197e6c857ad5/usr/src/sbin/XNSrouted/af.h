/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)af.h	5.1 (Berkeley) 6/4/85 (routed/af.h)
 *
 *	@(#)af.h	5.2 (Berkeley) %G%
 */

/*
 * Routing table management daemon.
 */

/*
 * Per address family routines.
 */
struct afswitch {
	int	(*af_hash)();		/* returns keys based on address */
	int	(*af_netmatch)();	/* verifies net # matching */
	int	(*af_output)();		/* interprets address for sending */
	int	(*af_portmatch)();	/* packet from some other router? */
	int	(*af_portcheck)();	/* packet from privileged peer? */
	int	(*af_checkhost)();	/* tells if address for host or net */
	int	(*af_ishost)();		/* tells if address is valid */
	int	(*af_canon)();		/* canonicalize address for compares */
};

/*
 * Structure returned by af_hash routines.
 */
struct afhash {
	u_int	afh_hosthash;		/* host based hash */
	u_int	afh_nethash;		/* network based hash */
};

struct	afswitch afswitch[AF_MAX];	/* table proper */
