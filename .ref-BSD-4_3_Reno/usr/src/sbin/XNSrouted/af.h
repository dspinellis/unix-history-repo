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
 *	@(#)af.h	5.1 (Berkeley) 6/4/85 (routed/af.h)
 *
 *	@(#)af.h	5.2 (Berkeley) 6/1/90
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
