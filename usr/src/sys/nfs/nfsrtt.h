/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nfsrtt.h	7.1 (Berkeley) %G%
 */

/*
 * Definitions for client side performance monitor.
 */
#define	NFSRTTLOGSIZ	128
struct nfsrtt {
	int pos;
	struct rttl {
		int	proc;
		int	rtt;
		int	rto;
		int	sent;
		int	cwnd;
		int	srtt;
		int	sdrtt;
		fsid_t	fsid;
		struct timeval tstamp;
	} rttl[NFSRTTLOGSIZ];
};
