/*
 * Copyright (c) University of British Columbia, 1984
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Laboratory for Computation Vision and the Computer Science Department
 * of the University of British Columbia.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)x25acct.h	7.2 (Berkeley) %G%
 */

/*
 * Format of X.25 accounting record written
 * to X25ACCTF whenever a circuit is closed.
 */

#ifdef waterloo
#define X25ACCTF	"/usr/adm/logs/x25acct"
#else
#define X25ACCTF	"/usr/adm/x25acct"
#endif

struct	x25acct {
	time_t	x25acct_stime;		/* start time */
#ifdef waterloo
	u_long	x25acct_etime;		/* elapsed time (seconds) */
#else
	u_short	x25acct_etime;		/* elapsed time (seconds) */
#endif
	short	x25acct_uid;		/* user id */
	short	x25acct_net;		/* network id */
	u_short	x25acct_psize:4,	/* packet size */
		x25acct_addrlen:4,	/* x25acct_addr length */
		x25acct_revcharge:1,	/* reverse charging */
		x25acct_callin:1,	/* incoming call */
		x25acct_unused:6;
	char	x25acct_addr[8];	/* remote DTE address (in bcd) */
	char	x25acct_udata[4];	/* protocol id */
	long	x25acct_txcnt;		/* packets transmitted */
	long	x25acct_rxcnt;		/* packets received */
};
