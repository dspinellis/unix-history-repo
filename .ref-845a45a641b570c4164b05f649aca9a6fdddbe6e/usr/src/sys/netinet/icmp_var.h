/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)icmp_var.h	7.7 (Berkeley) %G%
 */

/*
 * Variables related to this implementation
 * of the internet control message protocol.
 */
struct	icmpstat {
/* statistics related to icmp packets generated */
	u_long	icps_error;		/* # of calls to icmp_error */
	u_long	icps_oldshort;		/* no error 'cuz old ip too short */
	u_long	icps_oldicmp;		/* no error 'cuz old was icmp */
	u_long	icps_outhist[ICMP_MAXTYPE + 1];
/* statistics related to input messages processed */
 	u_long	icps_badcode;		/* icmp_code out of range */
	u_long	icps_tooshort;		/* packet < ICMP_MINLEN */
	u_long	icps_checksum;		/* bad checksum */
	u_long	icps_badlen;		/* calculated bound mismatch */
	u_long	icps_reflect;		/* number of responses */
	u_long	icps_inhist[ICMP_MAXTYPE + 1];
};

/*
 * Names for ICMP sysctl objects
 */
#define	ICMPCTL_MASKREPL	1	/* allow replies to netmask requests */
#define ICMPCTL_MAXID		2

#define ICMPCTL_NAMES { \
	{ 0, 0 }, \
	{ "maskrepl", CTLTYPE_INT }, \
}

#ifdef KERNEL
struct	icmpstat icmpstat;
#endif
