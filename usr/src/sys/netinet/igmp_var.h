/*
 * Copyright (c) 1988 Stephen Deering.
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Stephen Deering of Stanford University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)igmp_var.h	7.2 (Berkeley) %G%
 */

/*
 * Internet Group Management Protocol (IGMP),
 * implementation-specific definitions.
 *
 * Written by Steve Deering, Stanford, May 1988.
 *
 * MULTICAST 1.1
 */

struct igmpstat {
	u_long	igps_rcv_total;		/* total IGMP messages received */
	u_long	igps_rcv_tooshort;	/* received with too few bytes */
	u_long	igps_rcv_badsum;	/* received with bad checksum */
	u_long	igps_rcv_queries;	/* received membership queries */
	u_long	igps_rcv_badqueries;	/* received invalid queries */
	u_long	igps_rcv_reports;	/* received membership reports */
	u_long	igps_rcv_badreports;	/* received invalid reports */
	u_long	igps_rcv_ourreports;	/* received reports for our groups */
	u_long	igps_snd_reports;	/* sent membership reports */
};

#ifdef KERNEL
struct igmpstat igmpstat;

/*
 * Macro to compute a random timer value between 1 and (IGMP_MAX_REPORTING_
 * DELAY * countdown frequency).  We generate a "random" number by adding
 * the total number of IP packets received, our primary IP address, and the
 * multicast address being timed-out.  The 4.3 random() routine really
 * ought to be available in the kernel!
 */
#define IGMP_RANDOM_DELAY(multiaddr) \
	/* struct in_addr multiaddr; */ \
	( (ipstat.ips_total + \
	   ntohl(IA_SIN(in_ifaddr)->sin_addr.s_addr) + \
	   ntohl((multiaddr).s_addr) \
	  ) \
	  % (IGMP_MAX_HOST_REPORT_DELAY * PR_FASTHZ) + 1 \
	)

void	igmp_init __P(());
void	igmp_input __P((struct mbuf *, struct ifnet *));
void	igmp_joingroup __P((struct in_multi *));
void	igmp_leavegroup __P((struct in_multi *));
void	igmp_fasttimo __P(());
#endif
