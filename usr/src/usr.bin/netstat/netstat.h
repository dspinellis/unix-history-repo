/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)netstat.h	5.5 (Berkeley) %G%
 */

#include <sys/cdefs.h>

int	Aflag;		/* show addresses of protocol control block */
int	aflag;		/* show all sockets (including servers) */
int	dflag;		/* show i/f dropped packets */
int	hflag;		/* show IMP host table */
int	iflag;		/* show interfaces */
int	Bflag;		/* show multicast tables (or multicast stats) */
int	mflag;		/* show memory stats */
int	nflag;		/* show addresses numerically */
int	pflag;		/* show given protocol */
int	rflag;		/* show routing tables (or routing stats) */
int	sflag;		/* show protocol statistics */
int	tflag;		/* show i/f watchdog timers */

int	interval;	/* repeat interval for i/f stats */

char	*interface;	/* desired i/f for stats, or NULL for all i/fs */
int	unit;		/* unit number for above */

int	af;		/* address family */

char	*prog;		/* program name */


int	kread __P((u_long addr, char *buf, int size));
char	*plural __P((int));
char	*plurales __P((int));

void	protopr __P((u_long, char *));
void	tcp_stats __P((u_long, char *));
void	udp_stats __P((u_long, char *));
void	ip_stats __P((u_long, char *));
void	icmp_stats __P((u_long, char *));
void	igmp_stats __P((u_long, char *));
void	protopr __P((u_long, char *));

void	mbpr(u_long);

void	hostpr __P((u_long, u_long));
void	impstats __P((u_long, u_long));

void	intpr __P((int, u_long));

void	pr_rthdr __P(());
void	pr_family __P((int));
void	rt_stats __P((u_long));
char	*ns_phost __P((struct sockaddr *));
void	upHex __P((char *));

char	*routename __P((u_long));
char	*netname __P((u_long, u_long));
char	*ns_print __P((struct sockaddr *));
void	routepr __P((u_long));

void	nsprotopr __P((u_long, char *));
void	spp_stats __P((u_long, char *));
void	idp_stats __P((u_long, char *));
void	nserr_stats __P((u_long, char *));

void	intpr __P((int, u_long));

void	unixpr __P((u_long));

void	esis_stats __P((u_long, char *));
void	clnp_stats __P((u_long, char *));
void	cltp_stats __P((u_long, char *));
void	iso_protopr __P((u_long, char *));
void	iso_protopr1 __P((u_long, int));
void	tp_protopr __P((u_long, char *));
void	tp_inproto __P((u_long));
void	tp_stats __P((caddr_t, caddr_t));

void	mroutepr __P((u_long, u_long, u_long));
void	mrt_stats __P((u_long, u_long));
