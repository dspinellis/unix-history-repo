/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)netstat.h	5.1 (Berkeley) %G%
 */

#include <sys/cdefs.h>

int	Aflag;		/* show addresses of protocol control block */
int	aflag;		/* show all sockets (including servers) */
int	dflag;		/* show i/f dropped packets */
int	hflag;		/* show IMP host table */
int	iflag;		/* show interfaces */
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


int	kread __P((off_t addr, char *buf, int size));
char	*plural __P((int));

void	protopr __P((off_t, char *));
void	tcp_stats __P((off_t, char *));
void	udp_stats __P((off_t, char *));
void	ip_stats __P((off_t, char *));
void	icmp_stats __P((off_t, char *));
void	protopr __P((off_t, char *));

void	mbpr(off_t);

void	hostpr __P((off_t, off_t));
void	impstats __P((off_t, off_t));

void	pr_rthdr __P(());
void	pr_family __P((int));
void	rt_stats __P((off_t));
char	*ns_phost __P((struct sockaddr *));
void	upHex __P((char *));

char	*routename __P((u_long));
char	*netname __P((u_long, u_long));
char	*ns_print __P((struct sockaddr *));
void	routepr __P((off_t));

void	nsprotopr __P((off_t, char *));
void	spp_stats __P((off_t, char *));
void	idp_stats __P((off_t, char *));
void	nserr_stats __P((off_t, char *));
void	ns_erputil __P((int, int));

void	intpr __P((int, off_t));

void	unixpr __P((off_t, off_t, struct protosw *));

void	esis_stats __P((off_t, char *));
void	clnp_stats __P((off_t, char *));
void	cltp_stats __P((off_t, char *));
void	iso_protopr __P((off_t, char *));
void	tp_stats __P((caddr_t, caddr_t));
