/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)if.c	5.8 (Berkeley) 6/29/88";
#endif /* not lint */

#include <sys/types.h>
#include <sys/socket.h>

#include <net/if.h>
#include <netinet/in.h>
#include <netinet/in_var.h>
#include <netns/ns.h>

#include <stdio.h>
#include <signal.h>

#define	YES	1
#define	NO	0

extern	int kmem;
extern	int tflag;
extern	int dflag;
extern	int nflag;
extern	char *interface;
extern	int unit;
extern	char *routename(), *netname(), *ns_phost();

/*
 * Print a description of the network interfaces.
 */
intpr(interval, ifnetaddr)
	int interval;
	off_t ifnetaddr;
{
	struct ifnet ifnet;
	union {
		struct ifaddr ifa;
		struct in_ifaddr in;
	} ifaddr;
	off_t ifaddraddr;
	char name[16];

	if (ifnetaddr == 0) {
		printf("ifnet: symbol not defined\n");
		return;
	}
	if (interval) {
		sidewaysintpr((unsigned)interval, ifnetaddr);
		return;
	}
	klseek(kmem, ifnetaddr, 0);
	read(kmem, (char *)&ifnetaddr, sizeof ifnetaddr);
	printf("%-5.5s %-5.5s %-11.11s %-15.15s %8.8s %5.5s %8.8s %5.5s",
		"Name", "Mtu", "Network", "Address", "Ipkts", "Ierrs",
		"Opkts", "Oerrs");
	printf(" %5s", "Coll");
	if (tflag)
		printf(" %s", "Time");
	if (dflag)
		printf(" %s", "Drop");
	putchar('\n');
	ifaddraddr = 0;
	while (ifnetaddr || ifaddraddr) {
		struct sockaddr_in *sin;
		register char *cp;
		int n;
		char *index();
		struct in_addr inet_makeaddr();

		if (ifaddraddr == 0) {
			klseek(kmem, ifnetaddr, 0);
			read(kmem, (char *)&ifnet, sizeof ifnet);
			klseek(kmem, (off_t)ifnet.if_name, 0);
			read(kmem, name, 16);
			name[15] = '\0';
			ifnetaddr = (off_t) ifnet.if_next;
			if (interface != 0 &&
			    (strcmp(name, interface) != 0 || unit != ifnet.if_unit))
				continue;
			cp = index(name, '\0');
			*cp++ = ifnet.if_unit + '0';
			if ((ifnet.if_flags&IFF_UP) == 0)
				*cp++ = '*';
			*cp = '\0';
			ifaddraddr = (off_t)ifnet.if_addrlist;
		}
		printf("%-5.5s %-5d ", name, ifnet.if_mtu);
		if (ifaddraddr == 0) {
			printf("%-11.11s ", "none");
			printf("%-15.15s ", "none");
		} else {
			klseek(kmem, ifaddraddr, 0);
			read(kmem, (char *)&ifaddr, sizeof ifaddr);
			ifaddraddr = (off_t)ifaddr.ifa.ifa_next;
			switch (ifaddr.ifa.ifa_addr.sa_family) {
			case AF_UNSPEC:
				printf("%-11.11s ", "none");
				printf("%-15.15s ", "none");
				break;
			case AF_INET:
				sin = (struct sockaddr_in *)&ifaddr.in.ia_addr;
#ifdef notdef
				/* can't use inet_makeaddr because kernel
				 * keeps nets unshifted.
				 */
				in = inet_makeaddr(ifaddr.in.ia_subnet,
					INADDR_ANY);
				printf("%-11.11s ", netname(in));
#else
				printf("%-11.11s ",
					netname(htonl(ifaddr.in.ia_subnet),
						ifaddr.in.ia_subnetmask));
#endif
				printf("%-15.15s ", routename(sin->sin_addr));
				break;
			case AF_NS:
				{
				struct sockaddr_ns *sns =
				(struct sockaddr_ns *)&ifaddr.in.ia_addr;
				u_long net;
				char netnum[8];
				char *ns_phost();

				*(union ns_net *) &net = sns->sns_addr.x_net;
				sprintf(netnum, "%lxH", ntohl(net));
				upHex(netnum);
				printf("ns:%-8s ", netnum);
				printf("%-15s ", ns_phost(sns));
				}
				break;
			default:
				printf("af%2d: ", ifaddr.ifa.ifa_addr.sa_family);
				for (cp = (char *)&ifaddr.ifa.ifa_addr +
				    sizeof(struct sockaddr) - 1;
				    cp >= ifaddr.ifa.ifa_addr.sa_data; --cp)
					if (*cp != 0)
						break;
				n = cp - (char *)ifaddr.ifa.ifa_addr.sa_data + 1;
				cp = (char *)ifaddr.ifa.ifa_addr.sa_data;
				if (n <= 7)
					while (--n)
						printf("%02d.", *cp++ & 0xff);
				else
					while (--n)
						printf("%02d", *cp++ & 0xff);
				printf("%02d ", *cp & 0xff);
				break;
			}
		}
		printf("%8d %5d %8d %5d %5d",
		    ifnet.if_ipackets, ifnet.if_ierrors,
		    ifnet.if_opackets, ifnet.if_oerrors,
		    ifnet.if_collisions);
		if (tflag)
			printf(" %3d", ifnet.if_timer);
		if (dflag)
			printf(" %3d", ifnet.if_snd.ifq_drops);
		putchar('\n');
	}
}

#define	MAXIF	10
struct	iftot {
	char	ift_name[16];		/* interface name */
	int	ift_ip;			/* input packets */
	int	ift_ie;			/* input errors */
	int	ift_op;			/* output packets */
	int	ift_oe;			/* output errors */
	int	ift_co;			/* collisions */
	int	ift_dr;			/* drops */
} iftot[MAXIF];

u_char	signalled;			/* set if alarm goes off "early" */

/*
 * Print a running summary of interface statistics.
 * Repeat display every interval seconds, showing statistics
 * collected over that interval.  Assumes that interval is non-zero.
 * First line printed at top of screen is always cumulative.
 */
sidewaysintpr(interval, off)
	unsigned interval;
	off_t off;
{
	struct ifnet ifnet;
	off_t firstifnet;
	register struct iftot *ip, *total;
	register int line;
	struct iftot *lastif, *sum, *interesting;
	int oldmask;
	int catchalarm();

	klseek(kmem, off, 0);
	read(kmem, (char *)&firstifnet, sizeof (off_t));
	lastif = iftot;
	sum = iftot + MAXIF - 1;
	total = sum - 1;
	interesting = iftot;
	for (off = firstifnet, ip = iftot; off;) {
		char *cp;

		klseek(kmem, off, 0);
		read(kmem, (char *)&ifnet, sizeof ifnet);
		klseek(kmem, (off_t)ifnet.if_name, 0);
		ip->ift_name[0] = '(';
		read(kmem, ip->ift_name + 1, 15);
		if (interface && strcmp(ip->ift_name + 1, interface) == 0 &&
		    unit == ifnet.if_unit)
			interesting = ip;
		ip->ift_name[15] = '\0';
		cp = index(ip->ift_name, '\0');
		sprintf(cp, "%d)", ifnet.if_unit);
		ip++;
		if (ip >= iftot + MAXIF - 2)
			break;
		off = (off_t) ifnet.if_next;
	}
	lastif = ip;

	(void)signal(SIGALRM, catchalarm);
	signalled = NO;
	(void)alarm(interval);
banner:
	printf("   input    %-6.6s    output       ", interesting->ift_name);
	if (lastif - iftot > 0) {
		if (dflag)
			printf("      ");
		printf("     input   (Total)    output");
	}
	for (ip = iftot; ip < iftot + MAXIF; ip++) {
		ip->ift_ip = 0;
		ip->ift_ie = 0;
		ip->ift_op = 0;
		ip->ift_oe = 0;
		ip->ift_co = 0;
		ip->ift_dr = 0;
	}
	putchar('\n');
	printf("%8.8s %5.5s %8.8s %5.5s %5.5s ",
		"packets", "errs", "packets", "errs", "colls");
	if (dflag)
		printf("%5.5s ", "drops");
	if (lastif - iftot > 0)
		printf(" %8.8s %5.5s %8.8s %5.5s %5.5s",
			"packets", "errs", "packets", "errs", "colls");
	if (dflag)
		printf(" %5.5s", "drops");
	putchar('\n');
	fflush(stdout);
	line = 0;
loop:
	sum->ift_ip = 0;
	sum->ift_ie = 0;
	sum->ift_op = 0;
	sum->ift_oe = 0;
	sum->ift_co = 0;
	sum->ift_dr = 0;
	for (off = firstifnet, ip = iftot; off && ip < lastif; ip++) {
		klseek(kmem, off, 0);
		read(kmem, (char *)&ifnet, sizeof ifnet);
		if (ip == interesting) {
			printf("%8d %5d %8d %5d %5d",
				ifnet.if_ipackets - ip->ift_ip,
				ifnet.if_ierrors - ip->ift_ie,
				ifnet.if_opackets - ip->ift_op,
				ifnet.if_oerrors - ip->ift_oe,
				ifnet.if_collisions - ip->ift_co);
			if (dflag)
				printf(" %5d",
				    ifnet.if_snd.ifq_drops - ip->ift_dr);
		}
		ip->ift_ip = ifnet.if_ipackets;
		ip->ift_ie = ifnet.if_ierrors;
		ip->ift_op = ifnet.if_opackets;
		ip->ift_oe = ifnet.if_oerrors;
		ip->ift_co = ifnet.if_collisions;
		ip->ift_dr = ifnet.if_snd.ifq_drops;
		sum->ift_ip += ip->ift_ip;
		sum->ift_ie += ip->ift_ie;
		sum->ift_op += ip->ift_op;
		sum->ift_oe += ip->ift_oe;
		sum->ift_co += ip->ift_co;
		sum->ift_dr += ip->ift_dr;
		off = (off_t) ifnet.if_next;
	}
	if (lastif - iftot > 0) {
		printf("  %8d %5d %8d %5d %5d",
			sum->ift_ip - total->ift_ip,
			sum->ift_ie - total->ift_ie,
			sum->ift_op - total->ift_op,
			sum->ift_oe - total->ift_oe,
			sum->ift_co - total->ift_co);
		if (dflag)
			printf(" %5d", sum->ift_dr - total->ift_dr);
	}
	*total = *sum;
	putchar('\n');
	fflush(stdout);
	line++;
	oldmask = sigblock(sigmask(SIGALRM));
	if (! signalled) {
		sigpause(0);
	}
	sigsetmask(oldmask);
	signalled = NO;
	(void)alarm(interval);
	if (line == 21)
		goto banner;
	goto loop;
	/*NOTREACHED*/
}

/*
 * Called if an interval expires before sidewaysintpr has completed a loop.
 * Sets a flag to not wait for the alarm.
 */
catchalarm()
{
	signalled = YES;
}
