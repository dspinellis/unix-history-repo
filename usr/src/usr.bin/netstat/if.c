#ifndef lint
static char sccsid[] = "@(#)if.c	4.3 82/10/07";
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <net/in.h>
#include <net/if.h>
#include <stdio.h>

extern	int kmem;
extern	int tflag;
extern	int nflag;
extern	char *routename();

/*
 * Print a description of the network interfaces.
 */
intpr(interval, ifnetaddr)
	int interval;
	off_t ifnetaddr;
{
	struct ifnet ifnet;
	char name[16];

	if (ifnetaddr == 0) {
		printf("ifnet: symbol not defined\n");
		return;
	}
	if (interval) {
		sidewaysintpr(interval, ifnetaddr);
		return;
	}
	klseek(kmem, ifnetaddr, 0);
	read(kmem, &ifnetaddr, sizeof ifnetaddr);
	printf("%-5.5s %-5.5s %-10.10s  %-12.12s %-7.7s %-5.5s %-7.7s %-5.5s",
		"Name", "Mtu", "Network", "Address", "Ipkts", "Ierrs",
		"Opkts", "Oerrs");
	printf(" %-6.6s", "Collis");
	if (tflag)
		printf(" %-6.6s", "Timer");
	putchar('\n');
	while (ifnetaddr) {
		struct sockaddr_in *sin;
		register char *cp;
		char *index();
		struct in_addr in, inet_makeaddr();

		klseek(kmem, ifnetaddr, 0);
		read(kmem, &ifnet, sizeof ifnet);
		klseek(kmem, (int)ifnet.if_name, 0);
		read(kmem, name, 16);
		name[15] = '\0';
		cp = index(name, '\0');
		*cp++ = ifnet.if_unit + '0';
		if ((ifnet.if_flags&IFF_UP) == 0)
			*cp++ = '*';
		*cp = '\0';
		printf("%-5.5s %-5d ", name, ifnet.if_mtu);
		sin = (struct sockaddr_in *)&ifnet.if_addr;
		in = inet_makeaddr(ifnet.if_net, INADDR_ANY);
		printf("%-10.10s  ", routename(in));
		printf("%-12.12s %-7d %-5d %-7d %-5d %-6d",
		    routename(sin->sin_addr),
		    ifnet.if_ipackets, ifnet.if_ierrors,
		    ifnet.if_opackets, ifnet.if_oerrors,
		    ifnet.if_collisions);
		if (tflag)
			printf(" %-6d", ifnet.if_timer);
		putchar('\n');
		ifnetaddr = (off_t) ifnet.if_next;
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
} iftot[MAXIF];

/*
 * Print a running summary of interface statistics.
 * Repeat display every interval seconds, showing
 * statistics collected over that interval.  First
 * line printed at top of screen is always cumulative.
 */
sidewaysintpr(interval, off)
	int interval;
	off_t off;
{
	struct ifnet ifnet;
	off_t firstifnet;
	extern char _sobuf[];
	register struct iftot *ip, *total;
	register int line;
	struct iftot *lastif, *sum, *interesting;
	int maxtraffic;

	setbuf(stdout, _sobuf);
	klseek(kmem, off, 0);
	read(kmem, &firstifnet, sizeof (off_t));
	lastif = iftot;
	sum = iftot + MAXIF - 1;
	total = sum - 1;
	for (off = firstifnet, ip = iftot; off;) {
		char *cp;

		klseek(kmem, off, 0);
		read(kmem, &ifnet, sizeof ifnet);
		klseek(kmem, (int)ifnet.if_name, 0);
		ip->ift_name[0] = '(';
		read(kmem, ip->ift_name + 1, 15);
		ip->ift_name[15] = '\0';
		cp = index(ip->ift_name, '\0');
		sprintf(cp, "%d)", ifnet.if_unit);
		ip++;
		if (ip >= iftot + MAXIF - 2)
			break;
		off = (off_t) ifnet.if_next;
	}
	lastif = ip;
	interesting = iftot;
banner:
	printf("    input   %-6.6s    output       ", interesting->ift_name);
	if (lastif - iftot > 0)
		printf("    input   (Total)    output       ");
	for (ip = iftot; ip < iftot + MAXIF; ip++) {
		ip->ift_ip = 0;
		ip->ift_ie = 0;
		ip->ift_op = 0;
		ip->ift_oe = 0;
		ip->ift_co = 0;
	}
	putchar('\n');
	printf("%-7.7s %-5.5s %-7.7s %-5.5s %-5.5s ",
		"packets", "errs", "packets", "errs", "colls");
	if (lastif - iftot > 0)
		printf("%-7.7s %-5.5s %-7.7s %-5.5s %-5.5s ",
			"packets", "errs", "packets", "errs", "colls");
	putchar('\n');
	fflush(stdout);
	line = 0;
loop:
	sum->ift_ip = 0;
	sum->ift_ie = 0;
	sum->ift_op = 0;
	sum->ift_oe = 0;
	sum->ift_co = 0;
	for (off = firstifnet, ip = iftot; off && ip < lastif; ip++) {
		klseek(kmem, off, 0);
		read(kmem, &ifnet, sizeof ifnet);
		if (ip == interesting)
			printf("%-7d %-5d %-7d %-5d %-5d ",
				ifnet.if_ipackets - ip->ift_ip,
				ifnet.if_ierrors - ip->ift_ie,
				ifnet.if_opackets - ip->ift_op,
				ifnet.if_oerrors - ip->ift_oe,
				ifnet.if_collisions - ip->ift_co);
		ip->ift_ip = ifnet.if_ipackets;
		ip->ift_ie = ifnet.if_ierrors;
		ip->ift_op = ifnet.if_opackets;
		ip->ift_oe = ifnet.if_oerrors;
		ip->ift_co = ifnet.if_collisions;
		sum->ift_ip += ip->ift_ip;
		sum->ift_ie += ip->ift_ie;
		sum->ift_op += ip->ift_op;
		sum->ift_oe += ip->ift_oe;
		sum->ift_co += ip->ift_co;
		off = (off_t) ifnet.if_next;
	}
	if (lastif - iftot > 0)
		printf("%-7d %-5d %-7d %-5d %-5d\n",
			sum->ift_ip - total->ift_ip,
			sum->ift_ie - total->ift_ie,
			sum->ift_op - total->ift_op,
			sum->ift_oe - total->ift_oe,
			sum->ift_co - total->ift_co);
	*total = *sum;
	fflush(stdout);
	line++;
	if (interval)
		sleep(interval);
	if (line == 21)
		goto banner;
	goto loop;
	/*NOTREACHED*/
}
