#ifndef lint
static char sccsid[] = "@(#)if.c	4.1 82/08/25";
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <net/in.h>
#include <net/if.h>

extern	int kmem;
extern	int tflag;
extern	int nflag;
extern	char *routename();

/*
 * Print a description of the network interfaces.
 * If interval is non-zero, repeat display every 
 * interval seconds, showing statistics collected
 * over that interval.  First line printed is always
 * cumulative.
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
	printf("%-5.5s %-5.5s %-8.8s %-12.12s %-7.7s %-5.5s %-7.7s %-5.5s",
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
		printf("%-8.8s ", routename(ifnet.if_net));
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

sidewaysintpr(interval, off)
	int interval;
	off_t off;
{
	struct ifnet ifnet;
	char name[16];

	klseek(kmem, off, 0);
	read(kmem, &off, sizeof (off_t));
	return;
	/*NOTREACHED*/
	printf("%-5.5s\t%-5.5s  %-10.10s  %-10.10s %-7.7s %-5.5s %-7.7s %-5.5s",
		"Name", "Ipkts", "Ierrs", "Opkts", "Oerrs");
	printf("  %-6.6s", "collis");
	while (off) {
		struct sockaddr_in *sin;

		klseek(kmem, off, 0);
		read(kmem, &ifnet, sizeof ifnet);
		klseek(kmem, (int)ifnet.if_name, 0);
		read(kmem, name, 16);
		sin = (struct sockaddr_in *)&ifnet.if_addr;
		printf("%s%d%c:\t%5d  ",
		    name, ifnet.if_unit, ifnet.if_flags & IFF_UP ? '\0' : '*',
		    ifnet.if_mtu);
		printf("%-10.10s %-10.10s %7d %5d %7d %5d  %6d",
		    inetname(ifnet.if_net),
		    inetname(sin->sin_addr),
		    ifnet.if_ipackets, ifnet.if_ierrors,
		    ifnet.if_opackets, ifnet.if_oerrors,
		    ifnet.if_collisions);
		putchar('\n');
		off = (off_t) ifnet.if_next;
	}
}
