#ifndef lint
static char sccsid[] = "@(#)route.c	4.3 82/10/06";
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/mbuf.h>
#include <net/if.h>
#include <net/in.h>
#define	KERNEL		/* to get routehash and RTHASHSIZ */
#include <net/route.h>
#include <netdb.h>

extern	int kmem;
extern	int nflag;
extern	char *routename();

/*
 * Definitions for showing gateway flags.
 */
struct bits {
	short	b_mask;
	char	b_val;
} bits[] = {
	{ RTF_UP,	'U' },
	{ RTF_GATEWAY,	'G' },
	{ RTF_HOST,	'H' },
	{ 0 }
};

/*
 * Print routing tables.
 */
routepr(hostaddr, netaddr)
	off_t hostaddr, netaddr;
{
	struct mbuf mb;
	register struct rtentry *rt;
	register struct mbuf *m;
	register struct bits *p;
	struct netent *np;
	struct hostent *hp;
	char name[16], *flags;
	struct mbuf *routehash[RTHASHSIZ];
	struct ifnet ifnet;
	int first = 1, i, doinghost = 1;

	if (hostaddr == 0) {
		printf("rthost: symbol not in namelist\n");
		return;
	}
	if (netaddr == 0) {
		printf("rtnet: symbol not in namelist\n");
		return;
	}
	klseek(kmem, hostaddr, 0);
	read(kmem, routehash, sizeof (routehash));
	printf("Routing tables\n");
	printf("%-15.15s %-15.15s %-8.8s %-6.6s %-10.10s %s\n",
		"Destination", "Gateway",
		"Flags", "Refcnt", "Use", "Interface");
again:
	for (i = 0; i < RTHASHSIZ; i++) {
		if (routehash[i] == 0)
			continue;
		m = routehash[i];
		while (m) {
			struct sockaddr_in *sin;

			klseek(kmem, m, 0);
			read(kmem, &mb, sizeof (mb));
			rt = mtod(&mb, struct rtentry *);
			sin = (struct sockaddr_in *)&rt->rt_dst;
			printf("%-15.15s ", routename(sin->sin_addr));
			sin = (struct sockaddr_in *)&rt->rt_gateway;
			printf("%-15.15s ", routename(sin->sin_addr));
			for (flags = name, p = bits; p->b_mask; p++)
				if (p->b_mask & rt->rt_flags)
					*flags++ = p->b_val;
			*flags = '\0';
			printf("%-8.8s %-6d %-10d ", name,
				rt->rt_refcnt, rt->rt_use);
			if (rt->rt_ifp == 0) {
				putchar('\n');
				m = mb.m_next;
				continue;
			}
			klseek(kmem, rt->rt_ifp, 0);
			read(kmem, &ifnet, sizeof (ifnet));
			klseek(kmem, (int)ifnet.if_name, 0);
			read(kmem, name, 16);
			printf("%s%d\n", name, ifnet.if_unit);
			m = mb.m_next;
		}
	}
	if (doinghost) {
		klseek(kmem, netaddr, 0);
		read(kmem, routehash, sizeof (routehash));
		doinghost = 0;
		goto again;
	}
}

char *
routename(in)
	struct in_addr in;
{
	char *cp = 0;
	static char line[50];
	int lna, net;

	net = in_netof(in);
	lna = in_lnaof(in);
	if (!nflag) {
		if (lna == INADDR_ANY) {
			struct netent *np = getnetbyaddr(net, AF_INET);

			if (np)
				cp = np->n_name;
		} else {
			struct hostent *hp;

			hp = gethostbyaddr(&in, sizeof (struct in_addr),
				AF_INET);
			if (hp)
				cp = hp->h_name;
		}
	}
	if (cp)
		strcpy(line, cp);
	else {
		u_char *ucp = (u_char *)&in;
		if (lna == INADDR_ANY)
			sprintf(line, "%u.%u.%u", ucp[0], ucp[1], ucp[2]);
		else
			sprintf(line, "%u.%u.%u.%u", ucp[0], ucp[1],
				ucp[2], ucp[3]);
	}
	return (line);
}
