/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)startup.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Routing Table Management Daemon
 */
#include "defs.h"
#include <sys/ioctl.h>
#include <net/if.h>
#include <nlist.h>
#include <syslog.h>

struct	interface *ifnet;
int	lookforinterfaces = 1;
int	performnlist = 1;
int	externalinterfaces = 0;		/* # of remote and local interfaces */

/*
 * Find the network interfaces which have configured themselves.
 * If the interface is present but not yet up (for example an
 * ARPANET IMP), set the lookforinterfaces flag so we'll
 * come back later and look again.
 */
ifinit()
{
	struct interface ifs, *ifp;
	int s, n;
	char buf[BUFSIZ];
        struct ifconf ifc;
        struct ifreq ifreq, *ifr;
        struct sockaddr_in *sin;
	u_long i;

	if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		syslog(LOG_ERR, "socket: %m");
		exit(1);
	}
        ifc.ifc_len = sizeof (buf);
        ifc.ifc_buf = buf;
        if (ioctl(s, SIOCGIFCONF, (char *)&ifc) < 0) {
                syslog(LOG_ERR, "ioctl (get interface configuration)");
		close(s);
                return (0);
        }
        ifr = ifc.ifc_req;
	lookforinterfaces = 0;
        for (n = ifc.ifc_len / sizeof (struct ifreq); n > 0; n--, ifr++) {
		bzero((char *)&ifs, sizeof(ifs));
		ifs.int_addr = ifr->ifr_addr;
		ifreq = *ifr;
                if (ioctl(s, SIOCGIFFLAGS, (char *)&ifreq) < 0) {
                        syslog(LOG_ERR, "ioctl (get interface flags)");
                        continue;
                }
		ifs.int_flags = ifreq.ifr_flags | IFF_INTERFACE;
		if ((ifs.int_flags & IFF_UP) == 0 ||
		    ifr->ifr_addr.sa_family == AF_UNSPEC) {
			lookforinterfaces = 1;
			continue;
		}
		/* already known to us? */
		if (if_ifwithaddr(&ifs.int_addr))
			continue;
		/* argh, this'll have to change sometime */
		if (ifs.int_addr.sa_family != AF_INET)
			continue;
                if (ifs.int_flags & IFF_POINTOPOINT) {
                        if (ioctl(s, SIOCGIFDSTADDR, (char *)&ifreq) < 0) {
                                syslog(LOG_ERR, "ioctl (get dstaddr)");
                                continue;
			}
			ifs.int_dstaddr = ifreq.ifr_dstaddr;
		}
                if (ifs.int_flags & IFF_BROADCAST) {
                        if (ioctl(s, SIOCGIFBRDADDR, (char *)&ifreq) < 0) {
                                syslog(LOG_ERR, "ioctl (get broadaddr)");
                                continue;
                        }
			ifs.int_broadaddr = ifreq.ifr_broadaddr;
		}
		if (ioctl(s, SIOCGIFNETMASK, (char *)&ifreq) < 0) {
			syslog(LOG_ERR, "ioctl (get netmask)");
			continue;
		}
		sin = (struct sockaddr_in *)&ifreq.ifr_addr;
		ifs.int_subnetmask = ntohl(sin->sin_addr.s_addr);
		sin = (struct sockaddr_in *)&ifs.int_addr;
		i = ntohl(sin->sin_addr.s_addr);
		if (IN_CLASSA(i))
			ifs.int_netmask = IN_CLASSA_NET;
		else if (IN_CLASSB(i))
			ifs.int_netmask = IN_CLASSB_NET;
		else
			ifs.int_netmask = IN_CLASSC_NET;
		ifs.int_net = i & ifs.int_netmask;
		ifs.int_subnet = i & ifs.int_subnetmask;
		/* no one cares about software loopback interfaces */
		if (ifs.int_net == LOOPBACKNET)
			continue;
		ifp = (struct interface *)malloc(sizeof (struct interface));
		if (ifp == 0) {
			printf("routed: out of memory\n");
			break;
		}
		*ifp = ifs;
		/*
		 * Count the # of directly connected networks
		 * and point to point links which aren't looped
		 * back to ourself.  This is used below to
		 * decide if we should be a routing ``supplier''.
		 */
		if ((ifs.int_flags & IFF_POINTOPOINT) == 0 ||
		    if_ifwithaddr(&ifs.int_dstaddr) == 0)
			externalinterfaces++;
		ifp->int_name = malloc(strlen(ifr->ifr_name) + 1);
		if (ifp->int_name == 0) {
			fprintf(stderr, "routed: ifinit: out of memory\n");
			goto bad;		/* ??? */
		}
		strcpy(ifp->int_name, ifr->ifr_name);
		ifp->int_metric = 0;
		ifp->int_next = ifnet;
		ifnet = ifp;
		traceinit(ifp);
		addrouteforif(ifp);
	}
	if (externalinterfaces > 1 && supplier < 0)
		supplier = 1;
	close(s);
	return;
bad:
	sleep(60);
	close(kmem), close(s);
	execv("/etc/routed", argv0);
	_exit(0177);
}

addrouteforif(ifp)
	struct interface *ifp;
{
	struct sockaddr_in net;
	struct sockaddr *dst;
	int state, metric;
	struct rt_entry *rt;

	if (ifp->int_flags & IFF_POINTOPOINT)
		dst = &ifp->int_dstaddr;
	else {
		bzero((char *)&net, sizeof (net));
		net.sin_family = AF_INET;
		net.sin_addr = inet_makeaddr(ifp->int_subnet, INADDR_ANY);
		dst = (struct sockaddr *)&net;
	}
	rt = rtfind(dst);
	if (rt && (rt->rt_state & RTS_INTERFACE))
		return;
	if (rt)
		rtdelete(rt);
	if (ifp->int_transitions++ > 0)
		syslog(LOG_ERR, "re-installing interface %s", ifp->int_name);
	rtadd(dst, &ifp->int_addr, ifp->int_metric,
		ifp->int_flags & (IFF_INTERFACE|IFF_PASSIVE|IFF_REMOTE));
}

/*
 * As a concession to the ARPANET we read a list of gateways
 * from /etc/gateways and add them to our tables.  This file
 * exists at each ARPANET gateway and indicates a set of ``remote''
 * gateways (i.e. a gateway which we can't immediately determine
 * if it's present or not as we can do for those directly connected
 * at the hardware level).  If a gateway is marked ``passive''
 * in the file, then we assume it doesn't have a routing process
 * of our design and simply assume it's always present.  Those
 * not marked passive are treated as if they were directly
 * connected -- they're added into the interface list so we'll
 * send them routing updates.
 */
gwkludge()
{
	struct sockaddr_in dst, gate;
	FILE *fp;
	char *type, *dname, *gname, *qual, buf[BUFSIZ];
	struct interface *ifp;
	int metric;
	struct rt_entry route;

	fp = fopen("/etc/gateways", "r");
	if (fp == NULL)
		return;
	qual = buf;
	dname = buf + 64;
	gname = buf + ((BUFSIZ - 64) / 3);
	type = buf + (((BUFSIZ - 64) * 2) / 3);
	bzero((char *)&dst, sizeof (dst));
	bzero((char *)&gate, sizeof (gate));
	bzero((char *)&route, sizeof(route));
	/* format: {net | host} XX gateway XX metric DD [passive]\n */
#define	readentry(fp) \
	fscanf((fp), "%s %s gateway %s metric %d %s\n", \
		type, dname, gname, &metric, qual)
	for (;;) {
		if (readentry(fp) == EOF)
			break;
		if (!getnetorhostname(type, dname, &dst))
			continue;
		if (!gethostnameornumber(gname, &gate))
			continue;
		if (strcmp(qual, "passive") == 0) {
			/*
			 * Passive entries aren't placed in our tables,
			 * only the kernel's, so we don't copy all of the
			 * external routing information within a net.
			 * Internal machines should use the default
			 * route to a suitable gateway (like us).
			 */
			route.rt_dst = *(struct sockaddr *) &dst;
			route.rt_router = *(struct sockaddr *) &gate;
			route.rt_flags = RTF_UP;
			if (strcmp(type, "host") == 0)
				route.rt_flags |= RTF_HOST;
			if (metric)
				route.rt_flags |= RTF_GATEWAY;
			(void) ioctl(s, SIOCADDRT, (char *)&route.rt_rt);
			continue;
		}
		/* assume no duplicate entries */
		externalinterfaces++;
		ifp = (struct interface *)malloc(sizeof (*ifp));
		bzero((char *)ifp, sizeof (*ifp));
		ifp->int_flags = IFF_REMOTE;
		/* can't identify broadcast capability */
		ifp->int_net = inet_netof(dst.sin_addr);
		if (strcmp(type, "host") == 0) {
			ifp->int_flags |= IFF_POINTOPOINT;
			ifp->int_dstaddr = *((struct sockaddr *)&dst);
		}
		ifp->int_addr = *((struct sockaddr *)&gate);
		ifp->int_metric = metric;
		ifp->int_next = ifnet;
		ifnet = ifp;
		addrouteforif(ifp);
	}
	fclose(fp);
}

getnetorhostname(type, name, sin)
	char *type, *name;
	struct sockaddr_in *sin;
{

	if (strcmp(type, "net") == 0) {
		struct netent *np = getnetbyname(name);
		int n;

		if (np == 0)
			n = inet_network(name);
		else {
			if (np->n_addrtype != AF_INET)
				return (0);
			n = np->n_net;
			/*
			 * getnetbyname returns right-adjusted value.
			 */
			if (n < 128)
				n <<= IN_CLASSA_NSHIFT;
			else if (n < 65536)
				n <<= IN_CLASSB_NSHIFT;
			else
				n <<= IN_CLASSC_NSHIFT;
		}
		sin->sin_family = AF_INET;
		sin->sin_addr = inet_makeaddr(n, INADDR_ANY);
		return (1);
	}
	if (strcmp(type, "host") == 0) {
		struct hostent *hp = gethostbyname(name);

		if (hp == 0)
			sin->sin_addr.s_addr = inet_addr(name);
		else {
			if (hp->h_addrtype != AF_INET)
				return (0);
			bcopy(hp->h_addr, &sin->sin_addr, hp->h_length);
		}
		sin->sin_family = AF_INET;
		return (1);
	}
	return (0);
}

gethostnameornumber(name, sin)
	char *name;
	struct sockaddr_in *sin;
{
	struct hostent *hp;

	hp = gethostbyname(name);
	if (hp) {
		bcopy(hp->h_addr, &sin->sin_addr, hp->h_length);
		sin->sin_family = hp->h_addrtype;
		return (1);
	}
	sin->sin_addr.s_addr = inet_addr(name);
	sin->sin_family = AF_INET;
	return (sin->sin_addr.s_addr != -1);
}
