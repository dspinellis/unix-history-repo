#ifndef lint
static char sccsid[] = "@(#)startup.c	4.1 %G%";
#endif

/*
 * Routing Table Management Daemon
 */
#include "router.h"
#include <net/if.h>
#include <nlist.h>

struct	interface *ifnet;
int	kmem = -1;
int	lookforinterfaces = 1;
int	performnlist = 1;
int	externalinterfaces = 0;		/* # of remote and local interfaces */

struct nlist nl[] = {
#define	N_IFNET		0
	{ "_ifnet" },
	0,
};

/*
 * Probe the kernel through /dev/kmem to find the network
 * interfaces which have configured themselves.  If the
 * interface is present but not yet up (for example an
 * ARPANET IMP), set the lookforinterfaces flag so we'll
 * come back later and look again.
 */
ifinit()
{
	struct interface *ifp;
	struct ifnet ifs, *next;
	char name[32], *cp, *index();

	if (performnlist) {
		nlist("/vmunix", nl);
		if (nl[N_IFNET].n_value == 0) {
			printf("ifnet: not in namelist\n");
			goto bad;
		}
		performnlist = 0;
	}
	if (kmem < 0) {
		kmem = open("/dev/kmem", 0);
		if (kmem < 0) {
			perror("/dev/kmem");
			goto bad;
		}
	}
	if (lseek(kmem, (long)nl[N_IFNET].n_value, 0) == -1 ||
	    read(kmem, (char *)&next, sizeof (next)) != sizeof (next)) {
		printf("ifnet: error reading kmem\n");
		goto bad;
	}
	lookforinterfaces = 0;
	while (next) {
		if (lseek(kmem, (long)next, 0) == -1 ||
		    read(kmem, (char *)&ifs, sizeof (ifs)) != sizeof (ifs)) {
			perror("read");
			goto bad;
		}
		next = ifs.if_next;
		if ((ifs.if_flags & IFF_UP) == 0) {
			lookforinterfaces = 1;
			continue;
		}
		/* already known to us? */
		if (if_ifwithaddr(&ifs.if_addr))
			continue;
		/* argh, this'll have to change sometime */
		if (ifs.if_addr.sa_family != AF_INET)
			continue;
		/* no one cares about software loopback interfaces */
		if (ifs.if_net == LOOPBACKNET)
			continue;
		ifp = (struct interface *)malloc(sizeof (struct interface));
		if (ifp == 0) {
			printf("routed: out of memory\n");
			break;
		}
		/*
		 * Count the # of directly connected networks
		 * and point to point links which aren't looped
		 * back to ourself.  This is used below to
		 * decide if we should be a routing ``supplier''.
		 */
		if ((ifs.if_flags & IFF_POINTOPOINT) == 0 ||
		    if_ifwithaddr(&ifs.if_dstaddr) == 0)
			externalinterfaces++;
		lseek(kmem, ifs.if_name, 0);
		read(kmem, name, sizeof (name));
		name[sizeof (name) - 1] = '\0';
		cp = index(name, '\0');
		*cp++ = ifs.if_unit + '0';
		*cp = '\0';
		ifp->int_name = malloc(strlen(name) + 1);
		if (ifp->int_name == 0) {
			fprintf(stderr, "routed: ifinit: out of memory\n");
			goto bad;		/* ??? */
		}
		strcpy(ifp->int_name, name);
		ifp->int_addr = ifs.if_addr;
		ifp->int_flags = ifs.if_flags | IFF_INTERFACE;
		/* this works because broadaddr overlaps dstaddr */
		ifp->int_broadaddr = ifs.if_broadaddr;
		ifp->int_net = ifs.if_net;
		ifp->int_metric = 0;
		ifp->int_next = ifnet;
		ifnet = ifp;
		traceinit(ifp);
		addrouteforif(ifp);
	}
	if (externalinterfaces > 1 && supplier < 0)
		supplier = 1;
	return;
bad:
	sleep(60);
	close(kmem), close(s), close(snoroute);
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
		net.sin_addr = inet_makeaddr(ifp->int_net, INADDR_ANY);
		dst = (struct sockaddr *)&net;
	}
	rt = rtlookup(dst);
	rtadd(dst, &ifp->int_addr, ifp->int_metric,
		ifp->int_flags & (IFF_INTERFACE|IFF_PASSIVE|IFF_REMOTE));
	if (rt)
		rtdelete(rt);
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

	fp = fopen("/etc/gateways", "r");
	if (fp == NULL)
		return;
	qual = buf;
	dname = buf + 64;
	gname = buf + ((BUFSIZ - 64) / 3);
	type = buf + (((BUFSIZ - 64) * 2) / 3);
	bzero((char *)&dst, sizeof (dst));
	bzero((char *)&gate, sizeof (gate));
	dst.sin_family = gate.sin_family = AF_INET;
	/* format: {net | host} XX gateway XX metric DD [passive]\n */
#define	readentry(fp) \
	fscanf((fp), "%s %s gateway %s metric %d %s\n", \
		type, dname, gname, &metric, qual)
	for (;;) {
		struct hostent *host;
		struct netent *net;

		if (readentry(fp) == EOF)
			break;
		if (strcmp(type, "net") == 0) {
			net = getnetbyname(dname);
			if (net == 0 || net->n_addrtype != AF_INET)
				continue;
			dst.sin_addr = inet_makeaddr(net->n_net, INADDR_ANY);
		} else if (strcmp(type, "host") == 0) {
			host = gethostbyname(dname);
			if (host == 0)
				continue;
			bcopy(host->h_addr, &dst.sin_addr, host->h_length);
		} else
			continue;
		host = gethostbyname(gname);
		if (host == 0)
			continue;
		bcopy(host->h_addr, &gate.sin_addr, host->h_length);
		ifp = (struct interface *)malloc(sizeof (*ifp));
		bzero((char *)ifp, sizeof (*ifp));
		ifp->int_flags = IFF_REMOTE;
		/* can't identify broadcast capability */
		ifp->int_net = inet_netof(dst.sin_addr);
		if (strcmp(type, "host") == 0) {
			ifp->int_flags |= IFF_POINTOPOINT;
			ifp->int_dstaddr = *((struct sockaddr *)&dst);
		}
		if (strcmp(qual, "passive") == 0)
			ifp->int_flags |= IFF_PASSIVE;
		else
			/* assume no duplicate entries */
			externalinterfaces++;
		ifp->int_addr = *((struct sockaddr *)&gate);
		ifp->int_metric = metric;
		ifp->int_next = ifnet;
		ifnet = ifp;
		addrouteforif(ifp);
	}
	fclose(fp);
}
