#ifndef lint
static char rcsid[] = "$Header$";
#endif

/*
 * Routing Table Management Daemon
 */
#include "defs.h"
#include <sys/ioctl.h>
#include <net/if.h>
#include <nlist.h>

struct	interface *ifnet;
int	kmem = -1;
int	lookforinterfaces = 1;
int	performnlist = 1;
int	externalinterfaces = 0;		/* # of remote and local interfaces */
int	gateway = 0;		/* 1 if we are a gateway to parts beyond */
char ether_broadcast_addr[6] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff};

struct nlist nl[] = {
#define	N_IFNET		0
	{ "_ifnet" },
	{ "" },
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
	int j,iffound;
	struct sockaddr ifaddr, ifdstaddr;
	struct sockaddr_xn *sxn;

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
		/* Only interested in af XNS */
		iffound = 0;
		for( j = 0; j < NIFADDR; j++) {
		    if (ifs.if_addr[j].sa_family != AF_XNS)
		    	continue;
		    /* already known to us? */
		    if (if_ifwithaddr(&ifs.if_addr[j]))
			continue; 
		    ifaddr = ifs.if_addr[j];
		    ifdstaddr = ifs.if_dstaddr[j];
		    iffound++;
		}
		if (!iffound) continue;
		
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
		    if_ifwithaddr(ifdstaddr) == 0)
			externalinterfaces++;
		if ((ifs.if_flags & IFF_LOCAL) == 0 && gateway == 0) {
			/*
			 * If we have an interface to a non-local network,
			 * we are a candidate for use as a gateway.
			 */
			gateway = 1;
		}
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
		ifp->int_flags = (ifs.if_flags & 0x1ff) | IFF_INTERFACE;
		ifp->int_addr = ifaddr;
		/* this works because broadaddr overlaps dstaddr */
		ifp->int_broadaddr = ifdstaddr;
		ifp->int_metric = 0;
		ifp->int_next = ifnet;
		/* 
		 * Now arrange for a socket to listen on.
		 * This nonsense is necessary because of the stupidity
		 * of the raw socket code.
		 */
		/* listen on our own address... */
		sxn = (struct sockaddr_xn *) &ifp->int_addr;
		sxn->sxn_addr.xn_socket = htons(IDPPORT_RIF);
		ifp->int_ripsock[0] = getsocket(AF_XNS, SOCK_RAW, IDPPROTO_RIF, sxn);
		/* AND on our broadcast address if we have one */
		if (ifp->int_flags & IFF_BROADCAST) {
		    sxn = (struct sockaddr_xn *)  &ifp->int_broadaddr ;
		    sxn->sxn_addr.xn_socket = htons(IDPPORT_RIF);
		    ifp->int_ripsock[1] = getsocket(AF_XNS, SOCK_RAW, IDPPROTO_RIF, sxn);
		}
		ifnet = ifp;
		traceinit(ifp);
		addrouteforif(ifp);
	}
	if (externalinterfaces > 1 && supplier < 0)
		supplier = 1;
	return;
bad:
	sleep(60);
	for(j = 3; j < 32; j++) close(j);
	execv("/etc/XNSrouted", argv0);
	_exit(0177);
}

addrouteforif(ifp)
	struct interface *ifp;
{
	struct sockaddr_xn net;
	struct sockaddr *dst;
	int state, metric;
	struct rt_entry *rt;

	if (ifp->int_flags & IFF_POINTOPOINT)
		dst = &ifp->int_dstaddr;
	else {
		bzero(&net, sizeof (net));
		net.sxn_family = AF_XNS;
		xnnet(net.sxn_addr.xn_net) = xnnet(((struct sockaddr_xn *)(&ifp->int_addr))->sxn_addr.xn_net);
		dst = (struct sockaddr *)&net;
	}
	rt = rtlookup(dst);
	rtadd(dst, &ifp->int_addr, ifp->int_metric,
		ifp->int_flags & (IFF_INTERFACE|IFF_PASSIVE|IFF_REMOTE));
	if (rt)
		rtdelete(rt);
}


