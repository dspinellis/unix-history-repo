#ifndef lint
static char sccsid[] = "@(#)routed.c	4.2 %G%";
#endif

#include <sys/param.h>
#include <sys/protosw.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <net/in.h>
#define	KERNEL
#include <net/route.h>
#include <net/if.h>
#include <errno.h>
#include <stdio.h>
#include <nlist.h>
#include <signal.h>
#include "rip.h"
#include "router.h"

#define	LOOPBACKNET	0177
/* casts to keep lint happy */
#define	insque(q,p)	_insque((caddr_t)q,(caddr_t)p)
#define	remque(q)	_remque((caddr_t)q)
#define equal(a1, a2) \
	(bcmp((caddr_t)(a1), (caddr_t)(a2), sizeof (struct sockaddr)) == 0)

struct nlist nl[] = {
#define	N_IFNET		0
	{ "_ifnet" },
	0,
};

struct	sockaddr_in myaddr = { AF_INET, IPPORT_ROUTESERVER };

int	s;
int	kmem;
int	supplier;		/* process should supply updates */
int	initializing;		/* stem off broadcast() calls */
int	install = 0;		/* if 1 call kernel */
int	timeval;
int	timer();
int	cleanup();
int	trace = 0;

char	packet[MAXPACKETSIZE];

extern char *malloc();
extern int errno, exit();

main(argc, argv)
	int argc;
	char *argv[];
{
	int cc;
	struct sockaddr from;
	
	{   int t = open("/dev/tty", 2);
	    if (t >= 0) {
		ioctl(t, TIOCNOTTY, 0);
		close(t);
	    }
	}
	if (trace) {
		(void) fclose(stdout);
		(void) fclose(stderr);
		(void) fopen("trace", "a");
		(void) dup(fileno(stdout));
		setbuf(stdout, NULL);

	}
#ifdef vax
	myaddr.sin_port = htons(myaddr.sin_port);
#endif
again:
	s = socket(SOCK_DGRAM, 0, &myaddr, 0);
	if (s < 0) {
		perror("socket");
		sleep(30);
		goto again;
	}
	rtinit();
	getothers();
	getinterfaces();
	request();

	argv++, argc--;
	while (argc > 0) {
		if (strcmp(*argv, "-s") == 0)
			supplier++;
		else if (strcmp(*argv, "-q") == 0)
			supplier = 0;
		argv++, argc--;
	}
	sigset(SIGTERM, exit);
	sigset(SIGALRM, timer);
	alarm(TIMER_RATE);

	/*
	 * Listen for routing packets
	 */
	for (;;) {
		cc = receive(s, &from, packet, sizeof (packet));
		if (cc <= 0) {
			if (cc < 0 && errno != EINTR)
				perror("receive");
			continue;
		}
		sighold(SIGALRM);
		rip_input(&from, cc);
		sigrelse(SIGALRM);
	}
}

/*
 * Look in a file for any gateways we should configure
 * outside the directly connected ones.  This is a kludge,
 * but until we can find out about gateways on the "other side"
 * of the ARPANET using GGP, it's a must.
 *
 * We don't really know the distance to the gateway, so we
 * assume it's a neighbor.
 */
getothers()
{
	struct sockaddr_in dst, gate;
	FILE *fp = fopen("/etc/gateways", "r");
	struct rt_entry *rt;

	if (fp == NULL)
		return;
	bzero((char *)&dst, sizeof (dst));
	bzero((char *)&gate, sizeof (gate));
	dst.sin_family = AF_INET;
	gate.sin_family = AF_INET;
	while (fscanf(fp, "%x %x", &dst.sin_addr.s_addr, 
	   &gate.sin_addr.s_addr) != EOF) {
		rtadd((struct sockaddr *)&dst, (struct sockaddr *)&gate, 1);
		rt = rtlookup((struct sockaddr *)&dst);
		if (rt)
			rt->rt_flags |= RTF_SILENT;
	}
	fclose(fp);
}

struct ifnet *
if_ifwithaddr(addr)
	struct sockaddr *addr;
{
	register struct ifnet *ifp;

#define	same(a1, a2) \
	(bcmp((caddr_t)((a1)->sa_data), (caddr_t)((a2)->sa_data), 14) == 0)
	for (ifp = ifnet; ifp; ifp = ifp->if_next) {
		if (ifp->if_addr.sa_family != addr->sa_family)
			continue;
		if (same(&ifp->if_addr, addr))
			break;
		if ((ifp->if_flags & IFF_BROADCAST) &&
		    same(&ifp->if_broadaddr, addr))
			break;
	}
	return (ifp);
#undef same
}

struct ifnet *
if_ifwithnet(addr)
	register struct sockaddr *addr;
{
	register struct ifnet *ifp;
	register int af = addr->sa_family;
	register int (*netmatch)();

	if (af >= AF_MAX)
		return (0);
	netmatch = afswitch[af].af_netmatch;
	for (ifp = ifnet; ifp; ifp = ifp->if_next) {
		if (af != ifp->if_addr.sa_family)
			continue;
		if ((*netmatch)(addr, &ifp->if_addr))
			break;
	}
	return (ifp);
}

struct in_addr
if_makeaddr(net, host)
	int net, host;
{
	u_long addr;

	if (net < 128)
		addr = (net << 24) | host;
	else if (net < 65536)
		addr = (net << 16) | host;
	else
		addr = (net << 8) | host;
#ifdef vax
	addr = htonl(addr);
#endif
	return (*(struct in_addr *)&addr);
}

/*
 * Find the network interfaces attached to this machine.
 * The info is used to::
 *
 * (1) initialize the routing tables, as done by the kernel.
 * (2) ignore incoming packets we send.
 * (3) figure out broadcast capability and addresses.
 * (4) figure out if we're an internetwork gateway.
 *
 * We don't handle anything but Internet addresses.
 */
getinterfaces()
{
	register struct ifnet **pifp, *ifp;
	struct sockaddr_in net;
	struct in_addr logicaladdr;
	int nets;

	nlist("/vmunix", nl);
	if (nl[N_IFNET].n_value == 0) {
		printf("ifnet: symbol not in namelist\n");
		exit(1);
	}
	kmem = open("/dev/kmem", 0);
	if (kmem < 0) {
		perror("/dev/kmem");
		exit(1);
	}
	(void) lseek(kmem, (long)nl[N_IFNET].n_value, 0);
	(void) read(kmem, (char *)&ifnet, sizeof (ifnet));
	bzero((char *)&net, sizeof (net));
	net.sin_family = AF_INET;
	logicaladdr.s_addr = 0;
	nets = 0;
	pifp = &ifnet;
	initializing = 1;
	while (*pifp) {
		struct sockaddr_in *sin;

		(void) lseek(kmem, (long)*pifp, 0);
		ifp = *pifp = (struct ifnet *)malloc(sizeof (struct ifnet));
		if (ifp == 0) {
			printf("routed: out of memory\n");
			break;
		}
		if (read(kmem, (char *)ifp, sizeof (*ifp)) != sizeof (*ifp)) {
			perror("read");
			break;
		}
		if (ifp->if_net == LOOPBACKNET)
			goto skip;
		nets++;
		if ((ifp->if_flags & IFF_UP) == 0)
			goto skip;

		/*
		 * Kludge: don't treat logical host pseudo-interface
		 *	   as a net route, instead fabricate route
		 *	   to get packets back from the gateway.
		 */
		sin = (struct sockaddr_in *)&ifp->if_addr;
		if (sin->sin_family == AF_INET && ifp->if_net == 10 &&
		    sin->sin_addr.s_lh) {
			logicaladdr = sin->sin_addr;
			goto skip;
		}

		/*
		 * Before we can handle point-point links, the interface
		 * structure will have to include an indicator to allow
		 * us to distinguish entries from "network" entries.
		 */
		net.sin_addr = if_makeaddr(ifp->if_net, INADDR_ANY);
		rtadd((struct sockaddr *)&net, (struct sockaddr *)sin, 0);
	skip:
		pifp = &ifp->if_next;
	}
	if (logicaladdr.s_addr) {
		struct rt_entry *rt;

		net.sin_addr = logicaladdr;
		if (ifnet)
			rtadd((struct sockaddr *)&net, &ifnet->if_addr, 0);
		/* yech...yet another logical host kludge */
		rt = rtlookup((struct sockaddr *)&net);
		if (rt)
			rt->rt_flags |= RTF_SILENT;
	}
	(void) close(kmem);
	initializing = 0;
	supplier = nets > 1;
}

/*
 * Send a request message to all directly
 * connected hosts and networks.
 */
request()
{
	register struct rt_entry *rt;
	register struct rt_hash *rh;
	struct rt_hash *base = hosthash;
	int doinghost = 1;

again:
	for (rh = base; rh < &base[ROUTEHASHSIZ]; rh++)
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
		if ((rt->rt_flags & RTF_SILENT) || rt->rt_metric > 0)
			continue;
		getall(rt);
	}
	if (doinghost) {
		base = nethash;
		doinghost = 0;
		goto again;
	}
}

/*
 * Broadcast a new, or modified, routing table entry
 * to all directly connected hosts and networks.
 */
broadcast(entry)
	struct rt_entry *entry;
{
	register struct rt_hash *rh;
	register struct rt_entry *rt;
	register struct sockaddr *dst;
	struct rt_hash *base = hosthash;
	int doinghost = 1;
	struct rip *msg = (struct rip *)packet;

	if (trace)
		log("broadcast", entry);
	msg->rip_cmd = RIPCMD_RESPONSE;
	msg->rip_nets[0].rip_dst = entry->rt_dst;
	msg->rip_nets[0].rip_metric = entry->rt_metric + 1;

again:
	for (rh = base; rh < &base[ROUTEHASHSIZ]; rh++)
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
		if ((rt->rt_flags & RTF_SILENT) || rt->rt_metric > 0)
			continue;
		if (rt->rt_ifp && (rt->rt_ifp->if_flags & IFF_BROADCAST))
			dst = &rt->rt_ifp->if_broadaddr;
		else
			dst = &rt->rt_gateway;
		(*afswitch[dst->sa_family].af_output)(dst, sizeof (struct rip));
	}
	if (doinghost) {
		doinghost = 0;
		base = nethash;
		goto again;
	}
}

/*
 * Supply all directly connected neighbors with the
 * current state of the routing tables.
 */
supplyall()
{
	register struct rt_entry *rt;
	register struct rt_hash *rh;
	register struct sockaddr *dst;
	struct rt_hash *base = hosthash;
	int doinghost = 1;

again:
	for (rh = base; rh < &base[ROUTEHASHSIZ]; rh++)
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
		if ((rt->rt_flags & RTF_SILENT) || rt->rt_metric > 0)
			continue;
		if (rt->rt_ifp && (rt->rt_ifp->if_flags & IFF_BROADCAST))
			dst = &rt->rt_ifp->if_broadaddr;
		else
			dst = &rt->rt_gateway;
		if (trace)
			log("supply", rt);
		supply(dst);
	}
	if (doinghost) {
		base = nethash;
		doinghost = 0;
		goto again;
	}
}

/*
 * Supply routing information to target "sa".
 */
supply(sa)
	struct sockaddr *sa;
{
	struct rip *msg = (struct rip *)packet;
	struct netinfo *n = msg->rip_nets;
	register struct rt_hash *rh;
	register struct rt_entry *rt;
	struct rt_hash *base = hosthash;
	int space = MAXPACKETSIZE - sizeof (int), doinghost = 1;
	int (*output)() = afswitch[sa->sa_family].af_output;

	msg->rip_cmd = RIPCMD_RESPONSE;
again:
	for (rh = base; rh < &base[ROUTEHASHSIZ]; rh++)
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {

		/*
		 * Flush packet out if not enough room for
		 * another routing table entry.
		 */
		if (space < sizeof (struct netinfo)) {
			(*output)(sa, MAXPACKETSIZE - space);
			space = MAXPACKETSIZE - sizeof (int);
			n = msg->rip_nets;
		}
		n->rip_dst = rt->rt_dst;
		n->rip_metric = rt->rt_metric + 1;
		n++, space -= sizeof (struct netinfo);
	}
	if (doinghost) {
		doinghost = 0;
		base = nethash;
		goto again;
	}

	if (space < MAXPACKETSIZE - sizeof (int))
		(*output)(sa, MAXPACKETSIZE - space);
}

getall(rt)
	struct rt_entry *rt;
{
	register struct rip *msg = (struct rip *)packet;
	struct sockaddr *dst;

	msg->rip_cmd = RIPCMD_REQUEST;
	msg->rip_nets[0].rip_dst.sa_family = AF_UNSPEC;
	msg->rip_nets[0].rip_metric = HOPCNT_INFINITY;
	if (rt->rt_ifp && (rt->rt_ifp->if_flags & IFF_BROADCAST))
		dst = &rt->rt_ifp->if_broadaddr;
	else
		dst = &rt->rt_gateway;
	(*afswitch[dst->sa_family].af_output)(dst, sizeof (struct rip));
}

/*
 * Respond to a routing info request.
 */
rip_respond(from, size)
	struct sockaddr *from;
	int size;
{
	register struct rip *msg = (struct rip *)packet;
	struct netinfo *np = msg->rip_nets;
	struct rt_entry *rt;
	int newsize = 0;
	
	size -= sizeof (int);
	while (size > 0) {
		if (size < sizeof (struct netinfo))
			break;
		size -= sizeof (struct netinfo);
		if (np->rip_dst.sa_family == AF_UNSPEC &&
		    np->rip_metric == HOPCNT_INFINITY && size == 0) {
			supply(from);
			return;
		}
		rt = rtlookup(&np->rip_dst);
		np->rip_metric = rt == 0 ? HOPCNT_INFINITY : rt->rt_metric + 1;
		np++, newsize += sizeof (struct netinfo);
	}
	if (newsize > 0) {
		msg->rip_cmd = RIPCMD_RESPONSE;
		newsize += sizeof (int);
		(*afswitch[from->sa_family].af_output)(from, newsize);
	}
}

/*
 * Handle an incoming routing packet.
 */
rip_input(from, size)
	struct sockaddr *from;
	int size;
{
	register struct rip *msg = (struct rip *)packet;
	struct rt_entry *rt;
	struct netinfo *n;

	if (msg->rip_cmd != RIPCMD_RESPONSE &&
	    msg->rip_cmd != RIPCMD_REQUEST)
		return;

	/*
	 * The router port is in the lower 1K of the UDP port space,
	 * and so is priviledged.  Hence we can "authenticate" incoming
	 * updates simply by checking the source port.
	 */
	if (msg->rip_cmd == RIPCMD_RESPONSE &&
	    (*afswitch[from->sa_family].af_portmatch)(from) == 0)
		return;
	if (msg->rip_cmd == RIPCMD_REQUEST) {
		rip_respond(from, size);
		return;
	}

	/*
	 * Process updates.
	 * Extraneous information like Internet ports
	 * must first be purged from the sender's address for
	 * pattern matching below.
	 */
	(*afswitch[from->sa_family].af_canon)(from);
	if (trace)
		printf("input from %x\n", from->sin_addr);
	/*
	 * If response packet is from ourselves, use it only
	 * to reset timer on entry.  Otherwise, we'd believe
	 * it as gospel (since it comes from the router) and
	 * unknowingly update the metric to show the outgoing
	 * cost (higher than our real cost).  I guess the protocol
	 * spec doesn't address this because Xerox Ethernets
	 * don't hear their own broadcasts?
	 */
	if (if_ifwithaddr(from)) {
		rt = rtlookup(from);
		if (rt)
			rt->rt_timer = 0;
		return;
	}
	size -= sizeof (int);
	n = msg->rip_nets;
	for (; size > 0; size -= sizeof (struct netinfo), n++) {
		if (size < sizeof (struct netinfo))
			break;
		if (trace)
			printf("dst %x hc %d...", n->rip_dst.sin_addr,
				n->rip_metric);
		rt = rtlookup(&n->rip_dst);

		/*
		 * Unknown entry, add it to the tables only if
		 * its interesting.
		 */
		if (rt == 0) {
			if (n->rip_metric < HOPCNT_INFINITY)
				rtadd(&n->rip_dst, from, n->rip_metric);
			if (trace)
				printf("new\n");
			continue;
		}

		if (trace)
			printf("ours: gate %x hc %d timer %d\n",
			rt->rt_gateway.sin_addr,
			rt->rt_metric, rt->rt_timer);
		/*
		 * Update the entry if one of the following is true:
		 *
		 * (1) The update came directly from the gateway.
		 * (2) A shorter path is provided.
		 * (3) The entry hasn't been updated in a while
		 *     and a path of equivalent cost is offered.
		 */
		if (equal(from, &rt->rt_gateway) ||
		    rt->rt_metric > n->rip_metric ||
		    (rt->rt_timer > (EXPIRE_TIME/2) &&
		    rt->rt_metric == n->rip_metric)) {
			rtchange(rt, from, n->rip_metric);
			rt->rt_timer = 0;
		}
	}
}

/*
 * Lookup an entry to the appropriate dstination.
 */
struct rt_entry *
rtlookup(dst)
	struct sockaddr *dst;
{
	register struct rt_entry *rt;
	register struct rt_hash *rh;
	register int hash, (*match)();
	struct afhash h;
	int af = dst->sa_family, doinghost = 1;

	if (af >= AF_MAX)
		return (0);
	(*afswitch[af].af_hash)(dst, &h);
	hash = h.afh_hosthash;
	rh = &hosthash[hash % ROUTEHASHSIZ];

again:
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
		if (rt->rt_hash != hash)
			continue;
		if (doinghost) {
			if (equal(&rt->rt_dst, dst))
				return (rt);
		} else {
			if (rt->rt_dst.sa_family == af &&
			    (*match)(&rt->rt_dst, dst))
				return (rt);
		}
	}
	if (doinghost) {
		doinghost = 0;
		hash = h.afh_nethash;
		match = afswitch[af].af_netmatch;
		rh = &nethash[hash % ROUTEHASHSIZ];
		goto again;
	}
	return (0);
}

rtinit()
{
	register struct rt_hash *rh;

	for (rh = nethash; rh < &nethash[ROUTEHASHSIZ]; rh++)
		rh->rt_forw = rh->rt_back = (struct rt_entry *)rh;
	for (rh = hosthash; rh < &hosthash[ROUTEHASHSIZ]; rh++)
		rh->rt_forw = rh->rt_back = (struct rt_entry *)rh;
}

/*
 * Add a new entry.
 */
rtadd(dst, gate, metric)
	struct sockaddr *dst, *gate;
	short metric;
{
	struct afhash h;
	register struct rt_entry *rt;
	struct rt_hash *rh;
	int af = dst->sa_family, flags, hash;

	if (af >= AF_MAX)
		return;
	(*afswitch[af].af_hash)(dst, &h);
	flags = (*afswitch[af].af_checkhost)(dst) ? RTF_HOST : 0;
	if (flags & RTF_HOST) {
		hash = h.afh_hosthash;
		rh = &hosthash[hash % ROUTEHASHSIZ];
	} else {
		hash = h.afh_nethash;
		rh = &nethash[hash % ROUTEHASHSIZ];
	}
	rt = (struct rt_entry *)malloc(sizeof (*rt));
	if (rt == 0)
		return;
	rt->rt_hash = hash;
	rt->rt_dst = *dst;
	rt->rt_gateway = *gate;
	rt->rt_metric = metric;
	rt->rt_timer = 0;
	rt->rt_flags = RTF_UP | flags;
	rt->rt_ifp = if_ifwithnet(&rt->rt_gateway);
	if (metric == 0)
		rt->rt_flags |= RTF_DIRECT;
	insque(rt, rh);
	if (trace)
		log("add", rt);
	if (initializing)
		return;
	if (supplier)
		broadcast(rt);
	if (install) {
		rt->rt_flags |= RTF_ADDRT;
		rt->rt_retry = EXPIRE_TIME/TIMER_RATE;
	}
}

/*
 * Look to see if a change to an existing entry
 * is warranted; if so, make it.
 */
rtchange(rt, gate, metric)
	struct rt_entry *rt;
	struct sockaddr *gate;
	short metric;
{
	int change = 0;

	if (!equal(&rt->rt_gateway, gate)) {
		rt->rt_gateway = *gate;
		change++;
	}

	/*
	 * If the hop count has changed, adjust
	 * the flags in the routing table entry accordingly.
	 */
	if (metric != rt->rt_metric) {
		if (rt->rt_metric == 0)
			rt->rt_flags &= ~RTF_DIRECT;
		rt->rt_metric = metric;
		if (metric >= HOPCNT_INFINITY)
			rt->rt_flags &= ~RTF_UP;
		else
			rt->rt_flags |= RTF_UP;
		change++;
	}

	if (!change)
		return;
	if (supplier)
		broadcast(rt);
	if (trace)
		log("change", rt);
	if (install) {
		rt->rt_flags |= RTF_CHGRT;
		rt->rt_retry = EXPIRE_TIME/TIMER_RATE;
	}
}

/*
 * Delete a routing table entry.
 */
rtdelete(rt)
	struct rt_entry *rt;
{
	if (trace)
		log("delete", rt);
	if (install)
		if (ioctl(s, SIOCDELRT, (char *)&rt->rt_hash) &&
		  errno == EBUSY)
			rt->rt_flags |= RTF_DELRT;
	remque(rt);
	free((char *)rt);
}

/*
 * Timer routine:
 *
 * o handle timers on table entries,
 * o invalidate entries which haven't been updated in a while,
 * o delete entries which are too old,
 * o retry ioctl's which weren't successful the first
 *   time due to the kernel entry being busy
 * o if we're an internetwork router, supply routing updates
 *   periodically
 */
timer()
{
	register struct rt_hash *rh;
	register struct rt_entry *rt;
	struct rt_hash *base = hosthash;
	int doinghost = 1;

	if (trace)
		printf(">>> time %d >>>\n", timeval);
again:
	for (rh = base; rh < &base[ROUTEHASHSIZ]; rh++) {
		rt = rh->rt_forw;
		for (; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {

			/*
			 * If the host is indicated to be
			 * "silent" (i.e. it's a logical host,
			 * or one we got from the initialization
			 * file), don't time out it's entry.
			 */
			if (rt->rt_flags & RTF_SILENT)
				continue;
			if (trace)
				log("", rt);
			rt->rt_timer += TIMER_RATE;
			if (rt->rt_timer >= GARBAGE_TIME ||
			  (rt->rt_flags & RTF_DELRT)) {
				rt = rt->rt_forw;
				rtdelete(rt->rt_back);
				rt = rt->rt_back;
				continue;
			}
			if (rt->rt_timer >= EXPIRE_TIME)
				rt->rt_metric = HOPCNT_INFINITY;
			if (rt->rt_flags & RTF_CHGRT)
				if (!ioctl(s, SIOCCHGRT,(char *)&rt->rt_hash) ||
				  --rt->rt_retry == 0)
					rt->rt_flags &= ~RTF_CHGRT;
			if (rt->rt_flags & RTF_ADDRT)
				if (!ioctl(s, SIOCADDRT,(char *)&rt->rt_hash) ||
				  --rt->rt_retry == 0)
					rt->rt_flags &= ~RTF_ADDRT;
		}
	}
	if (doinghost) {
		doinghost = 0;
		base = nethash;
		goto again;
	}
	timeval += TIMER_RATE;
	if (supplier && (timeval % SUPPLY_INTERVAL) == 0)
		supplyall();
	if (trace)
		printf("<<< time %d <<<\n", timeval);
	alarm(TIMER_RATE);
}

log(operation, rt)
	char *operation;
	struct rt_entry *rt;
{
	time_t t = time(0);
	struct sockaddr_in *dst, *gate;
	static struct flagbits {
		int	t_bits;
		char	*t_name;
	} bits[] = {
		{ RTF_UP,	"UP" },
		{ RTF_DIRECT,	"DIRECT" },
		{ RTF_HOST,	"HOST" },
		{ RTF_DELRT,	"DELETE" },
		{ RTF_CHGRT,	"CHANGE" },
		{ RTF_SILENT,	"SILENT" },
		{ 0 }
	};
	register struct flagbits *p;
	register int first;
	char *cp;

	printf("%s ", operation);
	dst = (struct sockaddr_in *)&rt->rt_dst;
	gate = (struct sockaddr_in *)&rt->rt_gateway;
	printf("dst %x, router %x, metric %d, flags ",
		dst->sin_addr, gate->sin_addr, rt->rt_metric);
	cp = "%s";
	for (first = 1, p = bits; p->t_bits > 0; p++) {
		if ((rt->rt_flags & p->t_bits) == 0)
			continue;
		printf(cp, p->t_name);
		if (first) {
			cp = "|%s";
			first = 0;
		}
	}
	putchar('\n');
}
