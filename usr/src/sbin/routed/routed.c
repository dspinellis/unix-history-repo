#ifndef lint
static char sccsid[] = "@(#)routed.c	4.13 %G%";
#endif

/*
 * Routing Table Management Daemon
 */
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <net/in.h>
#include <net/if.h>
#include <errno.h>
#include <stdio.h>
#include <nlist.h>
#include <signal.h>
#include <time.h>
#include "rip.h"
#include "router.h"

#define	LOOPBACKNET	0177
/* casts to keep lint happy */
#define	insque(q,p)	_insque((caddr_t)q,(caddr_t)p)
#define	remque(q)	_remque((caddr_t)q)
#define equal(a1, a2) \
	(bcmp((caddr_t)(a1), (caddr_t)(a2), sizeof (struct sockaddr)) == 0)
#define	min(a,b)	((a)>(b)?(b):(a))

struct nlist nl[] = {
#define	N_IFNET		0
	{ "_ifnet" },
	0,
};

struct	sockaddr_in myaddr = { AF_INET, IPPORT_ROUTESERVER };

int	s;
int	snoroute;		/* socket with no routing */
int	kmem = -1;
int	supplier;		/* process should supply updates */
int	install = 1;		/* if 1 call kernel */
int	lookforinterfaces = 1;
int	performnlist = 1;
int	timeval;
int	timer();
int	cleanup();
int	trace = 0;
FILE	*ftrace;

char	packet[MAXPACKETSIZE];

struct in_addr if_makeaddr();
struct ifnet *if_ifwithaddr(), *if_ifwithnet();
extern char *malloc();
extern int errno, exit();

main(argc, argv)
	int argc;
	char *argv[];
{
	int cc;
	struct sockaddr from;
	
#ifndef DEBUG
	if (fork())
		exit(0);
	for (cc = 0; cc < 10; cc++)
		(void) close(cc);
	(void) open("/", 0);
	(void) dup2(0, 1);
	(void) dup2(0, 2);
	{ int t = open("/dev/tty", 2);
	  if (t >= 0) {
		ioctl(t, TIOCNOTTY, (char *)0);
		(void) close(t);
	  }
	}
#endif
	if (trace) {
		ftrace = fopen("/etc/routerlog", "w");
		dup2(fileno(ftrace), 1);
		dup2(fileno(ftrace), 2);
	}
#ifdef vax || pdp11
	myaddr.sin_port = htons(myaddr.sin_port);
#endif
again:
	s = socket(SOCK_DGRAM, 0, &myaddr, 0);
	if (s < 0) {
		perror("socket");
		sleep(30);
		goto again;
	}
again2:
	snoroute = socket(SOCK_DGRAM, 0, 0, SO_DONTROUTE);
	if (snoroute < 0) {
		perror("socket");
		sleep(30);
		goto again2;
	}
	rtinit();
	getinterfaces();
	request();

	argv++, argc--;
	while (argc > 0) {
		if (strcmp(*argv, "-s") == 0)
			supplier = 1;
		else if (strcmp(*argv, "-q") == 0)
			supplier = 0;
		argv++, argc--;
	}
	sigset(SIGALRM, timer);
	timer();

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
 * Timer routine:
 *
 * o handle timers on table entries,
 * o invalidate entries which haven't been updated in a while,
 * o delete entries which are too old,
 * o if we're an internetwork router, supply routing updates
 *   periodically
 */
timer()
{
	register struct rthash *rh;
	register struct rt_entry *rt;
	struct rthash *base = hosthash;
	int doinghost = 1;

	if (trace)
		printf(">>> time %d >>>\n", timeval);
again:
	for (rh = base; rh < &base[ROUTEHASHSIZ]; rh++) {
		rt = rh->rt_forw;
		for (; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {

			/*
			 * If the host is indicated to be
			 * "hidden" (i.e. it's one we got
			 * from the initialization file),
			 * don't time out it's entry.
			 */
			if ((rt->rt_state & RTS_PASSIVE) == 0)
				rt->rt_timer += TIMER_RATE;
			log("", rt);

			/*
			 * If the entry should be deleted
			 * attempt to do so and reclaim space.
			 */
			if (rt->rt_timer >= GARBAGE_TIME ||
			  (rt->rt_state & RTS_DELRT)) {
				rt = rt->rt_back;
				rtdelete(rt->rt_forw);
				continue;
			}

			/*
			 * If we haven't heard from the router
			 * in a long time indicate the route is
			 * hard to reach.
			 */
			if (rt->rt_timer >= EXPIRE_TIME)
				rt->rt_metric = HOPCNT_INFINITY;

			/*
			 * If a change or addition is to be made
			 * and this isn't time to broadcast an
			 * update, then broadcast the change.
			 */
			if ((rt->rt_state & (RTS_CHGRT|RTS_ADDRT)) &&
			    supplier &&
			    (timeval + TIMER_RATE) % SUPPLY_INTERVAL)
				broadcast(rt);

			if (rt->rt_state & RTS_CHGRT) {
				struct rtentry oldroute;

				oldroute = rt->rt_rt;
				rt->rt_router = rt->rt_newrouter;
				if (ioctl(s, SIOCADDRT, (char *)&rt->rt_rt) < 0)
					perror("SIOCADDRT");
				if (ioctl(s, SIOCDELRT, (char *)&oldroute) < 0)
					perror("SIOCDELRT");
				rt->rt_state &= ~RTS_CHGRT;
			}
			if (rt->rt_state & RTS_ADDRT) {
				if (ioctl(s, SIOCADDRT, (char *)&rt->rt_rt) < 0)
					perror("SIOCADDRT");
				rt->rt_state &= ~RTS_ADDRT;
			}
		}
	}
	if (doinghost) {
		doinghost = 0;
		base = nethash;
		goto again;
	}
	timeval += TIMER_RATE;
	if (lookforinterfaces && (timeval % CHECK_INTERVAL) == 0)
		getinterfaces();
	if (supplier && (timeval % SUPPLY_INTERVAL) == 0)
		supplyall();
	if (trace)
		printf("<<< time %d <<<\n", timeval);
	alarm(TIMER_RATE);
}

struct	ifnet *ifnet;
/*
 * Find the network interfaces attached to this machine.
 * The info is used to:
 *
 * (1) initialize the routing tables, as done by the kernel.
 * (2) ignore incoming packets we send.
 * (3) figure out broadcast capability and addresses.
 * (4) figure out if we're an internetwork gateway.
 *
 * We don't handle anything but Internet addresses.
 *
 * Note: this routine may be called periodically to
 * scan for new interfaces.  In fact, the timer routine
 * does so based on the flag lookforinterfaces.  The
 * flag performnlist is set whenever something odd occurs
 * while scanning the kernel; this is likely to occur
 * if /vmunix isn't up to date (e.g. someone booted /ovmunix).
 */
getinterfaces()
{
	struct ifnet *ifp;
	struct ifnet ifstruct, *next;
	struct sockaddr_in net;
	register struct sockaddr *dst;
	int nets;

	if (performnlist) {
		nlist("/vmunix", nl);
		if (nl[N_IFNET].n_value == 0) {
			performnlist++;
			printf("ifnet: not in namelist\n");
			return;
		}
		performnlist = 0;
	}
	if (kmem < 0) {
		kmem = open("/dev/kmem", 0);
		if (kmem < 0) {
			perror("/dev/kmem");
			return;
		}
	}
	if (lseek(kmem, (long)nl[N_IFNET].n_value, 0) == -1 ||
	    read(kmem, (char *)&ifp, sizeof (ifp)) != sizeof (ifp)) {
		performnlist = 1;
		return;
	}
	bzero((char *)&net, sizeof (net));
	net.sin_family = AF_INET;
	lookforinterfaces = 0;
	nets = 0;
	while (ifp) {
		if (lseek(kmem, (long)ifp, 0) == -1 ||
		  read(kmem, (char *)&ifstruct, sizeof (ifstruct)) !=
		  sizeof (ifstruct)) {
			perror("read");
			lookforinterfaces = performnlist = 1;
			break;
		}
		ifp = &ifstruct;
		if ((ifp->if_flags & IFF_UP) == 0) {
			lookforinterfaces = 1;
	skip:
			ifp = ifp->if_next;
			continue;
		}
		if (ifp->if_addr.sa_family != AF_INET)
			goto skip;
		/* ignore software loopback networks. */
		if (ifp->if_net == LOOPBACKNET)
			goto skip;
		/* check if we already know about this one */
		if (if_ifwithaddr(&ifstruct.if_addr)) {
			nets++;
			goto skip;
		}
		ifp = (struct ifnet *)malloc(sizeof (struct ifnet));
		if (ifp == 0) {
			printf("routed: out of memory\n");
			break;
		}
		bcopy((char *)&ifstruct, (char *)ifp, sizeof (struct ifnet));

		/*
		 * Count the # of directly connected networks
		 * and point to point links which aren't looped
		 * back to ourself.  This is used below to
		 * decide if we should be a routing "supplier".
		 */
		if ((ifp->if_flags & IFF_POINTOPOINT) == 0 ||
		    if_ifwithaddr(&ifp->if_dstaddr) == 0)
			nets++;

		if (ifp->if_flags & IFF_POINTOPOINT)
			dst = &ifp->if_dstaddr;
		else {
			net.sin_addr = if_makeaddr(ifp->if_net, INADDR_ANY);
			dst = (struct sockaddr *)&net;
		}
		next = ifp->if_next;
		ifp->if_next = ifnet;
		ifnet = ifp;
		if (rtlookup(dst) == 0) {
			struct rt_entry *rt;

			rtadd(dst, &ifp->if_addr, 0);
			rt = rtlookup(dst);
			if (rt)
				rt->rt_state |= RTS_INTERFACE;
		}
		ifp = next;
	}
	supplier = nets > 1;
	getothers();
}

/*
 * Look in a file for any gateways we should configure
 * outside the directly connected ones.  This is a kludge,
 * but until we can find out about gateways on the "other side"
 * of the ARPANET using GGP, it's a must.
 *
 * File format is one entry per line, 
 *	destination gateway flags	(all hex #'s)
 *
 * We don't really know the distance to the gateway, so we
 * assume it's a neighbor.
 */
getothers()
{
	struct sockaddr_in dst, gate;
	FILE *fp;
	struct rt_entry *rt;
	char flags[132];

	fp = fopen("/etc/gateways", "r");
	if (fp == NULL)
		return;
	bzero((char *)&dst, sizeof (dst));
	bzero((char *)&gate, sizeof (gate));
	dst.sin_family = AF_INET;
	gate.sin_family = AF_INET;
	flags[0] = '\0';
	while (fscanf(fp, "dst %x gateway %x %s\n", &dst.sin_addr.s_addr, 
	   &gate.sin_addr.s_addr, flags) != EOF) {
		if (rt = rtlookup((struct sockaddr *)&dst)) {
			flags[0] = '\0';
			continue;
		}
		rtadd((struct sockaddr *)&dst,(struct sockaddr *)&gate,1);
		rt = rtlookup(&dst);
		if (strcmp(flags, "passive") == 0)
			rt->rt_state |= RTS_PASSIVE;
		flags[0] = '\0';
	}
	fclose(fp);
}

/*
 * Send a request message to all directly
 * connected hosts and networks.
 */
request()
{
	register struct rip *msg = (struct rip *)packet;

	msg->rip_cmd = RIPCMD_REQUEST;
	msg->rip_nets[0].rip_dst.sa_family = AF_UNSPEC;
	msg->rip_nets[0].rip_metric = HOPCNT_INFINITY;
	sendall();
}

/*
 * Broadcast a new, or modified, routing table entry
 * to all directly connected hosts and networks.
 */
broadcast(entry)
	struct rt_entry *entry;
{
	struct rip *msg = (struct rip *)packet;

	log("broadcast", entry);
	msg->rip_cmd = RIPCMD_RESPONSE;
	msg->rip_nets[0].rip_dst = entry->rt_dst;
	msg->rip_nets[0].rip_metric = min(entry->rt_metric+1, HOPCNT_INFINITY);
	sendall();
}

/*
 * Send "packet" to all neighbors.
 */
sendall()
{
	register struct rthash *rh;
	register struct rt_entry *rt;
	register struct sockaddr *dst;
	struct rthash *base = hosthash;
	int doinghost = 1;

again:
	for (rh = base; rh < &base[ROUTEHASHSIZ]; rh++)
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
		if ((rt->rt_state&RTS_PASSIVE) || rt->rt_metric > 0)
			continue;
		if (rt->rt_ifp && (rt->rt_ifp->if_flags & IFF_BROADCAST))
			dst = &rt->rt_ifp->if_broadaddr;
		else
			dst = &rt->rt_router;
		(*afswitch[dst->sa_family].af_output)
			(s, dst, sizeof (struct rip));
	}
	if (doinghost) {
		base = nethash;
		doinghost = 0;
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
	register struct rthash *rh;
	register struct sockaddr *dst;
	struct rthash *base = hosthash;
	int doinghost = 1;

again:
	for (rh = base; rh < &base[ROUTEHASHSIZ]; rh++)
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
		if ((rt->rt_state&RTS_PASSIVE) ||
		    (!(rt->rt_state&RTS_INTERFACE) && rt->rt_metric > 0))
			continue;
		if (rt->rt_ifp && (rt->rt_ifp->if_flags & IFF_BROADCAST))
			dst = &rt->rt_ifp->if_broadaddr;
		else
			dst = &rt->rt_router;
		log("supply", rt);
		supply(rt->rt_state&RTS_INTERFACE ? snoroute : s, dst);
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
supply(s, sa)
	int s;
	struct sockaddr *sa;
{
	struct rip *msg = (struct rip *)packet;
	struct netinfo *n = msg->rip_nets;
	register struct rthash *rh;
	register struct rt_entry *rt;
	struct rthash *base = hosthash;
	int doinghost = 1, size;
	int (*output)() = afswitch[sa->sa_family].af_output;

	msg->rip_cmd = RIPCMD_RESPONSE;
again:
	for (rh = base; rh < &base[ROUTEHASHSIZ]; rh++)
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {

		/*
		 * Flush packet out if not enough room for
		 * another routing table entry.
		 */
		size = (char *)n - packet;
		if (size > MAXPACKETSIZE - sizeof (struct netinfo)) {
			(*output)(s, sa, size);
			n = msg->rip_nets;
		}
		n->rip_dst = rt->rt_dst;
		n->rip_metric = min(rt->rt_metric + 1, HOPCNT_INFINITY);
		n++;
	}
	if (doinghost) {
		doinghost = 0;
		base = nethash;
		goto again;
	}
	if (n != msg->rip_nets)
		(*output)(s, sa, (char *)n - packet);
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
	
	size -= 4 * sizeof (char);
	while (size > 0) {
		if (size < sizeof (struct netinfo))
			break;
		size -= sizeof (struct netinfo);
		if (np->rip_dst.sa_family == AF_UNSPEC &&
		    np->rip_metric == HOPCNT_INFINITY && size == 0) {
			supply(s, from);
			return;
		}
		rt = rtlookup(&np->rip_dst);
		np->rip_metric = rt == 0 ?
			HOPCNT_INFINITY : min(rt->rt_metric+1, HOPCNT_INFINITY);
		np++, newsize += sizeof (struct netinfo);
	}
	if (newsize > 0) {
		msg->rip_cmd = RIPCMD_RESPONSE;
		newsize += sizeof (int);
		(*afswitch[from->sa_family].af_output)(s, from, newsize);
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

	switch (msg->rip_cmd) {

	default:
		return;

	case RIPCMD_REQUEST:
		rip_respond(from, size);
		return;

	case RIPCMD_TRACEON:
	case RIPCMD_TRACEOFF:
		/* verify message came from a priviledged port */
		if ((*afswitch[from->sa_family].af_portcheck)(from) == 0)
			return;
		tracecmd(msg->rip_cmd, msg, size);
		return;

	case RIPCMD_RESPONSE:
		/* verify message came from a router */
		if ((*afswitch[from->sa_family].af_portmatch)(from) == 0)
			return;
		break;
	}

	/*
	 * Process updates.
	 * Extraneous information like Internet ports
	 * must first be purged from the sender's address for
	 * pattern matching below.
	 */
	(*afswitch[from->sa_family].af_canon)(from);
	if (trace)
		printf("input from %x\n",
			((struct sockaddr_in *)from)->sin_addr);
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
		rt = rtfind(from);
		if (rt)
			rt->rt_timer = 0;
		return;
	}
	size -= 4 * sizeof (char);
	n = msg->rip_nets;
	for (; size > 0; size -= sizeof (struct netinfo), n++) {
		if (size < sizeof (struct netinfo))
			break;
		if (trace)
			printf("dst %x hc %d...",
				((struct sockaddr_in *)&n->rip_dst)->sin_addr,
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
			  ((struct sockaddr_in *)&rt->rt_router)->sin_addr,
			  rt->rt_metric, rt->rt_timer);
		/*
		 * Update the entry if one of the following is true:
		 *
		 * (1) The update came directly from the gateway.
		 * (2) A shorter path is provided.
		 * (3) The entry hasn't been updated in a while
		 *     and a path of equivalent cost is offered
		 *     (with the cost finite).
		 */
		if (equal(from, &rt->rt_router) ||
		    rt->rt_metric > n->rip_metric ||
		    (rt->rt_timer > (EXPIRE_TIME/2) &&
		    rt->rt_metric == n->rip_metric &&
		    rt->rt_metric < HOPCNT_INFINITY)) {
			rtchange(rt, from, n->rip_metric);
			rt->rt_timer = 0;
		}
	}
}

/*
 * Handle tracing requests.
 */
tracecmd(cmd, msg, size)
	int cmd, size;
	struct rip *msg;
{
	time_t t;

	if (cmd == RIPCMD_TRACEOFF) {
		if (!trace)
			return;
		t = time(0);
		printf("*** Tracing turned off at %.24s ***\n", ctime(&t));
		fflush(stdout), fflush(stderr);
		if (ftrace)
			fclose(ftrace);
		(void) close(1), (void) close(2);
		trace = 0;
		return;
	}
	if (trace)
		return;
	packet[size] = '\0';
	ftrace = fopen(msg->rip_tracefile, "a");
	if (ftrace == NULL)
		return;
	(void) dup2(fileno(ftrace), 1);
	(void) dup2(fileno(ftrace), 2);
	trace = 1;
	t = time(0);
	printf("*** Tracing turned on at %.24s ***\n", ctime(&t));
}

rtinit()
{
	register struct rthash *rh;

	for (rh = nethash; rh < &nethash[ROUTEHASHSIZ]; rh++)
		rh->rt_forw = rh->rt_back = (struct rt_entry *)rh;
	for (rh = hosthash; rh < &hosthash[ROUTEHASHSIZ]; rh++)
		rh->rt_forw = rh->rt_back = (struct rt_entry *)rh;
}

/*
 * Lookup an entry to the appropriate dstination.
 */
struct rt_entry *
rtlookup(dst)
	struct sockaddr *dst;
{
	register struct rt_entry *rt;
	register struct rthash *rh;
	register int hash;
	struct afhash h;
	int doinghost = 1;

	if (dst->sa_family >= AF_MAX)
		return (0);
	(*afswitch[dst->sa_family].af_hash)(dst, &h);
	hash = h.afh_hosthash;
	rh = &hosthash[hash % ROUTEHASHSIZ];
again:
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
		if (rt->rt_hash != hash)
			continue;
		if (equal(&rt->rt_dst, dst))
			return (rt);
	}
	if (doinghost) {
		doinghost = 0;
		hash = h.afh_nethash;
		rh = &nethash[hash % ROUTEHASHSIZ];
		goto again;
	}
	return (0);
}

/*
 * Find an entry based on address "dst", as the kernel
 * does in selecting routes.  This means we look first
 * for a point to point link, settling for a route to
 * the destination network if the former fails.
 */
struct rt_entry *
rtfind(dst)
	struct sockaddr *dst;
{
	register struct rt_entry *rt;
	register struct rthash *rh;
	register int hash;
	struct afhash h;
	int af = dst->sa_family;
	int doinghost = 1, (*match)();

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
		rh = &nethash[hash % ROUTEHASHSIZ];
		match = afswitch[af].af_netmatch;
		goto again;
	}
	return (0);
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
	struct rthash *rh;
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
	rt->rt_router = *gate;
	rt->rt_metric = metric;
	rt->rt_timer = 0;
	rt->rt_flags = RTF_UP | flags;
	rt->rt_state = 0;
	rt->rt_ifp = if_ifwithnet(&rt->rt_router);
	if (metric)
		rt->rt_flags |= RTF_GATEWAY;
	insque(rt, rh);
	log("add", rt);
	if (install)
		rt->rt_state |= RTS_ADDRT;
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

	if (!equal(&rt->rt_router, gate)) {
		rt->rt_newrouter = *gate;
		change++;
	}
	if (metric != rt->rt_metric) {
		if (metric == 0)
			rt->rt_flags |= RTF_GATEWAY;
		rt->rt_metric = metric;
		change++;
	}
	if (!change)
		return;
	log("change", rt);
	if (install)
		rt->rt_state |= RTS_CHGRT;
}

/*
 * Delete a routing table entry.
 */
rtdelete(rt)
	struct rt_entry *rt;
{
	log("delete", rt);
	if (install && ioctl(s, SIOCDELRT, (char *)&rt->rt_rt))
		perror("SIOCDELRT");
	/* don't delete interface entries so we can poll them later */
	if (rt->rt_state & RTS_INTERFACE)
		return;
	remque(rt);
	free((char *)rt);
}

log(operation, rt)
	char *operation;
	struct rt_entry *rt;
{
	time_t t = time(0);
	struct sockaddr_in *dst, *gate;
	static struct bits {
		int	t_bits;
		char	*t_name;
	} flagbits[] = {
		{ RTF_UP,	"UP" },
		{ RTF_GATEWAY,	"GATEWAY" },
		{ RTF_HOST,	"HOST" },
		{ 0 }
	}, statebits[] = {
		{ RTS_DELRT,	"DELETE" },
		{ RTS_CHGRT,	"CHANGE" },
		{ RTS_PASSIVE,	"PASSIVE" },
		{ RTS_INTERFACE,"INTERFACE" },
		{ 0 }
	};
	register struct bits *p;
	register int first;
	char *cp;

	if (trace == 0)
		return;
	printf("%s ", operation);
	dst = (struct sockaddr_in *)&rt->rt_dst;
	gate = (struct sockaddr_in *)&rt->rt_router;
	printf("dst %x, router %x, metric %d, flags",
		dst->sin_addr, gate->sin_addr, rt->rt_metric);
	cp = " %s";
	for (first = 1, p = flagbits; p->t_bits > 0; p++) {
		if ((rt->rt_flags & p->t_bits) == 0)
			continue;
		printf(cp, p->t_name);
		if (first) {
			cp = "|%s";
			first = 0;
		}
	}
	printf(" state");
	cp = " %s";
	for (first = 1, p = statebits; p->t_bits > 0; p++) {
		if ((rt->rt_state & p->t_bits) == 0)
			continue;
		printf(cp, p->t_name);
		if (first) {
			cp = "|%s";
			first = 0;
		}
	}
	putchar('\n');
}

/*
 * Find the interface with address "addr".
 */
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

/*
 * Find the interface with network imbedded in
 * the sockaddr "addr".  Must use per-af routine
 * look for match.
 */
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

/*
 * Formulate an Internet address.
 */
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
