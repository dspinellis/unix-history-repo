#ifndef lint
static char sccsid[] = "@(#)routed.c	4.15 82/06/09";
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
#define	RIPCMDS
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

struct	sockaddr_in routingaddr = { AF_INET, IPPORT_ROUTESERVER };
struct	sockaddr_in noroutingaddr = { AF_INET, IPPORT_ROUTESERVER+1 };

int	s;
int	snoroute;		/* socket with no routing */
int	kmem = -1;
int	supplier = -1;		/* process should supply updates */
int	install = 1;		/* if 1 call kernel */
int	timeval = -TIMER_RATE;
int	timer();
int	cleanup();

#define tprintf if (trace) printf
int	trace = 0;
FILE	*ftrace;

char	packet[MAXPACKETSIZE+1];
struct	rip *msg = (struct rip *)packet;

struct in_addr if_makeaddr();
struct ifnet *if_ifwithaddr(), *if_ifwithnet();
extern char *malloc();
extern int errno, exit();
char	**argv0;

int	sendmsg(), supply();

main(argc, argv)
	int argc;
	char *argv[];
{
	int cc;
	struct sockaddr from;
	
	argv0 = argv;
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
	routingaddr.sin_port = htons(routingaddr.sin_port);
	noroutingaddr.sin_port = htons(noroutingaddr.sin_port);
#endif
again:
	s = socket(SOCK_DGRAM, 0, &routingaddr, 0);
	if (s < 0) {
		perror("socket");
		sleep(30);
		goto again;
	}
again2:
	snoroute = socket(SOCK_DGRAM, 0, &noroutingaddr, SO_DONTROUTE);
	if (snoroute < 0) {
		perror("socket");
		sleep(30);
		goto again2;
	}
	argv++, argc--;
	while (argc > 0 && **argv == '-') {
		if (!strcmp(*argv, "-s") == 0) {
			supplier = 1;
			argv++, argc--;
			continue;
		}
		if (!strcmp(*argv, "-q") == 0) {
			supplier = 0;
			argv++, argc--;
			continue;
		}
		goto usage;
	}
	if (argc > 0) {
usage:
		fprintf(stderr, "usage: routed [ -s ]\n");
		exit(1);
	}
	rtinit();
	ifinit();
	if (supplier < 0)
		supplier = 0;
	gwkludge();
	msg->rip_cmd = RIPCMD_REQUEST;
	msg->rip_nets[0].rip_dst.sa_family = AF_UNSPEC;
	msg->rip_nets[0].rip_metric = HOPCNT_INFINITY;
	toall(sendmsg);
	sigset(SIGALRM, timer);
	timer();

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

rtinit()
{
	register struct rthash *rh;

	for (rh = nethash; rh < &nethash[ROUTEHASHSIZ]; rh++)
		rh->rt_forw = rh->rt_back = (struct rt_entry *)rh;
	for (rh = hosthash; rh < &hosthash[ROUTEHASHSIZ]; rh++)
		rh->rt_forw = rh->rt_back = (struct rt_entry *)rh;
}

struct	ifnet *ifnet;

ifinit()
{
	struct ifnet *ifp, *next;
	int uniquemultihostinterfaces = 0;

	nlist("/vmunix", nl);
	if (nl[N_IFNET].n_value == 0) {
		printf("ifnet: not in namelist\n");
		goto bad;
	}
	kmem = open("/dev/kmem", 0);
	if (kmem < 0) {
		perror("/dev/kmem");
		goto bad;
	}
	if (lseek(kmem, (long)nl[N_IFNET].n_value, 0) == -1 ||
	    read(kmem, (char *)&next, sizeof (next)) != sizeof (next)) {
		printf("ifnet: error reading kmem\n");
		goto bad;
	}
	while (next) {
		ifp = (struct ifnet *)malloc(sizeof (struct ifnet));
		if (ifp == 0) {
			printf("routed: out of memory\n");
			break;
		}
		if (lseek(kmem, (long)next, 0) == -1 ||
		    read(kmem, (char *)ifp, sizeof (*ifp)) != sizeof (*ifp)) {
			perror("read");
			goto bad;
		}
		next = ifp->if_next;
		if (ifp->if_addr.sa_family != AF_INET)
			continue;
		if (ifp->if_net == LOOPBACKNET)
			continue;
		if ((ifp->if_flags & IFF_POINTOPOINT) == 0 ||
		    if_ifwithaddr(&ifp->if_dstaddr) == 0)
			uniquemultihostinterfaces++;
		ifp->if_next = ifnet;
		ifnet = ifp;
		addrouteforif(ifp);
	}
	if (uniquemultihostinterfaces > 1 && supplier < 0)
		supplier = 1;
	return;
bad:
	sleep(60);
	execv("/etc/routed", argv0);
	_exit(0177);
}

addrouteforif(ifp)
	struct ifnet *ifp;
{
	struct sockaddr_in net;
	struct sockaddr *dst;

	if (ifp->if_flags & IFF_POINTOPOINT)
		dst = &ifp->if_dstaddr;
	else {
		bzero((char *)&net, sizeof (net));
		net.sin_family = AF_INET;
		net.sin_addr = if_makeaddr(ifp->if_net, INADDR_ANY);
		dst = (struct sockaddr *)&net;
	}
	rtadd(dst, &ifp->if_addr, 0, RTS_DONTDELETE|RTS_DONTROUTE);
}

gwkludge()
{
	struct sockaddr_in dst, gate;
	FILE *fp;
	char buf[BUFSIZ];

	fp = fopen("/etc/gateways", "r");
	if (fp == NULL)
		return;
	bzero((char *)&dst, sizeof (dst));
	bzero((char *)&gate, sizeof (gate));
	dst.sin_family = AF_INET;
	gate.sin_family = AF_INET;
	for (;;) {
		buf[0] = '\0';
		/* format is: dst XX gateway XX [passive]\n */
		if (fscanf(fp, "dst %x gateway %x %s\n", &dst.sin_addr.s_addr, 
		   &gate.sin_addr.s_addr, buf) == EOF)
			break;
		rtadd((struct sockaddr *)&dst, (struct sockaddr *)&gate, 1,
		   strcmp(buf, "passive") == 0 ? RTS_PASSIVE : RTS_DONTDELETE);
	}
	fclose(fp);
}

timer()
{
	register struct rthash *rh;
	register struct rt_entry *rt;
	struct rthash *base = hosthash;
	int doinghost = 1, state;

	timeval += TIMER_RATE;
	tprintf(">>> time %d >>>\n", timeval);
again:
	for (rh = base; rh < &base[ROUTEHASHSIZ]; rh++) {
		rt = rh->rt_forw;
		for (; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
			if (!(rt->rt_state & RTS_PASSIVE)) {
				rt->rt_timer += TIMER_RATE;
				if (rt->rt_timer > 9999)
					rt->rt_timer = 9999;
			}
			log("", rt);
			if (rt->rt_state & RTS_HIDDEN)
				continue;
			if (rt->rt_timer >= EXPIRE_TIME)
				rt->rt_metric = HOPCNT_INFINITY;
			if ((rt->rt_state & RTS_DELRT) ||
			    rt->rt_timer >= GARBAGE_TIME) {
				rt = rt->rt_back;
				rtdelete(rt->rt_forw);
				continue;
			}
			state = rt->rt_state;
			if (rt->rt_state & RTS_ADDRT) {
				if (ioctl(s, SIOCADDRT,(char *)&rt->rt_rt) < 0)
					perror("SIOCADDRT");
				rt->rt_state &= ~RTS_ADDRT;
			}
			if (rt->rt_state & RTS_CHGRT) {
				struct rtentry oldroute;

				oldroute = rt->rt_rt;
				oldroute.rt_gateway = rt->rt_oldrouter;
				if (install &&
				    ioctl(s, SIOCADDRT,(char *)&rt->rt_rt) < 0)
					perror("SIOCADDRT");
				if (install &&
				    ioctl(s, SIOCDELRT, (char *)&oldroute) < 0)
					perror("SIOCDELRT");
				rt->rt_state &= ~RTS_CHGRT;
			}
			if (supplier && (state & (RTS_CHGRT|RTS_ADDRT))) {
				log("broadcast", rt);
				msg->rip_cmd = RIPCMD_RESPONSE;
				msg->rip_nets[0].rip_dst = rt->rt_dst;
				msg->rip_nets[0].rip_metric =
				    min(rt->rt_metric+1, HOPCNT_INFINITY);
				toall(sendmsg);
			}
		}
	}
	if (doinghost) {
		doinghost = 0;
		base = nethash;
		goto again;
	}
	if (supplier && (timeval % SUPPLY_INTERVAL) == 0)
		toall(supply);
	tprintf("<<< time %d <<<\n", timeval);
	alarm(TIMER_RATE);
}

toall(f)
	int (*f)();
{
	register struct rthash *rh;
	register struct rt_entry *rt;
	register struct sockaddr *dst;
	struct rthash *base = hosthash;
	int doinghost = 1;

again:
	for (rh = base; rh < &base[ROUTEHASHSIZ]; rh++)
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
		if ((rt->rt_state & RTS_PASSIVE) || rt->rt_metric > 0)
			continue;
		if (rt->rt_ifp && (rt->rt_ifp->if_flags & IFF_BROADCAST))
			dst = &rt->rt_ifp->if_broadaddr;
		else
			dst = &rt->rt_router;
		(*f)(dst, rt->rt_state & RTS_DONTROUTE);
	}
	if (doinghost) {
		base = nethash;
		doinghost = 0;
		goto again;
	}
}

/*ARGSUSED*/
sendmsg(dst, dontroute)
	struct sockaddr *dst;
	int dontroute;
{
	(*afswitch[dst->sa_family].af_output)(s, dst, sizeof (struct rip));
}

supply(dst, dontroute)
	struct sockaddr *dst;
	int dontroute;
{
	register struct rt_entry *rt;
	struct netinfo *n = msg->rip_nets;
	register struct rthash *rh;
	struct rthash *base = hosthash;
	int doinghost = 1, size;
	int (*output)() = afswitch[dst->sa_family].af_output;
	int sto = dontroute ? snoroute : s;

again:
	for (rh = base; rh < &base[ROUTEHASHSIZ]; rh++)
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
		if (rt->rt_state & RTS_HIDDEN)
			continue;
		size = (char *)n - packet;
		if (size > MAXPACKETSIZE - sizeof (struct netinfo)) {
			(*output)(sto, dst, size);
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
		(*output)(sto, dst, (char *)n - packet);
}

/*
 * Handle an incoming routing packet.
 */
rip_input(from, size)
	struct sockaddr *from;
	int size;
{
	struct rt_entry *rt;
	struct netinfo *n;
	struct ifnet *ifp;
	time_t t;
	int newsize;
	struct afswitch *afp;

	if (trace) {
		if (msg->rip_cmd < RIPCMD_MAX)
			printf("%s from %x\n", ripcmds[msg->rip_cmd], 
			    ((struct sockaddr_in *)from)->sin_addr);
		else
			printf("%x from %x\n", msg->rip_cmd,
			    ((struct sockaddr_in *)from)->sin_addr);
	}
	if (from->sa_family >= AF_MAX)
		return;
	afp = &afswitch[from->sa_family];
	switch (msg->rip_cmd) {

	case RIPCMD_REQUEST:
		newsize = 0;
		size -= 4 * sizeof (char);
		n = msg->rip_nets;
		while (size > 0) {
			if (size < sizeof (struct netinfo))
				break;
			size -= sizeof (struct netinfo);

			/* 
			 * A single entry with sa_family == AF_UNSPEC and
			 * metric ``infinity'' means ``all routes''.
			 */
			if (n->rip_dst.sa_family == AF_UNSPEC &&
			    n->rip_metric == HOPCNT_INFINITY && size == 0) {
				supply(from, 0);	/* don't route */
				return;
			}
			rt = rtlookup(&n->rip_dst);
			n->rip_metric = rt == 0 ? HOPCNT_INFINITY :
				min(rt->rt_metric+1, HOPCNT_INFINITY);
			n++, newsize += sizeof (struct netinfo);
		}
		if (newsize > 0) {
			msg->rip_cmd = RIPCMD_RESPONSE;
			newsize += sizeof (int);
			(*afp->af_output)(s, from, newsize);
		}
		return;

	case RIPCMD_TRACEON:
		if ((*afp->af_portcheck)(from) == 0)
			return;
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
		return;

	case RIPCMD_TRACEOFF:
		/* verify message came from a priviledged port */
		if ((*afp->af_portcheck)(from) == 0)
			return;
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

	case RIPCMD_RESPONSE:
		/* verify message came from a router */
		if ((*afp->af_portmatch)(from) == 0)
			return;
		(*afp->af_canon)(from);
		/* are we talking to ourselves? */
		ifp = if_ifwithaddr(from);
		if (ifp) {
			rt = rtfind(from, 0);
			if (rt == 0) {
				addrouteforif(ifp);
				return;
			}
			/* interface previously thought down? */
			if (rt->rt_metric > 0) {
				struct rt_entry *downrt = rtfind(from, 1);
				if (downrt) {
					/* unhide old route and delete other */
					downrt->rt_state &= ~RTS_HIDDEN;
					downrt->rt_state |= RTS_ADDRT;
					rt->rt_state |= RTS_DELRT;
					log("unhide", downrt);
				}
			}
			rt->rt_timer = 0;
			return;
		}
		size -= 4 * sizeof (char);
		n = msg->rip_nets;
		for (; size > 0; size -= sizeof (struct netinfo), n++) {
			if (size < sizeof (struct netinfo))
				break;
			if (n->rip_metric >= HOPCNT_INFINITY)
				continue;
			tprintf("dst %x hc %d...",
			    ((struct sockaddr_in *)&n->rip_dst)->sin_addr,
			    n->rip_metric);
			rt = rtlookup(&n->rip_dst);
			if (rt == 0) {
				rtadd(&n->rip_dst, from, n->rip_metric, 0);
				continue;
			}
			tprintf("ours: gate %x hc %d timer %d\n",
			  ((struct sockaddr_in *)&rt->rt_router)->sin_addr,
			  rt->rt_metric, rt->rt_timer);

			/*
			 * update if from gateway, shorter, or getting
			 * stale and equivalent.
			 */
			if (equal(from, &rt->rt_router) ||
			    n->rip_metric < rt->rt_metric ||
			    (rt->rt_timer > (EXPIRE_TIME/2) &&
			    rt->rt_metric == n->rip_metric)) {
				rtchange(rt, from, n->rip_metric);
				rt->rt_timer = 0;
			}
		}
		return;
	}
	printf("bad packet, cmd=%x\n", msg->rip_cmd);
}

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
		if (rt->rt_hash != hash || (rt->rt_state & RTS_HIDDEN))
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

struct rt_entry *
rtfind(dst, lookfordownif)
	struct sockaddr *dst;
	int lookfordownif;
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
		if (lookfordownif ^ (rt->rt_state & RTS_HIDDEN))
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

rtadd(dst, gate, metric, state)
	struct sockaddr *dst, *gate;
	int metric, state;
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
	rt->rt_state = state;
	rt->rt_ifp = if_ifwithnet(&rt->rt_router);
	if (metric)
		rt->rt_flags |= RTF_GATEWAY;
	insque(rt, rh);
	log("add", rt);
	if (install)
		rt->rt_state |= RTS_ADDRT;
}

rtchange(rt, gate, metric)
	struct rt_entry *rt;
	struct sockaddr *gate;
	short metric;
{
	int doioctl = 0, metricchanged = 0;

	if (!equal(&rt->rt_router, gate)) {
		if (rt->rt_state & RTS_DONTDELETE) {
			rtadd(&rt->rt_dst, gate, metric,
			   rt->rt_state &~ (RTS_DONTDELETE|RTS_DONTROUTE));
			rt->rt_state |= RTS_DELRT;
			return;
		}
		doioctl++;
	}
	if (metric != rt->rt_metric) {
		metricchanged++;
		rt->rt_metric = metric;
	}
	if (doioctl) {
		if ((rt->rt_state & RTS_CHGRT) == 0)
			rt->rt_oldrouter = rt->rt_router;
		rt->rt_router = *gate;
		rt->rt_state |= RTS_CHGRT;
	}
	if (doioctl || metricchanged)
		log("change", rt);
}

rtdelete(rt)
	struct rt_entry *rt;
{

	log("delete", rt);
	if (install && ioctl(s, SIOCDELRT, (char *)&rt->rt_rt))
		perror("SIOCDELRT");
	if (rt->rt_state & RTS_DONTDELETE) {
		rt->rt_state |= RTS_HIDDEN;
		return;
	}
	remque(rt);
	free((char *)rt);
}

log(operation, rt)
	char *operation;
	struct rt_entry *rt;
{
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
		{ RTS_ADDRT,	"ADD" },
		{ RTS_DELRT,	"DELETE" },
		{ RTS_CHGRT,	"CHANGE" },
		{ RTS_PASSIVE,	"PASSIVE" },
		{ RTS_DONTDELETE,"DONTDELETE" },
		{ RTS_DONTROUTE,"DONTROUTE" },
		{ RTS_HIDDEN,	"HIDDEN" },
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
	printf("dst %x, router %x", dst->sin_addr, gate->sin_addr);
	if (rt->rt_state & RTS_CHGRT)
		printf(" (old %x)",
			((struct sockaddr_in *)&rt->rt_oldrouter)->sin_addr);
	printf(", metric %d, flags", rt->rt_metric);
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
