#ifndef lint
static char rcsid[] = "$Header$";
#endif

/*
 * XNS Routing Information Protocol Daemon
 */
#include "defs.h"
#include <sys/ioctl.h>
#include <sys/time.h>

#include <net/if.h>

#include <errno.h>
#include <nlist.h>
#include <signal.h>

int	supplier = -1;		/* process should supply updates */
extern int gateway;

struct	rip *msg = (struct rip *) &packet[sizeof (struct idp)]; 
int	hup(), fkexit();

main(argc, argv)
	int argc;
	char *argv[];
{
	struct sockaddr from;
	u_char retry;
	int selectbits;
	struct interface *ifp;
	extern struct interface *ifnet;
	
	argv0 = argv;
	argv++, argc--;
	while (argc > 0 && **argv == '-') {
		if (strcmp(*argv, "-s") == 0) {
			supplier = 1;
			argv++, argc--;
			continue;
		}
		if (strcmp(*argv, "-q") == 0) {
			supplier = 0;
			argv++, argc--;
			continue;
		}
		if (strcmp(*argv, "-t") == 0) {
			tracepackets++;
			argv++, argc--;
			ftrace = stderr;
			tracing = 1; 
			continue;
		}
		if (strcmp(*argv, "-g") == 0) {
			gateway = 1;
			argv++, argc--;
			continue;
		}
		if (strcmp(*argv, "-l") == 0) {
			gateway = -1;
			argv++, argc--;
			continue;
		}
		fprintf(stderr,
			"usage: xnsrouted [ -s ] [ -q ] [ -t ] [ -g ] [ -l ]\n");
		exit(1);
	}
	
#ifndef DEBUG
	if (!tracepackets) {
		int t;

		if (fork())
			exit(0);
		for (t = 0; t < 20; t++)
			(void) close(t);
		(void) open("/", 0);
		(void) dup2(0, 1);
		(void) dup2(0, 2);
		t = open("/dev/tty", 2);
		if (t >= 0) {
			ioctl(t, TIOCNOTTY, (char *)0);
			(void) close(t);
		}
	}
#endif
	/*
	 * Any extra argument is considered
	 * a tracing log file.
	 */
	if (argc > 0)
		traceon(*argv);
	/*
	 * Collect an initial view of the world by
	 * snooping in the kernel.  Then, send a request packet on all
	 * directly connected networks to find out what
	 * everyone else thinks.
	 */
	rtinit();
	ifinit();
	if (supplier < 0)
		supplier = 0;
	/* request the state of the world */
	msg->rip_cmd = htons(RIPCMD_REQUEST);
	xnnet(msg->rip_nets[0].rip_dst) = htonl(DSTNETS_ALL);
	msg->rip_nets[0].rip_metric =  htons(HOPCNT_INFINITY);
	toall(sendmsg);
	signal(SIGALRM, timer);
	signal(SIGHUP, hup);
	signal(SIGINT, hup);
	signal(SIGEMT, fkexit);
	timer();
	
	/*
	 * Listen for RIF packets on all interfaces
	 */
	selectbits = 0;
	for( ifp = ifnet; ifp!=0; ifp = ifp->int_next) {
		selectbits |= 1 << ifp->int_ripsock[0];
		selectbits |= 1 << ifp->int_ripsock[1];
	}

	for (;;) {
		int ibits;
		register int n;

		ibits = selectbits;
		n = select(20, &ibits, 0, 0, 0);
		if (n < 0) {
			if(errno != EINTR) {
				perror("main:select");
				exit(1);
			}
			continue;
		}
		for( ifp = ifnet; ifp!=0; ifp = ifp->int_next) {
		    /* take RIF packet off interface */
		    for(n = 0; n < 2; n++)
			if(ibits & (1 << ifp->int_ripsock[n]))
				process(ifp->int_ripsock[n]);
		}
	}
}

process(fd)
	int fd;
{
	struct sockaddr from;
	int fromlen = sizeof (from), cc, omask;
	struct idp *idp = (struct idp *)packet;

	cc = recvfrom(fd, packet, sizeof (packet), 0, &from, &fromlen);
	if (cc <= 0) {
		if (cc < 0 && errno != EINTR)
			perror("recvfrom");
		return;
	}
	/* We get the IDP header in front of the RIF packet*/
	if (tracepackets > 1) {
	    fprintf(ftrace,"rcv %d bytes on %s ",
		cc, xns_ntoa(&idp->idp_dst));
	    fprintf(ftrace," from %s\n", xns_ntoa(&idp->idp_src));
	}
	
	if (xnnet(idp->idp_src.xn_net) != xnnet(idp->idp_dst.xn_net)) {
		fprintf(ftrace, "XNSrouted: net of interface (%d) != net on ether (%d)!\n",
			ntohl(xnnet(idp->idp_dst.xn_net)),
			ntohs(xnnet(idp->idp_src.xn_net)));
	}
			
	cc -= sizeof (struct idp);
	if (fromlen != sizeof (struct sockaddr_xn))
		return;
#define	mask(s)	(1<<((s)-1))
	omask = sigblock(mask(SIGALRM));
	rip_input(&from, cc);
	sigsetmask(omask);
}

getsocket(domain, type, proto, sxn)
	int domain, type, proto;
	struct sockaddr_xn *sxn;
{
	int retry, s;

	retry = 1;
	while ((s = socket(domain, type, proto, 0)) < 0 && retry) {
		perror("socket");
		sleep(5 * retry);
		retry <<= 1;
	}
	if (retry == 0)
		return (-1);
	while (bind(s, sxn, sizeof (*sxn), 0) < 0 && retry) {
		perror("bind");
		sleep(5 * retry);
		retry <<= 1;
	}
	if (retry == 0)
		return (-1);
	return (s);
}

/*
 * Fork and exit on EMT-- for profiling.
 */
fkexit()
{
	if (fork() == 0)
		exit(0);
}
