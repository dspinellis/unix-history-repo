#ifndef lint
static char sccsid[] = "@(#)main.c	4.1 %G%";
#endif

/*
 * Routing Table Management Daemon
 */
#include "router.h"
#include <sys/ioctl.h>
#include <net/if.h>
#include <errno.h>
#include <nlist.h>
#include <signal.h>
#include <time.h>

int	supplier = -1;		/* process should supply updates */

struct	rip *msg = (struct rip *)packet;

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
	if (tracing)
		traceon("/etc/routerlog");

	/*
	 * We use two sockets.  One for which outgoing
	 * packets are routed and for which they're not.
	 * The latter allows us to delete routing table
	 * entries in the kernel for network interfaces
	 * attached to our host which we believe are down
	 * while still polling it to see when/if it comes
	 * back up.  With the new ipc interface we'll be
	 * able to specify ``don't route'' as an option
	 * to send, but until then we utilize a second port.
	 */
	sp = getservbyname("router", "udp");
	if (sp == 0) {
		fprintf(stderr, "routed: udp/router: unknown service\n");
		exit(1);
	}
	routingaddr.sin_family = AF_INET;
	routingaddr.sin_port = htons(sp->s_port);
	noroutingaddr.sin_family = AF_INET;
	noroutingaddr.sin_port = htons(sp->s_port + 1);
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
		fprintf(stderr, "usage: routed [ -s ] [ -q ]\n");
		exit(1);
	}
	/*
	 * Collect an initial view of the world by
	 * snooping in the kernel and the gateway kludge
	 * file.  Then, send a request packet on all
	 * directly connected networks to find out what
	 * everyone else thinks.
	 */
	rtinit();
	gwkludge();
	ifinit();
	if (supplier < 0)
		supplier = 0;
	msg->rip_cmd = RIPCMD_REQUEST;
	msg->rip_nets[0].rip_dst.sa_family = AF_UNSPEC;
	msg->rip_nets[0].rip_metric = HOPCNT_INFINITY;
	toall(sendmsg);
	sigset(SIGALRM, timer);
	timer();

#define	INFINITY	1000000
	for (;;) {
		int ibits;
		register int n;

		ibits = (1 << s) | (1 << snoroute);
		n = select(32, &ibits, 0, INFINITY);
		if (n < 0)
			continue;
		if (ibits & (1 << s))
			process(s);
		if (ibits & (1 << snoroute))
			process(snoroute);
	}
}

process(fd)
	int fd;
{
	register int cc;
	struct sockaddr from;

	cc = receive(fd, &from, packet, sizeof (packet));
	if (cc <= 0) {
		if (cc < 0 && errno != EINTR)
			perror("receive");
		return;
	}
	sighold(SIGALRM);
	rip_input(&from, cc);
	sigrelse(SIGALRM);
}
