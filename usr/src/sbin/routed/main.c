#ifndef lint
static char sccsid[] = "@(#)main.c	4.3 %G%";
#endif

/*
 * Routing Table Management Daemon
 */
#include "defs.h"
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
	u_char retry;
#ifdef COMPAT
	int snoroute;
#endif
	
	argv0 = argv;
	sp = getservbyname("router", "udp");
	if (sp == NULL) {
		fprintf(stderr, "routed: router/udp: unknown service\n");
		exit(1);
	}
	addr.sin_family = AF_INET;
	addr.sin_port = sp->s_port;
	s = getsocket(AF_INET, SOCK_DGRAM, &addr);
	if (s < 0)
		exit(1);
#ifdef COMPAT
	bzero(&addr, sizeof (addr));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(ntohs(sp->s_port) + 1);
	snoroute = getsocket(AF_INET, SOCK_DGRAM, &addr);
	if (snoroute < 0)
		exit(1);
#endif
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
			continue;
		}
		fprintf(stderr, "usage: routed [ -s ] [ -q ] [ -t ]\n");
		exit(1);
	}
#ifndef DEBUG
	if (!tracepackets) {
		int t;

		if (fork())
			exit(0);
		for (cc = 0; cc < 10; cc++)
			(void) close(cc);
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

	for (;;) {
		int ibits;
		register int n;

		ibits = 1 << s;
#ifdef COMPAT
		ibits |= 1 << snoroute;
#endif
		n = select(20, &ibits, 0, 0, 0);
		if (n < 0)
			continue;
		if (ibits & (1 << s))
			process(s);
#ifdef COMPAT
		if (ibits & (1 << snoroute))
			process(snoroute);
#endif
		/* handle ICMP redirects */
	}
}

process(fd)
	int fd;
{
	struct sockaddr from;
	int fromlen = sizeof (from), cc;

	cc = recvfrom(fd, packet, sizeof (packet), 0, &from, &fromlen);
	if (cc <= 0) {
		if (cc < 0 && errno != EINTR)
			perror("recvfrom");
		return;
	}
	if (fromlen != sizeof (struct sockaddr_in))
		return;
	sighold(SIGALRM);
	rip_input(&from, cc);
	sigrelse(SIGALRM);
}

getsocket(domain, type, sin)
	int domain, type;
	struct sockaddr_in *sin;
{
	int retry, s;

	retry = 1;
	while ((s = socket(domain, type, 0, 0)) < 0 && retry) {
		perror("socket");
		sleep(5 * retry);
		retry <<= 1;
	}
	if (retry == 0)
		return (-1);
	while (bind(s, sin, sizeof (*sin), 0) < 0 && retry) {
		perror("bind");
		sleep(5 * retry);
		retry <<= 1;
	}
	if (retry == 0)
		return (-1);
	return (s);
}
