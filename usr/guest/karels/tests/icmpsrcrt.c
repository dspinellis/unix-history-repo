#ifndef lint
static char sccsid[] = "@(#)ping.c	4.4 (Berkeley) 10/21/85";
#endif

/*
 *			P I N G . C
 *
 * Using the InterNet Control Message Protocol (ICMP) "ECHO" facility,
 * measure round-trip-delays and packet loss across network paths.
 *
 * Author -
 *	Mike Muuss
 *	U. S. Army Ballistic Research Laboratory
 *	December, 1983
 * Modified at Uc Berkeley
 *
 * Status -
 *	Public Domain.  Distribution Unlimited.
 *
 * Bugs -
 *	More statistics could always be gathered.
 *	This program has to run SUID to ROOT to access the ICMP socket.
 */

#include <stdio.h>
#include <errno.h>
#include <sys/time.h>

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/file.h>

#include <netinet/in_systm.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <netdb.h>

#define	MAXWAIT		10	/* max time to wait for response, sec. */
#define	MAXPACKET	4096	/* max packet size */
#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN	64
#endif

int	verbose;
u_char	packet[MAXPACKET];
int	options;
extern	int errno;

int s;			/* Socket file descriptor */
struct hostent *hp;	/* Pointer to host info */
struct timezone tz;	/* leftover */

struct sockaddr whereto;/* Who to ping */
int datalen = 64-8;	/* How much data */

char usage[] = "Usage:  ping [-drv] host [data size] [npackets]\n";

char *hostname;
char hnamebuf[MAXHOSTNAMELEN];

int npackets;
int ntransmitted = 0;		/* sequence # for outbound packets = #sent */
int ident;

int nreceived = 0;		/* # of packets we got back */
int timing = 0;
int tmin = 999999999;
int tmax = 0;
int tsum = 0;			/* sum of all times, for doing average */
int finish(), catcher();
#define MAXHOPS	10
char srcrt[4 + MAXHOPS * sizeof(struct in_addr)];
int nhops;

/*
 * 			M A I N
 */
main(argc, argv)
char *argv[];
{
	struct sockaddr_in from;
	char **av = argv;
	struct sockaddr_in *to = (struct sockaddr_in *) &whereto;
	int on = 1;
	struct protoent *proto;
	char *cp = srcrt;
	int noopt = 0, loose = 0;
	register i, n;
	struct in_addr in;

	argc--, av++;
	while (argc > 0 && *av[0] == '-') {
		while (*++av[0]) switch (*av[0]) {
			case 'd':
				options |= SO_DEBUG;
				break;
			case 'r':
				options |= SO_DONTROUTE;
				break;
			case 'v':
				verbose++;
				break;
			case 'l':
				loose++;
				break;
			case 'n':
				noopt++;
				break;
		}
		argc--, av++;
	}
	if( argc < 1)  {
		printf(usage);
		exit(1);
	}

/*
	bzero( (char *)&whereto, sizeof(struct sockaddr) );
	to->sin_family = AF_INET;
	to->sin_addr.s_addr = inet_addr(av[0]);
	if (to->sin_addr.s_addr != -1) {
		strcpy(hnamebuf, av[0]);
		hostname = hnamebuf;
	} else {
		hp = gethostbyname(av[0]);
		if (hp) {
			to->sin_family = hp->h_addrtype;
			bcopy(hp->h_addr, (caddr_t)&to->sin_addr, hp->h_length);
			hostname = hp->h_name;
		} else {
			printf("%s: unknown host %s\n", argv[0], av[0]);
			exit(1);
		}
	}
*/

	*cp++ = IPOPT_NOP;
	if (loose)
		*cp++ = IPOPT_LSRR;
	else
		*cp++ = IPOPT_SSRR;
	*cp++ = 3;
	*cp++ = IPOPT_MINOFF;
	while (argc) {
		if (nhops == MAXHOPS) {
			fprintf(stderr, "too many hops, limit %d\n", MAXHOPS);
			exit(1);
		}
		if ((in.s_addr = inet_addr(*av)) != -1) {
			bcopy((char *)&in, cp, sizeof(struct in_addr));
			strcpy(hnamebuf, av[0]);
			hostname = hnamebuf;
		} else {
			hp = gethostbyname(*av);
			if (hp == 0) {
				fprintf(stderr, "%s: unknown host\n", *av);
				exit(1);
			}
			bcopy(hp->h_addr, cp, sizeof(struct in_addr));
			hostname = hp->h_name;
		}
		cp += sizeof(struct in_addr);
		nhops++;
		argc--; av++;
	}

	ident = getpid() & 0xFFFF;
	if (datalen >= sizeof(struct timeval))
		timing = 1;

	if ((proto = getprotobyname("icmp")) == NULL) {
		fprintf(stderr, "icmp: unknown protocol\n");
		exit(10);
	}
	if ((s = socket(AF_INET, SOCK_RAW, proto->p_proto)) < 0) {
		perror("ping: socket");
		exit(5);
	}
	if (options & SO_DEBUG)
		setsockopt(s, SOL_SOCKET, SO_DEBUG, &on, sizeof(on));
	if (options & SO_DONTROUTE)
		setsockopt(s, SOL_SOCKET, SO_DONTROUTE, &on, sizeof(on));
	if ((proto = getprotobyname("ip")) == 0) {
		fprintf(stderr, "ip: unknown protocol\n");
		exit(10);
	}
	srcrt[IPOPT_OLEN + 1] = cp - srcrt - 1;		/* -1 is NOP */
/*	*cp++ = IPOPT_EOL; */
	if (!noopt) {
		char buf[100];
		int n;

		if (options & SO_DEBUG) {
			n = 4 + nhops * sizeof(struct in_addr);
			printf("options %d long\n", n);
			for (i = 0; i < (n+3)/4; i++)
				printf("%x ", ((int *)srcrt)[i]);
			printf("\n");
		}
		if (setsockopt(s, proto->p_proto, IP_OPTIONS, srcrt,
		    4 + nhops * sizeof(struct in_addr)) < 0) {
			perror("setsockopt");
			exit(10);
		}
		if (options & SO_DEBUG) {
			n = sizeof(buf);
			if (getsockopt(s, proto->p_proto, IP_OPTIONS, buf,
			    &n) < 0)
				perror("getsockopt");
			else {
				printf("options now %d long\n", n);
				for (i = 0; i < (n+3)/4; i++)
					printf("%x ", ((int *)buf)[i]);
				printf("\n");
			}
		}
	}
	bcopy(cp - sizeof(struct in_addr), (caddr_t)&to->sin_addr,
	    sizeof(struct in_addr));

	printf("PING %s: %d data bytes\n", hostname, datalen );

	setlinebuf( stdout );

	signal( SIGINT, finish );
	signal(SIGALRM, catcher);

	catcher();	/* start things going */

	for (;;) {
		int len = sizeof (packet);
		int fromlen = sizeof (from);
		int cc;

		if ( (cc=recvfrom(s, packet, len, 0, &from, &fromlen)) < 0) {
			if( errno == EINTR )
				continue;
			perror("ping: recvfrom");
			continue;
		}
		pr_pack( packet, cc, &from );
		if (npackets && nreceived >= npackets)
			finish();
	}
	/*NOTREACHED*/
}

/*
 * 			C A T C H E R
 * 
 * This routine causes another PING to be transmitted, and then
 * schedules another SIGALRM for 1 second from now.
 * 
 * Bug -
 * 	Our sense of time will slowly skew (ie, packets will not be launched
 * 	exactly at 1-second intervals).  This does not affect the quality
 *	of the delay and loss statistics.
 */
catcher()
{
	int waittime;

	pinger();
	if (npackets == 0 || ntransmitted < npackets)
		alarm(1);
	else {
		if (nreceived) {
			waittime = 2 * tmax / 1000;
			if (waittime == 0)
				waittime = 1;
		} else
			waittime = MAXWAIT;
		signal(SIGALRM, finish);
		alarm(waittime);
	}
}

/*
 * 			P I N G E R
 * 
 * Compose and transmit an ICMP ECHO REQUEST packet.  The IP packet
 * will be added on by the kernel.  The ID field is our UNIX process ID,
 * and the sequence number is an ascending integer.  The first 8 bytes
 * of the data portion are used to hold a UNIX "timeval" struct in VAX
 * byte-order, to compute the round-trip time.
 */
pinger()
{
	static u_char outpack[MAXPACKET];
	register struct icmp *icp = (struct icmp *) outpack;
	int i, cc;
	register struct timeval *tp = (struct timeval *) &outpack[8];
	register u_char *datap = &outpack[8+sizeof(struct timeval)];

	icp->icmp_type = ICMP_ECHO;
	icp->icmp_code = 0;
	icp->icmp_cksum = 0;
	icp->icmp_seq = ntransmitted++;
	icp->icmp_id = ident;		/* ID */

	cc = datalen+8;			/* skips ICMP portion */

	if (timing)
		gettimeofday( tp, &tz );

	for( i=8; i<datalen; i++)	/* skip 8 for time */
		*datap++ = i;

	/* Compute ICMP checksum here */
	icp->icmp_cksum = in_cksum( icp, cc );

	i = sendto( s, outpack, cc, 0, &whereto, sizeof(struct sockaddr) );

	if( i < 0 || i != cc )  {
		if( i<0 )  perror("sendto");
		printf("ping: wrote %s %d chars, ret=%d\n",
			hostname, cc, i );
		fflush(stdout);
	}
}

/*
 * 			P R _ T Y P E
 *
 * Convert an ICMP "type" field to a printable string.
 */
char *
pr_type( t )
register int t;
{
	static char *ttab[] = {
		"Echo Reply",
		"ICMP 1",
		"ICMP 2",
		"Dest Unreachable",
		"Source Quence",
		"Redirect",
		"ICMP 6",
		"ICMP 7",
		"Echo",
		"ICMP 9",
		"ICMP 10",
		"Time Exceeded",
		"Parameter Problem",
		"Timestamp",
		"Timestamp Reply",
		"Info Request",
		"Info Reply"
	};

	if( t < 0 || t > 16 )
		return("OUT-OF-RANGE");

	return(ttab[t]);
}

/*
 *			P R _ P A C K
 *
 * Print out the packet, if it came from us.  This logic is necessary
 * because ALL readers of the ICMP socket get a copy of ALL ICMP packets
 * which arrive ('tis only fair).  This permits multiple copies of this
 * program to be run without having intermingled output (or statistics!).
 */
pr_pack( buf, cc, from )
char *buf;
int cc;
struct sockaddr_in *from;
{
	struct ip *ip;
	register struct icmp *icp;
	register long *lp = (long *) packet;
	register int i;
	struct timeval tv;
	struct timeval *tp;
	int hlen, triptime;
	char *inet_ntoa();

	from->sin_addr.s_addr = ntohl( from->sin_addr.s_addr );
	gettimeofday( &tv, &tz );

	ip = (struct ip *) buf;
	hlen = ip->ip_hl << 2;
	if (cc < hlen + ICMP_MINLEN) {
		if (verbose)
			printf("packet too short (%d bytes) from %s\n", cc,
				inet_ntoa(ntohl(from->sin_addr.s_addr)));
		return;
	}
	cc -= hlen;
	icp = (struct icmp *)(buf + hlen);
	if( icp->icmp_type != ICMP_ECHOREPLY )  {
		if (verbose) {
			printf("%d bytes from %s: ", cc,
				inet_ntoa(ntohl(from->sin_addr.s_addr)));
			printf("icmp_type=%d (%s)\n",
				icp->icmp_type, pr_type(icp->icmp_type) );
			for( i=0; i<12; i++)
			    printf("x%2.2x: x%8.8x\n", i*sizeof(long), *lp++ );
			printf("icmp_code=%d\n", icp->icmp_code );
		}
		return;
	}
	if( icp->icmp_id != ident )
		return;			/* 'Twas not our ECHO */

	tp = (struct timeval *)&icp->icmp_data[0];
	printf("%d bytes from %s: ", cc,
		inet_ntoa(ntohl(from->sin_addr.s_addr)));
	printf("icmp_seq=%d. ", icp->icmp_seq );
	if (timing) {
		tvsub( &tv, tp );
		triptime = tv.tv_sec*1000+(tv.tv_usec/1000);
		printf("time=%d. ms\n", triptime );
		tsum += triptime;
		if( triptime < tmin )
			tmin = triptime;
		if( triptime > tmax )
			tmax = triptime;
	} else
		putchar('\n');
	nreceived++;
}


/*
 *			I N _ C K S U M
 *
 * Checksum routine for Internet Protocol family headers (C Version)
 *
 */
in_cksum(addr, len)
u_short *addr;
int len;
{
	register int nleft = len;
	register u_short *w = addr;
	register u_short answer;
	register int sum = 0;

	/*
	 *  Our algorithm is simple, using a 32 bit accumulator (sum),
	 *  we add sequential 16 bit words to it, and at the end, fold
	 *  back all the carry bits from the top 16 bits into the lower
	 *  16 bits.
	 */
	while( nleft > 1 )  {
		sum += *w++;
		nleft -= 2;
	}

	/* mop up an odd byte, if necessary */
	if( nleft == 1 )
		sum += *(u_char *)w;

	/*
	 * add back carry outs from top 16 bits to low 16 bits
	 */
	sum = (sum >> 16) + (sum & 0xffff);	/* add hi 16 to low 16 */
	sum += (sum >> 16);			/* add carry */
	answer = ~sum;				/* truncate to 16 bits */
	return (answer);
}

/*
 * 			T V S U B
 * 
 * Subtract 2 timeval structs:  out = out - in.
 * 
 * Out is assumed to be >= in.
 */
tvsub( out, in )
register struct timeval *out, *in;
{
	if( (out->tv_usec -= in->tv_usec) < 0 )   {
		out->tv_sec--;
		out->tv_usec += 1000000;
	}
	out->tv_sec -= in->tv_sec;
}

/*
 *			F I N I S H
 *
 * Print out statistics, and give up.
 * Heavily buffered STDIO is used here, so that all the statistics
 * will be written with 1 sys-write call.  This is nice when more
 * than one copy of the program is running on a terminal;  it prevents
 * the statistics output from becomming intermingled.
 */
finish()
{
	printf("\n----%s PING Statistics----\n", hostname );
	printf("%d packets transmitted, ", ntransmitted );
	printf("%d packets received, ", nreceived );
	if (ntransmitted)
	    printf("%d%% packet loss",
		(int) (((ntransmitted-nreceived)*100) / ntransmitted ) );
	printf("\n");
	if (nreceived && timing)
	    printf("round-trip (ms)  min/avg/max = %d/%d/%d\n",
		tmin,
		tsum / nreceived,
		tmax );
	fflush(stdout);
	exit(0);
}
