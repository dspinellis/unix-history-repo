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
 * Record Route and verbose headers - Phil Dykstra, BRL, March 1988.
 * ttl, duplicate detection - Cliff Frost, UCB, April 1989
 * Pad pattern - Cliff Frost (from Tom Ferrin, UCSF), April 1989
 * Wait for dribbles, option decoding, pkt compare - vjs@sgi.com, May 1989
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
#include <sys/signal.h>

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/file.h>

#include <netinet/in_systm.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <netinet/ip_var.h>
#include <ctype.h>
#include <netdb.h>

#define	MAXWAIT		10	/* max time to wait for response, sec. */
#define	MAXPACKET	(65536-60-8)	/* max packet size */
#define VERBOSE		1	/* verbose flag */
#define QUIET		2	/* quiet flag */
#define FLOOD		4	/* floodping flag */
#define	RROUTE		8	/* record route flag */
#define PING_FILLED     16      /* is buffer filled? */
#define	NUMERIC		32	/* don't do gethostbyaddr() calls */
#define	INTERVAL	64	/* did user specify interval? */
#define	NROUTES		9	/* number of record route slots */
#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN	64
#endif

/* MAX_DUP_CHK is the number of bits in received table, ie the */
/*      maximum number of received sequence numbers we can keep track of. */
/*      Change 128 to 8192 for complete accuracy... */

#define MAX_DUP_CHK     8 * 128
int     mx_dup_ck = MAX_DUP_CHK;
char    rcvd_tbl[ MAX_DUP_CHK / 8 ];
int     nrepeats = 0;

#define A(bit)          rcvd_tbl[ (bit>>3) ]    /* identify byte in array */
#define B(bit)          ( 1 << (bit & 0x07) )   /* identify bit in byte */
#define SET(bit)        A(bit) |= B(bit)
#define CLR(bit)        A(bit) &= (~B(bit))
#define TST(bit)        (A(bit) & B(bit))


char	*malloc();

u_char	*packet;
int	packlen;
int	i, pingflags = 0, options;
extern	int errno;

int s;			/* Socket file descriptor */
struct hostent *hp;	/* Pointer to host info */
struct timezone tz;	/* leftover */

struct sockaddr whereto;	/* Who to ping */
int datalen = 64-8;		/* How much data */

char usage[] =
"Usage:  ping [-dfnqrvR][-c count][-i wait][-l preload][-p pattern][-s packetsize][-h] host \n";

char *hostname;
char hnamebuf[MAXHOSTNAMELEN];

static u_char outpack[MAXPACKET];

int npackets=0;
int preload = 0;		/* number of packets to "preload" */
int ntransmitted = 0;		/* sequence # for outbound packets = #sent */
int ident;
unsigned interval=1;		/* interval between packets */

int nreceived = 0;		/* # of packets we got back */
int timing = 0;
int tmin = 999999999;
int tmax = 0;
int tsum = 0;			/* sum of all times, for doing average */
int finish(), catcher();
int bufspace = 48*1024;
int prefinish();
char *inet_ntoa(),*strcpy(),*strncpy(),*sprintf();
char *pr_addr();
u_long inet_addr();
char rspace[3+4*NROUTES+1];	/* record route space */

/*
 * 			M A I N
 */
main(argc, argv)
char *argv[];
{
	struct sockaddr_in from;
/*	char **av = argv; */
	struct sockaddr_in *to = (struct sockaddr_in *) &whereto;
	int c, k, on = 1, hostind = 0;
	struct protoent *proto;
	static u_char *datap = &outpack[8+sizeof(struct timeval)];
	extern int optind;
	extern char *optarg;

	while ((c = getopt(argc, argv, "c:dfh:i:l:np:qrs:vR")) != EOF)
		switch(c) {
			case 'c':
				npackets = atoi(optarg);
				break;
			case 'd':
				options |= SO_DEBUG;
				break;
			case 'f':
				pingflags |= FLOOD;
				break;
			case 'h':
				hostind = optind-1;
				break;
                        case 'i':       /* wait between sending packets */
				interval = atoi(optarg);
				if (interval == 0) 
					interval = 1;
				pingflags |= INTERVAL;
                                break;
			case 'l':
				preload = atoi(optarg);
				break;
			case 'n':
				pingflags |= NUMERIC;
				break;
                        case 'p':       /* fill buffer with user pattern */
                                pingflags |= PING_FILLED;
				fill((char *)datap, optarg);
                                break;
			case 'q':
				pingflags |= QUIET;
				break;
			case 'r':
				options |= SO_DONTROUTE;
				break;
                        case 's':       /* size of packet to send */
				datalen = atoi(optarg);
                                break;
			case 'v':
				pingflags |= VERBOSE;
				break;
			case 'R':
				pingflags |= RROUTE;
				break;
			default:
				printf(usage);
				exit(1);
		}

	if (hostind == 0) {
		if (optind != argc-1) {
			fprintf(stderr, usage);
			exit(1);
		} else hostind = optind;
	}

	bzero((char *)&whereto, sizeof(struct sockaddr) );
	to->sin_family = AF_INET;
	to->sin_addr.s_addr = inet_addr(argv[hostind]);
	if(to->sin_addr.s_addr != (unsigned)-1) {
		strcpy(hnamebuf, argv[hostind]);
		hostname = hnamebuf;
	} else {
		hp = gethostbyname(argv[hostind]);
		if (hp) {
			to->sin_family = hp->h_addrtype;
			bcopy(hp->h_addr, (caddr_t)&to->sin_addr, hp->h_length);
			strncpy( hnamebuf, hp->h_name, sizeof(hnamebuf)-1 );
			hostname = hnamebuf;
		} else {
			printf("%s: unknown host %s\n", argv[0], argv[hostind]);
			exit(1);
		}
	}

	if ( (pingflags & FLOOD) && (pingflags & INTERVAL) ) {
		fprintf(stderr, "ping: -f and -i incompatible options\n");
		exit(1);
	}

	if (datalen > MAXPACKET) {
		fprintf(stderr, "ping: packet size too large\n");
		exit(1);
	}
	if (datalen >= sizeof(struct timeval))	/* can we time 'em? */
		timing = 1;
	packlen = datalen + 60 + 76;	/* MAXIP + MAXICMP */
	if( (packet = (u_char *)malloc((unsigned)packlen)) == NULL ) {
		fprintf( stderr, "ping: malloc failed\n" );
		exit(1);
	}

	if (!(pingflags & PING_FILLED)) {
                for( k=8; k<datalen; k++) *datap++ = k;
        }

	ident = getpid() & 0xFFFF;

	if ((proto = getprotobyname("icmp")) == NULL) {
		fprintf(stderr, "icmp: unknown protocol\n");
		exit(10);
	}
	if ((s = socket(AF_INET, SOCK_RAW, proto->p_proto)) < 0) {
		perror("ping: socket");
		exit(5);
	}
	if (options & SO_DEBUG) {
		(void)setsockopt(s, SOL_SOCKET, SO_DEBUG, &on, sizeof(on));
	}
	if (options & SO_DONTROUTE) {
		(void)setsockopt(s, SOL_SOCKET, SO_DONTROUTE, &on, sizeof(on));
	}
	/* Record Route option */
	if( pingflags & RROUTE ) {
#ifdef IP_OPTIONS
		rspace[IPOPT_OPTVAL] = IPOPT_RR;
		rspace[IPOPT_OLEN] = sizeof(rspace)-1;
		rspace[IPOPT_OFFSET] = IPOPT_MINOFF;
		if( setsockopt(s, IPPROTO_IP, IP_OPTIONS, rspace, sizeof(rspace)) < 0 ) {
			perror( "Record route" );
			exit( 42 );
		}
#else
		fprintf( stderr, "ping: record route not available on this machine.\n" );
		exit( 42 );
#endif IP_OPTIONS
	}

	if(to->sin_family == AF_INET) {
		printf("PING %s (%s): %d data bytes\n", hostname,
		  inet_ntoa(*(struct in_addr *)&to->sin_addr.s_addr), datalen);
	} else {
		printf("PING %s: %d data bytes\n", hostname, datalen );
	}
	/* When pinging the broadcast address, you can get a lot
	 * of answers.  Doing something so evil is useful if you
	 * are trying to stress the ethernet, or just want to
	 * fill the arp cache to get some stuff for /etc/ethers.
	 */
	(void)setsockopt(s, SOL_SOCKET, SO_RCVBUF, (char*)&bufspace,
			 sizeof(bufspace));

	signal( SIGINT, prefinish );
	signal(SIGALRM, catcher);

	/* fire off them quickies */
	for(i=0; i < preload; i++)
		pinger();

	if(!(pingflags & FLOOD))
		catcher();	/* start things going */

	for (;;) {
		int fromlen = sizeof (from);
		int cc;
		struct timeval timeout;
		int fdmask = 1 << s;

		timeout.tv_sec = 0;
		timeout.tv_usec = 10000;

		if(pingflags & FLOOD) {
			pinger();
			if( select(32, (fd_set *)&fdmask, (fd_set *)0, (fd_set *)0, &timeout) == 0)
				continue;
		}
		if ( (cc=recvfrom(s, (char *)packet, packlen, 0, (struct sockaddr *)&from, &fromlen)) < 0) {
			if( errno == EINTR )
				continue;
			perror("ping: recvfrom");
			continue;
		}
		pr_pack( (char *)packet, cc, &from );
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
	signal(SIGALRM, catcher);
	if (npackets == 0 || ntransmitted < npackets)
		alarm(interval);
	else {
		if (nreceived) {
			waittime = 2 * tmax / 1000;
			if (waittime == 0)
				waittime = 1;
		} else
			waittime = MAXWAIT;
		signal(SIGALRM, finish);
		alarm((unsigned)waittime);
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
	register struct icmp *icp = (struct icmp *) outpack;
	int i, cc;
	register struct timeval *tp = (struct timeval *) &outpack[8];

	icp->icmp_type = ICMP_ECHO;
	icp->icmp_code = 0;
	icp->icmp_cksum = 0;
	icp->icmp_seq = ntransmitted++;
	icp->icmp_id = ident;		/* ID */

        CLR( icp->icmp_seq % mx_dup_ck );

	cc = datalen+8;			/* skips ICMP portion */

	if (timing)
		gettimeofday( tp, &tz );

	/* Compute ICMP checksum here */
	icp->icmp_cksum = in_cksum( (u_short *)icp, cc );

	/* cc = sendto(s, msg, len, flags, to, tolen) */
	i = sendto( s, (char *)outpack, cc, 0, &whereto, sizeof(struct sockaddr) );

	if( i < 0 || i != cc )  {
		if( i<0 )  perror("sendto");
		printf("ping: wrote %s %d chars, ret=%d\n",
			hostname, cc, i );
		fflush(stdout);
	}
	if( pingflags & FLOOD ) {
		putchar('.');
		fflush(stdout);
	}
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
	register int i, j;
	register u_char *cp,*dp;
	static int old_rrlen;
	static char old_rr[MAX_IPOPTLEN];
	struct timeval tv;
	struct timeval *tp;
	int hlen, triptime, dupflag;

	gettimeofday( &tv, &tz );

	/* Check the IP header */
	ip = (struct ip *) buf;
	hlen = ip->ip_hl << 2;
	if( cc < hlen + ICMP_MINLEN ) {
		if( pingflags & VERBOSE )
			printf("packet too short (%d bytes) from %s\n", cc,
				inet_ntoa(*(struct in_addr *)&from->sin_addr.s_addr));
			fflush(stdout);
		return;
	}

	/* Now the ICMP part */
	cc -= hlen;
	icp = (struct icmp *)(buf + hlen);
	if( icp->icmp_type == ICMP_ECHOREPLY ) {
		if( icp->icmp_id != ident )
			return;			/* 'Twas not our ECHO */

		nreceived++;
		if (timing) {
#ifndef icmp_data
			tp = (struct timeval *)&icp->icmp_ip;
#else
			tp = (struct timeval *)&icp->icmp_data[0];
#endif
			tvsub( &tv, tp );
			triptime = tv.tv_sec*1000+(tv.tv_usec/1000);
			tsum += triptime;
			if( triptime < tmin )
				tmin = triptime;
			if( triptime > tmax )
				tmax = triptime;
		}

                if ( TST(icp->icmp_seq%mx_dup_ck) ) {
                       	nrepeats++, nreceived--;
			dupflag=1;
                } else {
			SET(icp->icmp_seq%mx_dup_ck);
			dupflag=0;
		}

		if( pingflags & QUIET )
			return;

		if( pingflags & FLOOD ) {
			putchar('\b');
			fflush(stdout);
		} else {
			printf("%d bytes from %s: icmp_seq=%d", cc,
			  inet_ntoa(*(struct in_addr *)&from->sin_addr.s_addr),
			  icp->icmp_seq );
			if ( dupflag ) printf(" DUP!");
			printf(" ttl=%d", ip->ip_ttl);
			if (timing)
				printf(" time=%d ms", triptime );
			/* check the data */
			cp = (u_char*)&icp->icmp_data[8];
			dp = &outpack[8+sizeof(struct timeval)];
			for (i=8; i<datalen; i++, cp++, dp++) {
				if (*cp != *dp) {
		    printf("\nwrong data byte #%d should be 0x%x but was 0x%x",
						i, *dp, *cp);
					cp = (u_char*)&icp->icmp_data[0];
					for (i=8; i<datalen; i++, cp++) {
						if ((i%32) == 8)
							printf("\n\t");
						printf("%x ", *cp);
					}
					break;
				}
			}

		}
	} else {
		/* We've got something other than an ECHOREPLY */
		if( !(pingflags & VERBOSE) )
			return;

		printf("%d bytes from %s: ",
		  cc, pr_addr(from->sin_addr.s_addr) );
		pr_icmph( icp );
	}

	/* Display any IP options */
	cp = (u_char *)buf + sizeof(struct ip);
	while (hlen > sizeof(struct ip) & (hlen >= 0)) { /* !ANSI C will  */
		register unsigned long l;		 /* force hlen to */
		switch (*cp) {				 /* unsigned!     */
		case IPOPT_EOL:
			hlen = 0;
			break;
		case IPOPT_LSRR:
			printf("\nLSRR: ");
			hlen -= 2;
			j = *++cp;
			++cp;
			if (j > IPOPT_MINOFF) for (;;) {
				l = *++cp;
				l = (l<<8) + *++cp;
				l = (l<<8) + *++cp;
				l = (l<<8) + *++cp;
				if (l == 0)
					printf("\t0.0.0.0");
				else
					printf("\t%s", pr_addr(ntohl(l)));
				hlen -= 4;
				j -= 4;
				if (j <= IPOPT_MINOFF)
					break;
				putchar('\n');
			}
			break;
		case IPOPT_RR:
			j = *++cp;	/* get length */
			i = *++cp;	/* and pointer */
			hlen -= 2;
			if (i > j) i = j;
			i -= IPOPT_MINOFF;
			if (i <= 0)
				continue;
			if (i == old_rrlen
			    && cp == (u_char *)buf + sizeof(struct ip) + 2
			    && !bcmp((char *)cp, old_rr, i)
			    && !(pingflags & FLOOD)) {
				printf("\t(same route)");
				i = ((i+3)/4)*4;
				hlen -= i;
				cp += i;
				break;
			}
			old_rrlen = i;
			bcopy((char *)cp, old_rr, i);
			printf("\nRR: ");
			for (;;) {
				l = *++cp;
				l = (l<<8) + *++cp;
				l = (l<<8) + *++cp;
				l = (l<<8) + *++cp;
				if (l == 0)
					printf("\t0.0.0.0");
				else
					printf("\t%s", pr_addr(ntohl(l)));
				hlen -= 4;
				i -= 4;
				if (i <= 0)
					break;
				putchar('\n');
			}
			break;
		case IPOPT_NOP:
			printf("\nNOP");
			break;
		default:
			printf("\nunknown option %x", *cp);
			break;
		}
		hlen--;
		cp++;
	}
	if (!(pingflags & FLOOD))
		putchar('\n');
	fflush(stdout);
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
	register int sum = 0;
	u_short answer = 0;

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
	if( nleft == 1 ) {
		*(u_char *)(&answer) = *(u_char *)w ;
		sum += answer;
	}

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

/* On the first SIGINT, allow any outstanding packets to dribble in */
prefinish()
{
	if (nreceived >= ntransmitted	/* quit now if caught up */
	    || nreceived == 0)		/* or if remote is dead */
		finish();
	signal(SIGINT, finish);		/* do this only the 1st time */
	npackets = ntransmitted+1;	/* let the normal limit work */
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
	putchar('\n');
	fflush(stdout);
	printf("\n----%s PING Statistics----\n", hostname );
	printf("%d packets transmitted, ", ntransmitted );
	printf("%d packets received, ", nreceived );
        if (nrepeats) printf("+%d duplicates, ", nrepeats );
	if (ntransmitted)
		if( nreceived > ntransmitted)
			printf("-- somebody's printing up packets!");
		else
			printf("%d%% packet loss", 
			  (int) (((ntransmitted-nreceived)*100) /
			  ntransmitted));
	printf("\n");
	if (nreceived && timing)
	    printf("round-trip (ms)  min/avg/max = %d/%d/%d\n",
		tmin,
		tsum / (nreceived + nrepeats),
		tmax );
	fflush(stdout);
	exit(0);
}

#if 0
static char *ttab[] = {
	"Echo Reply",		/* ip + seq + udata */
	"Dest Unreachable",	/* net, host, proto, port, frag, sr + IP */
	"Source Quench",	/* IP */
	"Redirect",		/* redirect type, gateway, + IP  */
	"Echo",
	"Time Exceeded",	/* transit, frag reassem + IP */
	"Parameter Problem",	/* pointer + IP */
	"Timestamp",		/* id + seq + three timestamps */
	"Timestamp Reply",	/* " */
	"Info Request",		/* id + sq */
	"Info Reply"		/* " */
};
#endif	/* 0 */

/*
 *  Print a descriptive string about an ICMP header.
 */
pr_icmph( icp )
struct icmp *icp;
{
	switch( icp->icmp_type ) {
	case ICMP_ECHOREPLY:
		printf("Echo Reply\n");
		/* XXX ID + Seq + Data */
		break;
	case ICMP_UNREACH:
		switch( icp->icmp_code ) {
		case ICMP_UNREACH_NET:
			printf("Destination Net Unreachable\n");
			break;
		case ICMP_UNREACH_HOST:
			printf("Destination Host Unreachable\n");
			break;
		case ICMP_UNREACH_PROTOCOL:
			printf("Destination Protocol Unreachable\n");
			break;
		case ICMP_UNREACH_PORT:
			printf("Destination Port Unreachable\n");
			break;
		case ICMP_UNREACH_NEEDFRAG:
			printf("frag needed and DF set\n");
			break;
		case ICMP_UNREACH_SRCFAIL:
			printf("Source Route Failed\n");
			break;
		default:
			printf("Dest Unreachable, Bad Code: %d\n", icp->icmp_code );
			break;
		}
		/* Print returned IP header information */
#ifndef icmp_data
		pr_retip( &icp->icmp_ip );
#else
		pr_retip( (struct ip *)icp->icmp_data );
#endif
		break;
	case ICMP_SOURCEQUENCH:
		printf("Source Quench\n");
#ifndef icmp_data
		pr_retip( &icp->icmp_ip );
#else
		pr_retip( (struct ip *)icp->icmp_data );
#endif
		break;
	case ICMP_REDIRECT:
		switch( icp->icmp_code ) {
		case ICMP_REDIRECT_NET:
			printf("Redirect Network");
			break;
		case ICMP_REDIRECT_HOST:
			printf("Redirect Host");
			break;
		case ICMP_REDIRECT_TOSNET:
			printf("Redirect Type of Service and Network");
			break;
		case ICMP_REDIRECT_TOSHOST:
			printf("Redirect Type of Service and Host");
			break;
		default:
			printf("Redirect, Bad Code: %d", icp->icmp_code );
			break;
		}
		printf(" (New addr: 0x%08x)\n", icp->icmp_hun.ih_gwaddr );
#ifndef icmp_data
		pr_retip( &icp->icmp_ip );
#else
		pr_retip( (struct ip *)icp->icmp_data );
#endif
		break;
	case ICMP_ECHO:
		printf("Echo Request\n");
		/* XXX ID + Seq + Data */
		break;
	case ICMP_TIMXCEED:
		switch( icp->icmp_code ) {
		case ICMP_TIMXCEED_INTRANS:
			printf("Time to live exceeded\n");
			break;
		case ICMP_TIMXCEED_REASS:
			printf("Frag reassembly time exceeded\n");
			break;
		default:
			printf("Time exceeded, Bad Code: %d\n", icp->icmp_code );
			break;
		}
#ifndef icmp_data
		pr_retip( &icp->icmp_ip );
#else
		pr_retip( (struct ip *)icp->icmp_data );
#endif
		break;
	case ICMP_PARAMPROB:
		printf("Parameter problem: pointer = 0x%02x\n",
			icp->icmp_hun.ih_pptr );
#ifndef icmp_data
		pr_retip( &icp->icmp_ip );
#else
		pr_retip( (struct ip *)icp->icmp_data );
#endif
		break;
	case ICMP_TSTAMP:
		printf("Timestamp\n");
		/* XXX ID + Seq + 3 timestamps */
		break;
	case ICMP_TSTAMPREPLY:
		printf("Timestamp Reply\n");
		/* XXX ID + Seq + 3 timestamps */
		break;
	case ICMP_IREQ:
		printf("Information Request\n");
		/* XXX ID + Seq */
		break;
	case ICMP_IREQREPLY:
		printf("Information Reply\n");
		/* XXX ID + Seq */
		break;
#ifdef ICMP_MASKREQ
	case ICMP_MASKREQ:
		printf("Address Mask Request\n");
		break;
#endif
#ifdef ICMP_MASKREPLY
	case ICMP_MASKREPLY:
		printf("Address Mask Reply\n");
		break;
#endif
	default:
		printf("Bad ICMP type: %d\n", icp->icmp_type);
	}
}

/*
 *  Print an IP header with options.
 */
pr_iph( ip )
struct ip *ip;
{
	int	hlen;
	unsigned char *cp;

	hlen = ip->ip_hl << 2;
	cp = (unsigned char *)ip + 20;	/* point to options */

	printf("Vr HL TOS  Len   ID Flg  off TTL Pro  cks      Src      Dst Data\n");
	printf(" %1x  %1x  %02x %04x %04x",
		ip->ip_v, ip->ip_hl, ip->ip_tos, ip->ip_len, ip->ip_id );
	printf("   %1x %04x", ((ip->ip_off)&0xe000)>>13, (ip->ip_off)&0x1fff );
	printf("  %02x  %02x %04x", ip->ip_ttl, ip->ip_p, ip->ip_sum );
	printf(" %s ", inet_ntoa(*(struct in_addr *)&ip->ip_src.s_addr));
	printf(" %s ", inet_ntoa(*(struct in_addr *)&ip->ip_dst.s_addr));
	/* dump and option bytes */
	while( hlen-- > 20 ) {
		printf( "%02x", *cp++ );
	}
	printf("\n");
}

/*
 *  Return an ascii host address
 *  as a dotted quad and optionally with a hostname
 */
char *
pr_addr( l )
unsigned long l;
{
	struct	hostent	*hp;
	static	char	buf[80];

	if( (pingflags & NUMERIC) || (hp = gethostbyaddr((char *)&l, 4, AF_INET)) == NULL )
		sprintf( buf, "%s", inet_ntoa(*(struct in_addr *)&l) );
	else
		sprintf( buf, "%s (%s)", hp->h_name, inet_ntoa(*(struct in_addr *)&l) );

	return( buf );
}

/*
 *  Dump some info on a returned (via ICMP) IP packet.
 */
pr_retip( ip )
struct ip *ip;
{
	int	hlen;
	unsigned char	*cp;

	pr_iph( ip );
	hlen = ip->ip_hl << 2;
	cp = (unsigned char *)ip + hlen;

	if( ip->ip_p == 6 ) {
		printf( "TCP: from port %d, to port %d (decimal)\n",
			(*cp*256+*(cp+1)), (*(cp+2)*256+*(cp+3)) );
	} else if( ip->ip_p == 17 ) {
		printf( "UDP: from port %d, to port %d (decimal)\n",
			(*cp*256+*(cp+1)), (*(cp+2)*256+*(cp+3)) );
	}
}

fill(bp, patp)
char *bp, *patp;
{
        register int ii,jj,kk;
        char *cp;
        int pat[16];

        for (cp=patp; *cp; cp++)
                if (!isxdigit(*cp)) {
                        printf("\"-p %s\" ???: ", patp);
                        printf("patterns must be specified as hex digits\n");
                        exit(1);
                }

        ii = sscanf(patp,
                "%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x",
                &pat[0], &pat[1], &pat[2], &pat[3],
                &pat[4], &pat[5], &pat[6], &pat[7],
                &pat[8], &pat[9], &pat[10], &pat[11],
                &pat[12], &pat[13], &pat[14], &pat[15]);

        if (ii > 0)
                for (kk=0; kk<=MAXPACKET-(8+ii); kk+=ii)
                for (jj=0; jj<ii; jj++)
                        bp[jj+kk] = pat[jj];

        if (!(pingflags & QUIET)) {
                printf("PATTERN: 0x");
                for (jj=0; jj<ii; jj++)
                        printf("%02x", bp[jj]&0xFF);
                printf("\n");
        }

}
