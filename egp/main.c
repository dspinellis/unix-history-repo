/* main.c */

/*   
 *  EGP User Process, ISI 23-Jun-84
 */

#include "include.h"
#include <signal.h>

int	install = INSTALL;	/* install routes from now on */

/* "egpup" is a user process that implements the Exterior Gateway Protocol 
 * under Unix 4.2 BSD according to the spec. in RFC 888 and 904.
 * Usage: egpup [-t[i][e][r][p]] [logfile]
 * where t => (if alone) trace all error messages, route changes and packets,
 *	 i => trace internal errors,
 *	 e => trace external errors due to EGP, and egp state changes,
 *	 r => trace all routing changes,
 *	 p => trace all EGP packets,
 *	 logfile is the output log file.
 * Fatal error messages are always logged.
 *
 * Written by Paul Kirton@ISI, based on EGP code written for C-gateway by Liza
 * Martin and Unix 4.2 BSD "routed" code.
 */
int tracelevel = 0;
int	trace[] = {
	0,
	TR_INT|TR_EXT,
	TR_INT|TR_EXT|TR_RT,
	TR_INT|TR_EXT|TR_RT|TR_PKT
};
char *logfile;
int	sigtrace();

main(argc, argv)
int   argc;
char *argv[];

{
	struct sigvec vec, ovec;
	int	selectbits,
		forever = TRUE,
		error = FALSE,
		count,
		arg_n = 0,
		time;
	char	*strtime,
		*cp;
	struct interface *ifp;
	FILE *fp;

/* check arguments for turning on tracing and a trace file */

	while(--argc > 0 && !error) {
		argv++;
		arg_n++;

		switch (arg_n) {
		case 1:
		    cp = *argv;
		    if( *cp++ != '-') {
			if( argc > 1) error = TRUE;
			else logfile = *argv;
		    }
		    else if( *cp++ != 't') error = TRUE;
		    else if( *cp == '\0')
				tracing = TR_INT | TR_EXT | TR_RT | TR_PKT;
		    else {
			while( *cp != '\0')
			    switch ( *cp++) {
			    case 'i':	tracing |= TR_INT;
					tracelevel = 1;
				    	break;
			    case 'e':	tracing |= TR_EXT;
					tracelevel = 1;
					break;
			    case 'r':	tracing |= TR_RT;
					tracelevel = 2;
					break;
			    case 'p':	tracing |= TR_PKT;
					tracelevel = 3;
					break;
			    default:	error = TRUE;
			    };
		    }
		    break;

		case 2:
		    logfile = *argv;
		    break;

		default:
		    error = TRUE;
		}
	}
	if(error) {
		fprintf(stderr, "Usage: egpup [-t[i][e][r][p]] [logfile]\n");
		exit(1);
	}
#ifndef DEBUG
	if (!tracing || logfile != NULL) {
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
	if( logfile != NULL) {
		freopen( logfile, "a", stdout);
		freopen( logfile, "a", stderr);
		dup2( fileno( stdout), fileno( stderr));
		setlinebuf(stdout);
		printf("File \"%s\" logs trace and error messages for ",
								logfile);
		printf("the EGP user process, \"egpup\".\n");
		printf("Tracing levels turned on: ");
		TRACE_INT("i ");
		TRACE_EXT("e ");
		if( tracing & TR_RT) printf("r ");
		if( tracing & TR_PKT) printf("p ");
		printf("\n\n");
	}
	if(tracing) {
		getod(&time);
		strtime = ctime(&time);
		printf("Start egpup at %s\n", strtime);
	}
	signal(SIGIOT, sigtrace);
	signal(SIGEMT, sigtrace);
		
			/* get socket for sending EGP packets - single socket
 			   so kernel assigns correct source address */

	init_if();	/* initialize tables for internet interfaces except
			/* loopback */

	init_sock();	/* open sockets on each interface */
	s = ifnet->int_egpsock;		/* socket descriptor for ioctl calls
					to install routes */

	fp = fopen( EGPINITFILE, "r");	/* open initialization file */
	if(fp == NULL) {
		printf("main: initialization file %s missing\n", EGPINITFILE);
		quit();
	}

	init_egpngh(fp);	/* read egp neighbor list */
	init_egp();		/* initialize EGP neighbor tables */

	rt_init();		/* initialize route hash tables */
/*	rt_readkernel();	/* Initailize routing tables consistent with
				 * kernel's initial state */
	rt_ifinit();		/* initialize interior routes for direct nets
				 */

	if(install) {
		TRACE_RT("\n***Routes are being installed in kernel\n\n");
	}
	else
		TRACE_RT("\n***Routes are not being installed in kernel\n\n");

	rt_dumbinit(fp);	/* initialize interior routes for non-routing
				 * gateways */
	rt_NRadvise_init(fp);	/*  initialize interior routes to be advised
				 * in NR mesages */
	fclose(fp);


/* setup abort signal */

	vec.sv_handler = egpallcease;
	vec.sv_mask = sigmask(SIGIO) | sigmask(SIGALRM);
	if(error = sigvec(SIGTERM, &vec, &ovec))
		{
		 p_error("main:sigvec");
		 quit();
		};

/* commence periodic EGP and route-age processing */

	vec.sv_handler = timeout;
	vec.sv_mask = sigmask(SIGIO) | sigmask(SIGTERM);
	if(error = sigvec(SIGALRM, &vec, &ovec))
		{
		 p_error("main:sigvec");
		 quit();
		};


	timeout();

/* wait to receive EGP, ICMP or IMP messages */

	selectbits = 0;				/* select descriptor mask */
	for(ifp=ifnet;ifp!=0;ifp=ifp->int_next){
		selectbits |= 1 << ifp->int_egpsock;	/* EGP socket */
		selectbits |= 1 << ifp->int_icmpsock;	/* ICMP socket */
	}

/* IMP sockets not included yet */

	while(forever){
		int ibits;
		register int n;

		ibits = selectbits;
		n = select(20, &ibits, 0, 0, 0);	/* multiplex input */

		if (n < 0) {
			if(errno != EINTR) {
				p_error("main:select");
				quit();
			}

			continue;
		}

		for(ifp=ifnet;ifp!=0;ifp=ifp->int_next){
						/* receive EGP packet */
			if(ibits & (1 << ifp->int_egpsock))
				recvpkt(ifp->int_egpsock, IPPROTO_EGP);

						/* receive ICMP packet
					- not implemeted - need new protocol
					number for listening to ICMP redirects
					see work book 4, p.26 */

			if(ibits & (1 << ifp->int_icmpsock))
				recvpkt(ifp->int_icmpsock, IPPROTO_ICMP);
		}

		/* handle IMP messages - not implemented */

	}
}

sigtrace(sig)
{
	time_t time;

	if (sig == SIGIOT)
		tracelevel = 0;
	else if (tracelevel < 3)
		tracelevel++;

	if (tracing == 0 && trace[tracelevel]) {
		freopen( logfile, "a", stdout);
		freopen( logfile, "a", stderr);
		dup2( fileno( stdout), fileno( stderr));
	}
	tracing = trace[tracelevel];
	getod(&time);
	printf("Change tracing at %s\n", ctime(&time));
	printf("Tracing levels turned on: ");
	TRACE_INT("i ");
	TRACE_EXT("e ");
	if( tracing & TR_RT) printf("r ");
	if( tracing & TR_PKT) printf("p ");
	printf("\n\n");
	if (tracing == 0) {
		freopen( "/dev/null", "a", stdout);
		freopen( "/dev/null", "a", stderr);
	}
}
		
recvpkt(sock, protocol)
	int sock, protocol;
{
	static u_char packet[MAXPACKETSIZE + 10];  	/* packet buffer */
	struct sockaddr_in from;
	int fromlen = sizeof(from), count, omask;
	short hlen;
	count = recvfrom(sock, packet, sizeof(packet), 0, &from, &fromlen);
	if (count <= 0) {
		if (count < 0 && errno != EINTR)
			if(tracing & TR_INT) p_error("recvpkt:recvfrom");
		return;
	}
	if (fromlen != sizeof (struct sockaddr_in)){
		TRACE_INT("recvpkt: fromlen %d invalid\n\n", fromlen);
		return;
	}
	if (count > MAXPACKETSIZE){
		TRACE_INT("recvfrom: packet discarded, length %d > %d",
			count, sizeof(packet));
		TRACE_INT(", from addr %s\n\n", inet_ntoa(from.sin_addr));
		return;
	}

	omask = sigblock(sigmask(SIGALRM) | sigmask(SIGTERM));

/* NOTE: Unix raw socket passes IP packet data length in place of the total 
length, this and id and offset are in vax format not network. (but ICMP 
packet does not include the IP header).
*/

	if( tracing & TR_PKT) tracercv(sock, protocol, packet, count, &from);

	switch (protocol) {
	  case IPPROTO_EGP:
			egpin(packet);
			break;

	  case IPPROTO_ICMP:
			icmpin(packet);
			break;

	  case IMPLINK_IP:
			break;  /* not implemented - need care with different
				address family for imp? */

	}

	sigsetmask(omask);
}



/* process ICMP redirect message */

icmpin(ip, cc)
	register struct ip *ip;
	int cc;
{
	register struct	icmp	*icmppkt;
	int hlen;
	struct	sockaddr_in	gateway,	/* gateway address */
				dest;		/* destination addr */

	hlen = ip->ip_hl << 2;
	if (cc < hlen + ICMP_MINLEN)
		return;
	cc -= hlen;
	icmppkt = (struct icmp *)((char *)ip + hlen);
	if( icmppkt->icmp_type != ICMP_REDIRECT)
		return;
	else if( icmppkt->icmp_code != ICMP_REDIRECT_NET)
		return;
	else {
		bzero( (char *)&gateway, sizeof( gateway));
		gateway.sin_family = AF_INET;
		gateway.sin_addr = icmppkt->icmp_gwaddr;

		bzero( (char *)&dest, sizeof( dest));
		dest.sin_family = AF_INET;
		dest.sin_addr = icmppkt->icmp_ip.ip_dst;
						     /* set local part zero */
		dest.sin_addr = 
			inet_makeaddr( inet_netof( dest.sin_addr.s_addr), 0);

		rt_redirect( &dest, &gateway);
	}

	return;
}


/* timer control for periodic EGP and route-age processing */

timeout()
{
	static	int	next_egpjob = 0,
			next_rttime = 0;
	int	time,
		interval;
	struct itimerval  value, ovalue;

	getod(&time);

	if(next_rttime == 0)			/* initialization */
		next_rttime = time + RT_TIMERRATE;
	
					/* execute routines past time */
	if(time >= next_egpjob){
		egpjob();
		next_egpjob = time + egpsleep;
	}

	if(time >= next_rttime){
		rt_time();
		next_rttime = time + RT_TIMERRATE;
	}
					/* determine next timeout interval 
					and reset timer */

	interval = egpsleep;
	if( RT_TIMERRATE < egpsleep)
		interval = RT_TIMERRATE;

	value.it_interval.tv_sec = 0;		/* no auto timer reload */
	value.it_interval.tv_usec = 0;
	value.it_value.tv_sec = interval;
	value.it_value.tv_usec = 0;
	setitimer(ITIMER_REAL, &value, &ovalue);

	fflush(stdout);		/* periodically flush output */

	return;
}


/* get time of day in seconds */

getod(timept)

	long *timept;

{
    struct timeval tp;
    struct timezone tzp;

    if (gettimeofday (&tp, &tzp))
	p_error("getod: gettimeofday");

    *timept = tp.tv_sec;

    return;
}


/* exit egpup */

quit()
{
	int	time;
	char	*strtime;

	if( rt_default_status == NOTINSTALLED)
		rt_default("ADD");
	if(tracing) {
		getod(&time);
		strtime = ctime(&time);
		printf("Exit egpup at %s\n\n", strtime);
	}
	exit(1);
}


/* Print error message
 *
 * First flush stdout stream to preserve log order as perror writes directly
 * to file
 */
p_error(str)
	char *str;
{
	fflush(stdout);
	perror(str);
}
