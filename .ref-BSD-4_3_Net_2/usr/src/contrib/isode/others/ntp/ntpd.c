#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/ntp/RCS/ntpd.c,v 7.2 91/02/22 09:33:57 mrose Interim $";
#endif	lint

/*
 * ntp daemon - based on the 3.4 version but heavily modified for OSI
 * interworking.
 *
 * $Log:	ntpd.c,v $
 * Revision 7.2  91/02/22  09:33:57  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/12/10  23:15:49  mrose
 * isode/
 * 
 * Revision 7.0  90/12/10  17:21:38  mrose
 * *** empty log message ***
 * 
 * Revision 1.4  90/08/14  10:14:00  jpo
 * new protocol version
 * 
 * Revision 1.3  90/02/13  14:24:11  jpo
 * First beta released version
 * 
 * Revision 1.2  89/12/19  08:33:00  jpo
 * Updated for ISODE 6.0ish
 * 
 * Revision 1.1  89/06/15  20:37:00  jpo
 * Initial revision
 * 
 * 
 */


#include "ntp.h"
#include "patchlevel.h"

char	*conf = NTPINITFILE;
char 	*driftcomp_file = NTPDRIFTCOMP;
char	*osiaddress = "Internet=localhost+10123";
char	*myname = "ntpd";

int	doset = 1;
int	selfds = 0;
int	trusting = 1;
int	keepallpeers = 1;
int	logstats;
#ifdef	DEBUG
int	debug = 0;
#endif
#ifdef	SETTICKADJ
int	tickadj = 0;
int	dotickadj = 0;
#endif
#ifdef	NOSWAP
int	noswap = 0;
#endif

unsigned int	servport;
unsigned long clock_watchdog;

double WayTooBig = WAYTOOBIG;


struct list	peer_list;
struct timeval 	tv;
struct ntpdata	ntpframe;
struct sysdata	sys;

fd_set globmask, globwmask;

/* STATIC data */
static int drift_fd = -1;

static LLog _pgm_log = {
	"ntp.log", NULLCP, NULLCP,
	LLOG_FATAL | LLOG_EXCEPTIONS | LLOG_NOTICE, LLOG_FATAL, -1,
	LLOGCLS | LLOGCRT, NOTOK
};
LLog *pgm_log = &_pgm_log;

static int priority = -10;
static int ticked;


static void timeout();
static void init_ntp();
static void initialize();
static void init_kern_vars();
static void hourly();
static void do_peer ();
static int finish ();

extern void make_new_peer();
extern void transmit();
extern void process_packet();
extern void clock_update();
extern void clear();
extern void clock_filter();
extern void select_clock();
extern void advise ();
extern void adios ();
extern void init_logical_clock();
extern void create_osilisten ();
extern void iso_init ();
#if	defined(DEBUG) && defined(SIGUSR1) && defined(SIGUSR2)
static	int incdebug(), decdebug();
#endif

struct ntp_peer *find_peer ();

main(argc, argv)
int argc;
char *argv[];
{
	int	cc;
	extern char *optarg;
	register int i;

	if (myname = rindex (argv[0], '/'))
		myname++;
	if (myname == NULL || *myname == NULL)
		myname = argv[0];

	isodetailor (myname, 0);
	initialize();		/* call NTP protocol initialization first,
				   then allow others to override default
				   values */

	while ((cc = getopt(argc, argv, "a:c:dD:lstnp:")) != EOF) {
		switch (cc) {
		    case 'a':
			if (strcmp(optarg, "any") == 0)
				WayTooBig = 10e15;
			else
				WayTooBig = atof(optarg);
			break;

		    case 'd':
#ifdef	DEBUG
			debug++;
#else
			adios (NULLCP, "%s: not compiled with DEBUG", myname);
#endif
			break;

		    case 'D':
#ifdef	DEBUG
			debug = atoi(optarg);
#else
			adios (NULLCP, "%s: not compiled with DEBUG", myname);
#endif
			break;

		    case 's':
			doset = 0;
			break;

		    case 't':
#ifdef	SETTICKADJ
			dotickadj++;
#else
			adios (NULLCP, "%s: not compiled to set tickadj",
			       myname);
#endif
			break;

		    case 'n':
#ifdef	NOSWAP
			noswap = 1;
#else
			adios (NULLCP, "%s: not compiled for noswap",
			       myname);
#endif
			break;

		    case 'l':
			logstats = 1;
			break;

		    case 'c':
			conf = optarg;
			break;

		    case 'p':
			servport = htons (atoi(optarg));
			break;
		    default:
			adios (NULLCP, "ntpd: -%c: unknown option", cc);
			break;
		}
	}

	if (servport == 0) {
		struct servent *sp;

		if ((sp = getservbyname ("ntp", "udp")) == NULL)
			servport = htons (NTP_PORT);
		else	servport = sp -> s_port;
	}
		


	peer_list.head = peer_list.tail = NULL;
	peer_list.members = 0;
	srandom(getpid ());

	init_ntp(conf);
	init_kern_vars();
	init_logical_clock();

	envinit ();
	create_listeners ();
	/*
	 * Attempt to open for writing the file for storing the drift comp
	 * register.  File must already exist for snapshots to be taken.
	 */
	if ((i = open(driftcomp_file, O_WRONLY|O_CREAT, 0644)) >= 0) {
		drift_fd = i;
	}
	doit ();
}

doit ()
{
	struct timeval tvt;
	fd_set readfds, writefds;
	int	vecp;
	char *vec[4];
	struct TSAPdisconnect tds;
	struct TSAPdisconnect *td = &tds;
	register int i;
	int	newfd;

	(void) gettimeofday(&tv, (struct timezone *) 0);

	FD_ZERO(&globmask);
	FD_ZERO(&globwmask);
	for (i = 0; i < nintf; i++) {
		if ((addrs[i].flags & INTF_VALID) == 0)
			continue;
		FD_SET(addrs[i].fd, &globmask);
		if (addrs[i].if_flags & IFF_BROADCAST)
			TRACE (2, ("Addr %d: %s fd %d %s broadcast %s",
				   i, addrs[i].name, addrs[i].fd,
				   paddr (&addrs[i].addr),
				   ntoa (&addrs[i].bcast)));
		else
			TRACE (2, ("Addr %d: %s fd %d %s", i,
				   addrs[i].name, addrs[i].fd,
				   paddr (&addrs[i].addr)));
	}

	(void) signal(SIGINT, finish);
	(void) signal(SIGTERM, finish);
#if	defined(DEBUG) && defined(SIGUSR1) && defined(SIGUSR2)
	(void) signal(SIGUSR1, incdebug);
	(void) signal(SIGUSR2, decdebug);
#endif
	/*
	 * Find highest fd in use.  This might save a few microseconds in
	 * the select system call.
	 */
	for (selfds = FD_SETSIZE - 1; selfds; selfds--)
		if (FD_ISSET(selfds, &globmask))
			break;
	TRACE (2, ("Highest fd in use is %d", selfds));
	if (!selfds) abort();
	selfds++;

	/* prime the pump! */
	ticked = 1;
	(void) gettimeofday(&tv, (struct timezone *) 0);

	for (;;) {	/* go into a finite but hopefully very long loop */

		readfds = globmask;
		writefds = globwmask;
		TRACE (7, ("wait nfds %d, fds 0x%x 0x%x", selfds,
			   readfds.fds_bits[0], writefds.fds_bits[0]));

		if (ticked) {
			ticked = 0;
			tvt = tv;
		}

		(void) TNetAcceptAux (&vecp, vec, &newfd, NULLTA,
				      selfds, &readfds,
				      &writefds, NULLFD, (1<<CLOCK_ADJ), td);

		(void) gettimeofday(&tv, (struct timezone *) 0);
		if (tv.tv_sec - tvt.tv_sec >= (1 << CLOCK_ADJ))
			ticked = 1;

		if (vecp > 0)
			iso_init (vecp, vec, newfd);

		for(i = 0; i < nintf; i++) {
			if ((addrs[i].flags & INTF_SELECT) == 0)
				continue;

			if (!FD_ISSET(addrs[i].fd, &readfds) &&
			    !FD_ISSET (addrs[i].fd, &writefds))
				continue;
			TRACE (3, ("Activity on if %d fd=%d (%s)",
				   i, addrs[i].fd, addrs[i].name));
			if (addrs[i].flags & INTF_PENDING) {
				struct ntp_peer *p = find_peer (i);
				if (p)
					(void) make_osi_conn (find_peer(i),
							      osiaddress);
				continue;
			}
			if (addrs[i].flags & INTF_ACCEPTING) {
				iso_accept (&addrs[i]);
				continue;
			}
			addrs[i].uses++;
			switch (addrs[i].addr.type) {
			    case AF_INET:
				if (recv_inet (&addrs[i], &tv) == -1)
					continue;
				break;

			    case AF_OSI:
				if (recv_osi (&addrs[i], &tv) == -1)
					continue;
				break;

			    default:
				TRACE (1, ("Address family %d not supported",
						addrs[i].addr.type));
				continue;
			}
		}
		if (ticked)
			timeout((int) (tv.tv_sec - tvt.tv_sec));
	}
}

struct ntp_peer *
check_peer(dst, sock)
	struct Naddr *dst;
	int sock;
{
	register struct ntp_peer *peer = peer_list.head;

	while (peer != NULL) {
		if (addr_compare (&peer->src, dst) &&
		    ((peer->sock == sock) || (peer->sock == -1)))
			return peer;
		peer = peer->next;
	}
	return ((struct ntp_peer *) NULL);
}

#ifdef	DEBUG
void
dump_pkt(dst, pkt, peer)
	struct Naddr *dst;
	struct ntpdata *pkt;
	struct ntp_peer *peer;
{
	struct Naddr clock_host;

	printf("Packet: %s\n", paddr (dst));

	printf("Leap %d, version %d, mode %d, poll %d, precision %d stratum %d",
	       (pkt->status & LEAPMASK) >> 6, (pkt->status & VERSIONMASK) >> 3,
	       pkt->status & MODEMASK, pkt->ppoll, pkt->precision,
	       pkt->stratum);
	switch (pkt->stratum) {
	case 0:
	case 1:
		printf(" (%.4s)\n", (char *)&pkt->refid);
		break;
	default:
		clock_host.inet_ad.sin_addr.s_addr = (u_long) pkt->refid;
		clock_host.type = AF_INET;
		printf(" [%s]\n", paddr (&clock_host));
		break;
	}
	printf("Synch Dist is %04X.%04X  Synch Dispersion is %04X.%04X\n",
	       ntohs((u_short) pkt->distance.int_part),
	       ntohs((u_short) pkt->distance.fraction),
	       ntohs((u_short) pkt->dispersion.int_part),
	       ntohs((u_short) pkt->dispersion.fraction));
	printf("Reference Timestamp is %08lx.%08lx\n",
	       ntohl(pkt->reftime.int_part),
	       ntohl(pkt->reftime.fraction));
	printf("Originate Timestamp is %08lx.%08lx\n",
	       ntohl(pkt->org.int_part),
	       ntohl(pkt->org.fraction));
	printf("Receive Timestamp is   %08lx.%08lx\n",
	       ntohl(pkt->rec.int_part),
	       ntohl(pkt->rec.fraction));
	printf("Transmit Timestamp is  %08lx.%08lx\n",
	       ntohl(pkt->xmt.int_part),
	       ntohl(pkt->xmt.fraction));
	if(peer != NULL)
		printf("Input Timestamp is     %08lx.%08lx\n",
		       ntohl(peer->rec.int_part),
		       ntohl(peer->rec.fraction));
	putchar('\n');
}
#endif

void
make_new_peer(peer)
	struct ntp_peer *peer;
{
	int i;
	void	double_to_s_fixed ();

	/*
	 * initialize peer data fields 
	 */
	bzero ((char *)peer, sizeof (*peer));
	peer->src.type = peer->src.inet_ad.sin_family = AF_INET;
	peer->hmode = MODE_SYM_PAS;	/* default: symmetric passive mode */
	peer->timer = 1 << NTP_MINPOLL;
	peer->hpoll = NTP_MINPOLL;
	peer->ppoll = NTP_MAXPOLL;
	peer->vers = 2;
	peer->mode = 0;
	double_to_s_fixed(&peer->dispersion, PEER_MAXDISP);
	peer->estoffset = 0.0;
	peer->estdelay = 0.0;
	for (i = 0; i < NTP_WINDOW; i++) {
		peer->filter.offset[i] = 0.0;
		peer->filter.delay[i] = 0.0;
	}
}

/*
 *  This procedure is called to delete a peer from our list of peers.
 */
demobilize(l, peer)
	struct list *l;
	struct ntp_peer *peer;
{
	extern struct ntp_peer dummy_peer;

	if (keepallpeers) {
		peer -> flags |= PEER_FL_SNOOZE;
		return 0;
	}

	if (peer == &dummy_peer)
#ifdef	DEBUG
		abort();
#else
		return 1;
#endif

#ifdef	DEBUG
	if ((peer->next == NULL && peer->prev == NULL) ||
	    l->tail == NULL || l->head == NULL)
		abort();
#endif

	/* delete only peer in list? */
	if (l->head == l->tail) {
#ifdef	DEBUG
		if (l->head != peer) abort();
#endif
		l->head = l->tail = NULL;
		goto dropit;
	}

	/* delete first peer? */
	if (l->head == peer) {
		l->head = peer->next;
		l->head->prev = NULL;
		goto dropit;
	}

	/* delete last peer? */
	if (l->tail == peer) {
		l->tail = peer->prev;
		l->tail->next = NULL;
		goto dropit;
	}

	/* drop peer in middle */
	peer->prev->next = peer->next;
	peer->next->prev = peer->prev;

 dropit:
#ifdef	DEBUG
	/* just some sanity checking */
	if ((l->members < 0) || 
	    (l->members && l->tail == NULL) ||
	    (l->members == 0 && l->tail != NULL)) {
		advise (LLOG_EXCEPTIONS, NULLCP, "List hosed (demobilize)");
		abort();
	}
	peer->next = peer->prev = NULL;
#endif
	free((char *) peer);
	l->members--;

	return 1;
}

enqueue(l, peer)
	register struct list *l;
	struct ntp_peer *peer;
{
	l->members++;
	if (l->tail == NULL) {
		/* insertion into empty list */
		l->tail = l->head = peer;
		peer->next = peer->prev = NULL;
		return;
	}

	/* insert at end of list */
	l->tail->next = peer;
	peer->next = NULL;
	peer->prev = l->tail;
	l->tail = peer;
}


static void timeout(n)
int	n;
{
	static int periodic = 0;
	register struct ntp_peer *peer;
#ifndef	XADJTIME2
	extern void adj_host_clock();

	adj_host_clock(n);
#endif
	TRACE (7, ("timeout (%d)", n));
	/*
	 * Count down sys.hold if necessary.
	 */
	if (sys.hold) {
		if (sys.hold <= n)
			sys.hold = 0;
		else
			sys.hold -= n;
	}
	/*
	 * If interval has expired blast off an NTP to that host.
	 */
	for (peer = peer_list.head; peer != NULL; peer = peer->next) {

#ifdef	DEBUG
		if (peer->next == NULL && peer != peer_list.tail) {
			advise (LLOG_EXCEPTIONS, NULLCP, "Broken peer list");
			abort();
		}
#endif
		if (peer -> flags & PEER_FL_SNOOZE)
			continue;
		if (peer -> mode == PEERMODE_QUERY)
			continue;
		if (peer->reach != 0 || peer->hmode != MODE_SERVER) {
			peer->stopwatch += n;
			if (peer->timer <= peer->stopwatch)
				do_peer (peer);
		}
	}

	periodic += n;
	if (periodic >= 60*60) {
		periodic = 0;
		hourly();
	}

	clock_watchdog += n;
	if (clock_watchdog >= NTP_MAXAGE) {
		/* woof, woof - barking dogs bite! */
		sys.leap = ALARM;
		if (clock_watchdog < NTP_MAXAGE + n) {
			advise (LLOG_EXCEPTIONS, NULLCP, 
			       "logical clock adjust timeout (%d seconds)",
			       NTP_MAXAGE);
		}
	}

#ifdef	DEBUG
	if (debug)
		(void) fflush(stdout);
#endif
}

static void do_peer (peer)
struct ntp_peer *peer;
{
	if (peer -> flags & PEER_FL_SNOOZE)
		return;
	peer->stopwatch = 0;
	if (peer->flags & PEER_FL_CONREQ) {
		switch (peer -> flags & PEER_FL_CONNSTATE) {
		    case 0:
			peer -> timer = 1 << (MAX(MIN(peer->ppoll,
						      MIN(peer->hpoll,
							  NTP_MAXPOLL)),
						  NTP_MINPOLL));
			if (peer->backoff == 0)
				peer -> backoff = BACKOFF_COUNT;
			else {
				if (peer -> backoff == 1)
					poll_update (peer, (int)peer->hpoll +1);
				peer -> backoff --;
			}

			if (make_osi_conn (peer, osiaddress) == NOTOK)
				return;
			break;
		    case PEER_FL_CONNECTED:
			break;
		    case PEER_FL_CONINP1:
		    case PEER_FL_CONINP2:
			return;
		    default:
			advise (LLOG_EXCEPTIONS, NULLCP,
				"Bad state flags %d",
				peer -> flags & PEER_FL_CONNSTATE);
			return;
		}
	}
	transmit (peer);
}



/*
 * init_ntp() reads NTP daemon configuration information from disk file.
 */
int precision;

static void init_ntp(config)
char *config;
{
	struct Naddr addr;
	char buffer[BUFSIZ];
	FILE *fp;
	double j;
	int	argc;
#define MAXARGS	10
	char	*argv[MAXARGS];

	extern double drift_comp;

	bzero((char *) &addr, sizeof(addr));
	fp = fopen(config, "r");
	if (fp == NULL)
		adios (config, "Problem opening NTP initialization file");

	while (fgets (buffer, sizeof buffer, fp)) { /* read line and parse */
		if (buffer[0] == '#' || buffer[0] == '\n')
			continue;

		if ((argc = sstr2arg (buffer, MAXARGS, argv, " \t\n")) <= 0)
			continue;

		if (config_line (argv, argc) != OK)
			TRACE (1, ("Ignoring line %s ...", argv[0]));
	}
	/*
	 *  Read saved drift compensation register value.
	 */
	if ((fp = fopen(driftcomp_file, "r")) != NULL) {
		if (fscanf(fp, "%lf", &j) == 1 && j > -1.0 && j < 1.0) {
			drift_comp = j;
			advise (LLOG_NOTICE, NULLCP,
			       "Drift compensation value initialized to %f", j);
		} else {
			advise (LLOG_EXCEPTIONS, NULLCP,
			       "init_ntp: bad drift compensation value");
		}
		(void) fclose(fp);
	}
}

#include "cmd_srch.h"

CMD_TABLE	config_tbl[] = {
#define	TBL_MAXPEERS	1
	"maxpeers",	TBL_MAXPEERS,
#define	TBL_TRUSTING	2
	"trusting",	TBL_TRUSTING,
#define	TBL_OSILISTEN	3
	"osilisten",	TBL_OSILISTEN,
#define	TBL_LOGCLOCK	4
	"logclock",	TBL_LOGCLOCK,
#define	TBL_DRIFTFILE	5
	"driftfile",	TBL_DRIFTFILE,
#define	TBL_WAYTOOBIG	6
	"waytoobig",	TBL_WAYTOOBIG,
	"setthreshold",	TBL_WAYTOOBIG,
#define	TBL_DEBUGLEVEL	7
	"debuglevel",	TBL_DEBUGLEVEL,
#define	TBL_TICKADJ	8
	"tickadj",	TBL_TICKADJ,
#define	TBL_SETTICKADJ	9
	"settickadj",	TBL_SETTICKADJ,
#define	TBL_NOSWAP	10
	"noswap",	TBL_NOSWAP,
#define	TBL_BROADCAST	11
	"broadcast",	TBL_BROADCAST,
#define	TBL_PEER	12
	"peer",		TBL_PEER,
#define	TBL_PASSIVE	13
	"passive",	TBL_PASSIVE,
#define	TBL_SERVER	14
	"server",	TBL_SERVER,
#define	TBL_REFCLOCK	15
	"refclock",	TBL_REFCLOCK,
#define TBL_STRATUM	16
	"stratum",	TBL_STRATUM,
#define TBL_PRECISION	17
	"precision",	TBL_PRECISION,
#define TBL_LOGFILE	18
	"logfile",	TBL_LOGFILE,
#define TBL_PRIORITY	19
	"priority",	TBL_PRIORITY,
#define TBL_KEEPALLPEERS 20
	"keepallpeers", TBL_KEEPALLPEERS,
	"",	-1
};

config_line (argv, argc)
char	*argv[];
int	argc;
{
	int	result;
	struct Naddr addr;
	int	i;
	struct ntp_peer *peer;

	switch (result = cmd_srch (argv[0], config_tbl)) {
	    case TBL_PRIORITY:
		if (argc > 1)
			priority = atoi (argv[1]);
		else	return NOTOK;
		break;

	    case TBL_MAXPEERS:
		if (argc > 1)
			sys.maxpeers = atoi (argv[1]);
		else	return NOTOK;
		break;

	    case TBL_TRUSTING:
		if (argc < 2)
			return NOTOK;
		trusting = ynorint (argv[1]);
		break;

	    case TBL_OSILISTEN:
		if (argc < 2)
			return NOTOK;
		osiaddress = strdup (argv[1]);
		break;

	    case TBL_LOGCLOCK:
		if (argc < 2)
			return NOTOK;
		logstats = ynorint (argv[1]);
		break;

	    case TBL_LOGFILE:
		if (argc < 2)
			return NOTOK;
		pgm_log -> ll_file = strdup (argv[1]);
		break;

	    case TBL_DRIFTFILE:
		if (argc < 2)
			return NOTOK;
		driftcomp_file = strdup (argv[1]);
		break;

	    case TBL_WAYTOOBIG:
		if (argc < 2)
			return NOTOK;
		if (lexequ (argv[1], "any") == 0)
			WayTooBig = 10e15;
		else	WayTooBig = atof (argv[1]);
		break;

	    case TBL_DEBUGLEVEL:
		if (argc < 2)
			return NOTOK;
#ifdef DEBUG
		debug += atoi (argv[1]);
#endif
		break;

	    case TBL_STRATUM:
		advise (LLOG_NOTICE, NULLCP,
			"Obsolete command 'stratum'");
		break;

	    case TBL_PRECISION:
		if (argc < 2)
			return NOTOK;
		sys.precision = atoi (argv[1]);
		break;
		
	    case TBL_TICKADJ:
		if (argc < 2)
			return NOTOK;
#ifdef	SETTICKADJ
		tickadj = atoi (argv[1]);
#endif
		break;

	    case TBL_SETTICKADJ:
		if (argc < 2)
			return NOTOK;
#ifdef SETTICKADJ
		dotickadj = ynorint (argv[1]);
#endif
		break;

	    case TBL_NOSWAP:
#ifdef NOSWAP
		noswap = 1;
#endif
		break;

	    case TBL_BROADCAST:
		if (argc < 2)
			return NOTOK;
#ifdef BROADCAST_NTP
		for (i = 0; i < nintf; i++)
			if (strcmp(addrs[i].name, argv[1]) == 0)
				break;
		if (i == nintf) {
			advise (LLOG_EXCEPTIONS, NULLCP,
				"config: %s not a known interface", argv[1]);
			return NOTOK;
		}
		if ((addrs[i].if_flags & IFF_BROADCAST) == 0) {
			advise (LLOG_EXCEPTIONS, NULLCP,
				"config: %s doesn't support broadcast",
				argv[1]);
			return NOTOK;
		}
		if (peer = check_peer(&addrs[i].bcast, -1)) {
			advise (LLOG_EXCEPTIONS, NULLCP,
				"config file: duplicate broadcast for %s",
				argv[1]);
			return NOTOK;
		}
		peer = (struct ntp_peer *) malloc(sizeof(struct ntp_peer));
		if (peer == NULL)
			adios ("malloc", "peer");
		make_new_peer(peer);
		peer->flags = PEER_FL_BCAST;
		peer->hmode = MODE_BROADCAST;
		peer->src = addrs[i].bcast;
		peer->sock = i;
#endif				/* BROADCAST_NTP */
		break;

	    case TBL_PEER:
	    case TBL_PASSIVE:
	    case TBL_SERVER:
		if (argc < 2)
			return NOTOK;

		if (GetHostName(argv[1], &addr) == NOTOK) {
			advise (LLOG_EXCEPTIONS, NULLCP,
				"%s: unknown host", argv[1]);
			return NOTOK;
		}
		for (i=0; i<nintf; i++)
			if (addr_compare (&addrs[i].addr, &addr))
				return NOTOK;

		peer = check_peer(&addr, -1);
		if (peer != NULL) {
			advise (LLOG_NOTICE, NULLCP,
				"Duplicate peer %s in in config file",
				paddr (&addr));
			return NOTOK;
		}
		peer = (struct ntp_peer *)
			malloc(sizeof(struct ntp_peer));
		if (peer == NULL)
			adios ("failed", "malloc");

		make_new_peer(peer);
		peer->flags = PEER_FL_CONFIG;
		switch (result) {
		    case TBL_PEER: /* "peer" */
			peer->hmode = MODE_SYM_ACT;
			peer->stopwatch = random () % (1 << NTP_MINPOLL);
			peer->flags |= PEER_FL_SYNC;
			break;
		    case TBL_SERVER: /* "server" */
			peer->hmode = MODE_CLIENT;
			peer->stopwatch = random () % (1 << NTP_MINPOLL);
			peer->flags |= PEER_FL_SYNC;
			break;
		    case TBL_PASSIVE: /* "passive" */
			peer->hmode = MODE_SYM_PAS;
			peer->flags |= PEER_FL_SYNC;
			break;
		    default:
			printf("can't happen\n");
			abort();
		}
		peer->src = addr;
		if (addr.type == AF_OSI)
			peer -> flags |= PEER_FL_CONREQ;
		peer->sock = -1;
		clear(peer);
		other_peer_fields (peer, argv + 2, argc - 2);
		enqueue(&peer_list, peer);
		TRACE (2, ("Peer %s mode %d",
			   paddr(&peer->src),
			   peer->hmode));
		break;

	    case TBL_REFCLOCK:
		if (argc < 6)
			return NOTOK;
#ifdef REFCLOCK
		else {
			int stratum, prec;
			char	*ref_clock;
			char *clk_type;

			ref_clock = argv[2];
			stratum = atoi (argv[3]);
			prec = atoi (argv[4]);
			clk_type = argv[5];

			if((i = init_clock(argv[1], clk_type)) < 0) {
				/* If we could not initialize clock line */
				advise (LLOG_EXCEPTIONS, NULLCP,
					"Could not init reference source %s (type %s)",
					argv[1], clk_type);
				return NOTOK;
			}
			peer = (struct ntp_peer *)
				calloc(1, sizeof(struct ntp_peer));
			if (peer == NULL) {
				(void) close(i);
				return NOTOK;
			}
			make_new_peer(peer);
			(void) strncpy(peer->refid.rid_string,
				       ref_clock, 4);
			peer->refid.rid_string[4] = 0;
			peer->refid.rid_type = RID_STRING;
			peer->flags = PEER_FL_CONFIG|PEER_FL_REFCLOCK;
			peer->hmode = MODE_SYM_ACT;
			peer->stopwatch = random () % (1 << NTP_MINPOLL);
			peer->flags |= PEER_FL_SYNC;
			peer->sock = i;
			peer->stratum = stratum;
			peer->precision = prec;
			clear(peer);
			enqueue(&peer_list, peer);
			TRACE (2, ("Refclock %s mode %d refid %.4s stratum %d precision %d",
				   argv[1], peer->hmode,
				   peer->refid.rid_string,
				   stratum, prec));
			transmit(peer);	/* head start for REFCLOCK */
		}
#endif REFCLOCK
		break;
	    case TBL_KEEPALLPEERS:
		if (argc < 2)
			return NOTOK;
		keepallpeers = ynorint (argv[1]);
		break;

	    default:
		return NOTOK;
	}
	return OK;
}


CMD_TABLE tbl_peer_flags[] = {
#define TBLPEER_VERSION	1
	"version",	TBLPEER_VERSION,
#define TBLPEER_AUTH	2
	"auth",		TBLPEER_AUTH,
	NULLCP,		-1
	};
	
other_peer_fields (peer, argv, argc)
struct ntp_peer *peer;
char	**argv;
int	argc;
{
	while (argc > 0) {
		switch (cmd_srch (argv[0], tbl_peer_flags)) {
		    case TBLPEER_VERSION:
			peer -> vers = atoi(argv[1]);
			if (peer -> vers < 1 || peer -> vers > 2)
				adios (NULLCP, "Bad version %d",
				       peer -> vers);
			break;

		    case TBLPEER_AUTH:
			advise (LLOG_NOTICE, "auth code not done yet");
			break;
		}
		argc -= 2;
		argv += 2;
	}
}

ynorint (s)
char	*s;
{
	if (*s == 'y' || *s == 'Y')
		return 1;
	if (*s == 'n' || *s == 'N')
		return 0;
	return atoi (s);
}

/*  */
int kern_tickadj;
static int kern_hz, kern_tick;
#ifdef NeXT
#define n_name n_un.n_name
#endif

static void
init_kern_vars() {
	int kmem;
	static char	*memory = "/dev/kmem";
	static struct nlist nl[4];
	static char *knames[] = { "_tickadj", "_hz", "_tick" };
	static int *kern_vars[] = {&kern_tickadj, &kern_hz, &kern_tick};
	int i;
	kmem = open(memory, O_RDONLY);
	if (kmem < 0) {
		advise (LLOG_EXCEPTIONS, memory, "Can't open");
		return;
	}

	for (i = 0; i < 3; i++) {
#ifdef SYS5		
		(void) strcpy (nl[i].n_name, knames[i]);
#else
		nl[i].n_name = knames[i];
#endif
	}
#ifdef SYS5
	(void) nlist("/unix", nl);
#else
	(void) nlist("/vmunix", nl);
#endif

	for (i = 0; i < (sizeof(kern_vars)/sizeof(kern_vars[0])); i++) {
		long where;

		if ((where = nl[i].n_value) == 0) {
			advise (LLOG_EXCEPTIONS, NULLCP,
				"Unknown kernal var %s", nl[i].n_name);
			continue;
		}
		if (lseek(kmem, where, L_SET) == -1) {
			advise (LLOG_EXCEPTIONS, "", "lseek for %s fails",
			       nl[i].n_name);
			continue;
		}
		if (read(kmem, (char *)kern_vars[i], sizeof(int)) != sizeof(int)) {
			advise (LLOG_EXCEPTIONS, "", "read for %s fails",
			       nl[i].n_name);

			*kern_vars[i] = 0;
		}
	}
#ifdef	SETTICKADJ
	/*
	 *  If desired value of tickadj is not specified in the configuration
	 *  file, compute a "reasonable" value here, based on the assumption 
	 *  that we don't have to slew more than 2ms every 4 seconds.
	 *
	 *  TODO: the 500 needs to be parameterized.
	 */
	if (tickadj == 0 && kern_hz)
		tickadj = 500/kern_hz;

	TRACE (1, ("kernel vars: tickadj = %d, hz = %d, tick = %d",
		   kern_tickadj, kern_hz, kern_tick));
	TRACE (1, ("desired tickadj = %d, dotickadj = %d", tickadj,
		   dotickadj));

	if (dotickadj && tickadj && (tickadj != kern_tickadj)) {
		(void) close(kmem);
		if ((kmem = open(memory, O_RDWR)) >= 0) {
			if (lseek(kmem, (long)nl[0].n_value, L_SET) == -1) {
				advise (LLOG_EXCEPTIONS, memory, "lseek fails");
				(void) close(kmem);
				tickadj = 0;
			}
			if (tickadj && write(kmem, (char *)&tickadj,
					     sizeof(tickadj)) !=
			    sizeof(tickadj)) {
				advise (LLOG_EXCEPTIONS, memory, "tickadj set fails");
				tickadj = 0;
			} 
			if (tickadj && tickadj != kern_tickadj)
				advise (LLOG_NOTICE, NULLCP,
				       "System tickadj SET to %d",
				       tickadj);
		} else {
			advise (LLOG_EXCEPTIONS, memory, "Can't open");
		}
	}
#endif	/* SETTICKADJ */
	(void) close(kmem);

	/*
	 *  If we have successfully discovered `hz' from the kernel, then we
	 *  can set sys.precision, if it has not already been specified.  If
	 *  no value of `hz' is available, then use default (-6)
	 */
	if (sys.precision == 0) {
		if (kern_hz <= 64)
			sys.precision = -6;
		else if (kern_hz <= 128)
			sys.precision = -7;
		else if (kern_hz <= 256)
			sys.precision = -8;
		else if (kern_hz <= 512)
			sys.precision = -9;
		else if (kern_hz <= 1024)
			sys.precision = -10;
		else sys.precision = -11;
		advise (LLOG_NOTICE, NULLCP,
			"sys.precision set to %d from sys clock of %d HZ",
		       sys.precision, kern_hz);
	}
}


/*
 * Given host or net name or internet address in dot notation assign the
 * internet address in byte format. source is ../routed/startup.c with minor
 * changes to detect syntax errors. 
 *
 * We now try to interpret the name as in address before we go off and bother
 * the domain name servers.
 *
 * Unfortunately the library routine inet_addr() does not detect mal formed
 * addresses that have characters or byte values > 255. 
 */

GetHostName(name, addr)
	char *name;
	struct Naddr *addr;
{
	long HostAddr;
	struct hostent *hp;
	char	*cp;

	if (cp = index (name, ':')) {
		*cp ++ = '\0';
		if (strcmp (name, "INET") == 0)
			addr->type = AF_INET;
		else if (strcmp (name, "OSI") == 0)
			addr->type = AF_OSI;
		else {
			advise  (LLOG_EXCEPTIONS, NULLCP, "Unknown prefix %s", name);
			return NOTOK;
		}
		name = cp;
			
	}
	else 
		addr->type = AF_INET;

	if (addr->type == AF_INET && (HostAddr = inet_addr(name)) != -1) {
		addr->inet_ad.sin_addr.s_addr = (u_long) HostAddr;
		addr->inet_ad.sin_family = AF_INET;
		addr->inet_ad.sin_port = servport;
		return OK;
	}

	if (addr->type == AF_INET && (hp = gethostbyname(name)) != NULL) {
		if (hp->h_addrtype != AF_INET)
			return NOTOK;
		bcopy((char *) hp->h_addr, (char *) &addr->inet_ad.sin_addr,
		      hp->h_length);
		addr->inet_ad.sin_family = hp->h_addrtype;
		addr->inet_ad.sin_port = servport;
		return OK;
	}
	if (addr->type == AF_OSI) {
		struct PSAPaddr *pa;
		if ((pa = str2paddr (name)) == NULLPA)
			return NOTOK;
		addr->psap_ad = *pa;
		return OK;
	}
	return (NOTOK);
}
/* every hour, dump some useful information to the log */
static void
hourly() {
	char buf[200];
	register int p = 0;
	static double drifts[5] = { 0.0, 0.0, 0.0, 0.0, 0.0 };
	static int drift_count = 0;
	extern double drift_comp, compliance;
	extern int peer_switches, peer_sw_inhibited;

	(void) sprintf(buf, "stats: dc %f comp %f peersw %d inh %d",
		       drift_comp, compliance, peer_switches,
		       peer_sw_inhibited);

	if (sys.peer == NULL) {
		(void) strcat(buf, " UNSYNC");
#ifdef	REFCLOCK
	} else if (sys.peer->flags & PEER_FL_REFCLOCK) {
		p = strlen(buf);
		(void) sprintf(buf + p, " off %f SYNC %.4s %d",
			       sys.peer->estoffset,
			       sys.peer->refid.rid_string,
			       sys.peer->stratum);
#endif
	} else {
		p = strlen(buf);
		(void) sprintf(buf + p, " off %f SYNC %s %d",
			       sys.peer->estoffset,
			       paddr (&sys.peer->src),
			       sys.peer->stratum);
	}
	advise (LLOG_NOTICE, NULLCP, buf);
	/*
	 *  If the drift compensation snapshot file is open, then write
	 *  the current value to it.  Since there's only one block in the
	 *  file, and no one else is reading it, we'll just keep the file
	 *  open and write to it.
	 */
	if (drift_fd >= 0) {
		drifts[drift_count % 5] = drift_comp;
		/* works out to be 70 bytes */
		(void) sprintf(buf,
		     "%+12.10f %+12.10f %+12.10f %+12.10f %+12.10f %4d\n",
			       drifts[drift_count % 5],
			       drifts[(drift_count+4) % 5],
			       drifts[(drift_count+3) % 5],
			       drifts[(drift_count+2) % 5],
			       drifts[(drift_count+1) % 5],
			       drift_count + 1);

		(void) lseek(drift_fd, 0L, L_SET);
		if (write(drift_fd, buf, strlen(buf)) < 0) {
			advise (LLOG_EXCEPTIONS, "write error", "drift comp file");
		}
		drift_count++;
	}
}
/* Debugging stuff */
#if	defined(DEBUG) && defined(SIGUSR1) && defined(SIGUSR2)
int
static incdebug()
{
	if (debug == 255)
		return;
	debug++;
	advise (LLOG_DEBUG, NULLCP, "DEBUG LEVEL %d", debug);
}

static int
decdebug()
{
	if (debug == 0)
		return;
	debug--;
	advise (LLOG_DEBUG, NULLCP, "DEBUG LEVEL %d", debug);
}
#endif

static int
finish(sig)
	int sig;
{
	exit(1);
}

#ifdef	REFCLOCK
struct refclock {
	int fd;
	int (*reader)();
	struct refclock *next;
} *refclocks = NULL;

int init_clock_local(), read_clock_local();
#ifdef PSTI
int init_clock_psti(), read_clock_psti();
#endif PSTI

init_clock(name, type)
char *name, *type;
{
	struct refclock *r;
	int (*reader)();
	int cfd;

	if (strcmp(type, "local") == 0) {
		reader = read_clock_local;
		cfd = init_clock_local(name);
	}
#ifdef PSTI
	else if (strcmp(type, "psti") == 0) {
		reader = read_clock_psti;
		cfd = init_clock_psti(name);
	}
#endif PSTI
	else {
		advise (LLOG_EXCEPTIONS, NULLCP,
			"Unknown reference clock type (%s)", type);
		return(-1);
	}
	if (cfd >= 0) {
		r = (struct refclock *)malloc(sizeof(struct refclock));
		r->fd = cfd;
		r->reader = reader;
		r->next = refclocks;
		refclocks = r;
	}
	return(cfd);
}

read_clock(cfd, tvpp, otvpp)
int cfd;
struct timeval **tvpp, **otvpp;
{
	struct refclock *r;

	for (r = refclocks; r; r = r->next)
		if(r->fd == cfd)
			return((r->reader)(cfd, tvpp, otvpp));
	return(1); /* Can't happen */
}
#endif

create_listeners ()
{
	(void) create_sockets(servport);

	create_osilisten (osiaddress);
}

char	*paddr (addr)
struct Naddr *addr;
{
	static char buf[128];

	switch (addr -> type) {
	    case AF_UNSPEC:
		return "None";

	    case AF_INET:
		return ntoa (&addr -> inet_ad);
		
	    case AF_OSI:
		return paddr2str (&addr -> psap_ad, NULLNA);

	    default:
		(void) sprintf (buf, "Unknown address type %d", addr -> type);
		return buf;
	}
}

envinit ()
{
	int s;

	if (!debug) {
		if (fork())
			exit(0);

		for (s = getdtablesize(); s >= 0; s--)
			(void) close(s);
		(void) open("/", 0);
		(void) dup2(0, 1);
		(void) dup2(0, 2);
		(void) setpgrp(0, getpid());
#ifdef TIOCNOTTY
		s = open("/dev/tty", O_RDWR);
		if (s >= 0) {
			(void) ioctl(s, TIOCNOTTY, (char *) 0);
			(void) close(s);
		}
#endif
		ll_hdinit (pgm_log, myname);
	}
	else	{
		pgm_log -> ll_events = LLOG_ALL;
		ll_dbinit (pgm_log, myname);
	}

	advise (LLOG_NOTICE, NULLCP,
		"%s starting: version $Revision: 7.2 $ patchlevel %d",
		myname, PATCHLEVEL);

#ifdef SYS5
	(void) nice (priority);
#else
	(void) setpriority(PRIO_PROCESS, 0, priority);
#endif

#ifdef	NOSWAP
	if (noswap)
		if (plock(PROCLOCK) != 0)
			advise (LLOG_EXCEPTIONS, "failed", "plock()");
#endif
}

#include <varargs.h>

#ifndef	lint
void	adios (va_alist)
va_dcl
{
    va_list ap;
    extern LLog *pgm_log;

    va_start (ap);
    
    _ll_log (pgm_log, LLOG_FATAL, ap);

    va_end (ap);

    _exit (1);
}
#else
/* VARARGS2 */

void	adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef	lint
void	advise (va_alist)
va_dcl
{
    int	    code;
    extern LLog    *pgm_log;
    va_list ap;

    va_start (ap);
    
    code = va_arg (ap, int);

    _ll_log (pgm_log, code, ap);

    va_end (ap);
}
#else
/* VARARGS3 */

void	advise (code, what, fmt)
char   *what,
       *fmt;
int	code;
{
    advise (code, what, fmt);
}
#endif

addr_compare (pa1, pa2)
struct Naddr *pa1, *pa2;
{
	if (pa1 -> type != pa2 -> type)
		return 0;
	switch (pa1 -> type) {
	    case AF_INET:
		if (pa1 -> inet_ad.sin_addr.s_addr !=
		    pa2 -> inet_ad.sin_addr.s_addr)
			return 0;
		if (pa1 -> inet_ad.sin_port != pa2 -> inet_ad.sin_port)
			return 0;
		return 1;
	    case AF_OSI:
		return psapaddr_cmp (&pa1 -> psap_ad, &pa2 -> psap_ad);
	    default:
		return 0;
	}
}

psapaddr_cmp (pa1, pa2)
struct PSAPaddr *pa1, *pa2;
{
	char	*ps1, *ps2;

	if ((ps1 = _paddr2str (pa1, NULLNA, -1)) == NULLCP)
		return 0;
	if ((ps2 = _paddr2str (pa2, NULLNA, -1)) == NULLCP)
		return 0;
	if (strcmp (ps1, ps2) == 0)
		return 1;
	return 0;
}

struct ntp_peer *find_peer (n)
{
	struct ntp_peer *peer;

	for (peer = peer_list.head; peer; peer = peer->next)
		if (peer -> sock == n)
			return peer;
	TRACE (1, ("Can't find peer with sock %d", n));
	return NULL;
}

struct intf *getintf (n)
int	*n;
{
	int acount;
	struct intf *ap;

	for (acount = 0, ap = addrs; acount < nintf; ap++, acount++)
		if (ap -> flags == 0) {
			*n = acount;
			return ap;
		}
	if (nintf == 0)
		addrs = (struct intf *) malloc (sizeof *ap);
	else
		addrs = (struct intf *)realloc ((char *)addrs,
						(unsigned)(nintf+1) * sizeof *ap);
	if (addrs == NULL)
		adios ("memory", "out of");
	ap = &addrs[nintf];
	bzero ((char *)ap, sizeof *ap);

	*n = nintf++;
	return ap;
}
