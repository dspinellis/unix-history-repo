/* snmpd.c - SNMP agent for 4BSD/ISODE */

/* Someday add sets for these objects...
	ifAdminStatus
	arptable
	ipForwarding
	route table
	clnpForwarding
	clnp route table
	clnp es-is table
	smux stuff
	exprExpr
 */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/snmpd.c,v 7.51 91/03/09 11:57:29 mrose Exp $";
#endif

/*
 * $Header: /f/osi/snmp/RCS/snmpd.c,v 7.51 91/03/09 11:57:29 mrose Exp $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	snmpd.c,v $
 * Revision 7.51  91/03/09  11:57:29  mrose
 * update
 * 
 * Revision 7.49  91/02/20  17:25:55  mrose
 * stuff
 *
 * Revision 7.48  91/01/12  11:43:24  mrose
 * stuff
 *
 * Revision 7.47  91/01/11  15:35:10  mrose
 * sets
 *
 * Revision 7.46  91/01/07  12:40:55  mrose
 * update
 *
 * Revision 7.45  90/12/23  18:43:00  mrose
 * update
 *
 * Revision 7.44  90/12/18  10:13:50  mrose
 * update
 *
 * Revision 7.43  90/12/17  22:08:59  mrose
 * split-up
 *
 * Revision 7.42  90/11/20  15:32:16  mrose
 * update
 *
 * Revision 7.41  90/10/29  18:38:51  mrose
 * updates
 *
 * Revision 7.40  90/10/23  20:36:29  mrose
 * update
 *
 * Revision 7.39  90/09/03  12:57:40  mrose
 * update
 *
 * Revision 7.38  90/08/30  15:11:15  mrose
 * ho-hum
 *
 * Revision 7.37  90/08/29  12:23:44  mrose
 * touch-up
 *
 * Revision 7.36  90/08/08  14:01:07  mrose
 * stuff
 *
 * Revision 7.35  90/07/09  14:49:04  mrose
 * sync
 *
 * Revision 7.34  90/07/01  21:07:26  mrose
 * pepsy
 *
 * Revision 7.33  90/06/23  17:07:38  mrose
 * loopback
 *
 * Revision 7.32  90/06/23  17:01:24  mrose
 * update
 *
 * Revision 7.31  90/06/23  01:33:12  mrose
 * proxy again
 *
 * Revision 7.30  90/06/21  21:27:14  mrose
 * proxy and snmpt
 *
 * Revision 7.29  90/06/20  23:52:57  mrose
 * again
 *
 * Revision 7.28  90/06/20  21:38:33  mrose
 * update
 *
 * Revision 7.27  90/06/15  16:58:39  mrose
 * update
 *
 * Revision 7.26  90/06/13  17:58:44  mrose
 * defaultView
 *
 * Revision 7.25  90/06/12  05:19:03  mrose
 * again
 *
 * Revision 7.24  90/06/12  02:21:43  mrose
 * again
 *
 * Revision 7.23  90/06/12  02:05:33  mrose
 * views ...
 *
 * Revision 7.22  90/06/05  20:47:10  mrose
 * touch-up
 *
 * Revision 7.21  90/05/21  10:07:39  mrose
 * bug-fix
 *
 * Revision 7.20  90/05/15  16:56:20  mrose
 * bump COMM_RDWRITE
 *
 * Revision 7.19  90/05/14  19:55:48  mrose
 * optimize views
 *
 * Revision 7.18  90/05/13  18:13:46  mrose
 * update
 *
 * Revision 7.17  90/05/13  17:54:39  mrose
 * views again
 *
 * Revision 7.16  90/05/13  16:18:17  mrose
 * views
 *
 * Revision 7.15  90/04/18  08:51:53  mrose
 * oid_normalize
 *
 * Revision 7.14  90/04/09  08:50:16  mrose
 * update
 *
 * Revision 7.13  90/02/27  18:49:55  mrose
 * unix stuff
 *
 * Revision 7.12  90/02/23  17:47:49  mrose
 * update
 *
 * Revision 7.11  90/02/19  16:25:56  mrose
 * typo
 *
 * Revision 7.10  90/02/19  15:38:50  mrose
 * one more time
 *
 * Revision 7.9  90/02/17  17:18:48  mrose
 * touch-up
 *
 * Revision 7.8  90/01/11  18:34:33  mrose
 * real-sync
 *
 * Revision 7.7  89/12/19  22:01:52  mrose
 * touch-up
 *
 * Revision 7.6  89/12/19  16:18:23  mrose
 * dgram
 *
 * Revision 7.5  89/12/11  16:22:29  mrose
 * more clts
 *
 * Revision 7.4  89/12/09  21:07:41  mrose
 * touch-up
 *
 * Revision 7.3  89/12/08  14:20:27  mrose
 * touch-up
 *
 * Revision 7.2  89/12/07  22:15:12  mrose
 * touch-up
 *
 * Revision 7.1  89/12/01  10:42:15  mrose
 * clts
 *
 * Revision 7.0  89/11/23  22:23:26  mrose
 * Release 6.0
 *
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <varargs.h>
#include "mib.h"
#include <sys/ioctl.h>
#ifdef	BSD42
#include <sys/file.h>
#endif
#ifdef	SYS5
#include <fcntl.h>
#endif
#include <sys/stat.h>
#include "tailor.h"

#include "dgram.h"
#include "tsap.h"
#ifdef	TCP
#define	SMUX
#include "internet.h"
#endif
#ifdef	X25
#include "x25.h"
#define	COTS
#endif
#ifdef	TP4
#include "tp4.h"
#if	!defined(CLTS) && !defined(COTS)
#define	COTS
#endif
#endif

#if	defined(SNMPT) || defined(SNMPC)
#undef	SMUX
#endif


#define	MAXSNMP		484
#define	IDLE_TIME	(3 * 60)

/*    DATA */

int	debug = 0;
static	int	tooBig = MAXSNMP;
static	int	nbits = FD_SETSIZE;
#ifndef	SNMPT
static	int	rflag = 0;
#endif


#define	LLOG_XXX	(LLOG_PDUS | LLOG_DEBUG)

static LLog _pgm_log = {
#ifndef	SNMPT
#ifndef	SNMPC
    "snmpd.log",
#else
    "snmpc.log",
#endif
#else
    "snmpt.log",
#endif
    NULLCP, NULLCP, LLOG_FATAL | LLOG_EXCEPTIONS | LLOG_NOTICE,
    LLOG_FATAL, -1, LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
static LLog *pgm_log = &_pgm_log;

#ifndef	SNMPT
#ifndef	SNMPC
static char *myname = "snmpd";
#else
static char *myname = "snmpc";
#endif
#else
static char *myname = "snmpt";
#endif

	int	tcpservice = 1;
static	int	x25service = 1;
static	int	tp4service = 1;


#define	NTADDRS	FD_SETSIZE

static fd_set	ifds;

static struct TSAPaddr *tz;
static struct TSAPaddr  tas[NTADDRS];

#ifdef	COTS
static fd_set	cfds;
static struct TSAPaddr  taddrs[FD_SETSIZE];
static struct timeval   lru[FD_SETSIZE];
#endif
static char	source[BUFSIZ];


#ifdef	DEBUG
static	int	didhup = OK;

SFD	hupser ();
#endif

void	adios (), advise ();
void	ts_advise ();


extern int  errno;

/*  */

int	nd = NOTOK;

#ifdef	TCP
static	int	udp = NOTOK;
#ifndef	SNMPT
	int	udport;
	int	traport;
#endif
#endif

#ifdef	CLTS
static	int	clts = NOTOK;
#endif

/*  */

#ifndef	SNMPT
int	quantum = 0;


#include "snmp-g.h"

struct snmpstat snmpstat;
int	unix_netstat = 1;


/* PROXY */

#define	NPQ	10

static struct proxyque {
    integer pq_quantum;
    int	    pq_age;

    int	    pq_fd;
    IFP	    pq_closefnx;
    PS	    pq_ps;

    struct qbuf pq_community;
    integer pq_request;
}	pips[NPQ];

static	int	pqs = 0;

static struct proxyque *pqr = NULL;


/* VIEWS */

#include "view-g.h"


static OID viewTree = NULLOID;

static struct view viewque;
struct view *VHead = &viewque;


/* COMMUNITIES */

static struct community commque;
struct community *CHead = &commque;

struct community *str2comm ();


/* TRAPS */

static struct trap trapque;
struct trap *UHead = &trapque;

#ifdef	TCP
static struct type_SNMP_Message *trap = NULL;
#endif
#ifdef	SMUX
static struct qbuf *loopback_addr = NULL;
#endif


/* SMUX GROUP */

#ifdef	SMUX
#include "smux-g.h"


static	int	smux_enabled = 1;
static	int	smux = NOTOK;

static fd_set	sfds;

static struct smuxPeer peerque;
struct smuxPeer *PHead = &peerque;

static struct smuxTree treeque;
struct smuxTree *THead = &treeque;

static struct smuxReserved {
    char   *rb_text;
    OID	    rb_name;
}    reserved[] = {
    "snmp", NULLOID,
    "view", NULLOID,
    "smux", NULLOID,

    NULL
};
#endif

#else	/* SNMPT */

/*  */

#define	proxy_clear(fd)


static	PS	audit = NULLPS;

#endif	/* SNMPT */

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int	    failed,
	    listening,
	    nfds;
    register struct TSAPaddr  *ta;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect  *td = &tds;

    arginit (argv);
    envinit ();

    failed = listening = 0;
    nfds = 0;
    FD_ZERO (&ifds);

    for (ta = tas; ta < tz; ta++) {
	if (ta -> ta_naddr == 0) {
	    if (!tp4service)
		continue;
#ifdef	CLTS
	    goto do_clts;
#endif
	}
	else {
	    register struct NSAPaddr *na = ta -> ta_addrs;

	    switch (na -> na_stack) {
		case NA_TCP:
		    if (!tcpservice)
			continue;
#ifdef	TCP
		    {
			struct sockaddr_in lo_socket;
			register struct sockaddr_in *lsock = &lo_socket;

			bzero ((char *) lsock, sizeof *lsock);
			lsock -> sin_family = AF_INET;
			lsock -> sin_port = na -> na_port;

			if ((udp = start_udp_server (lsock, 0, 0, 0))
			        == NOTOK) {
			    advise (LLOG_EXCEPTIONS, "failed",
				    "start_udp_server");
			    failed++;
			    continue;
			}
			if (udp >= nfds)
			    nfds = udp + 1;
			FD_SET (udp, &ifds);
			if (nd == NOTOK)
			    nd = udp;

			advise (LLOG_NOTICE, NULLCP,
				"listening on UDP port %d",
				(int) ntohs (na -> na_port));
			listening++;
			continue;
		    }
#else
		    advise (LLOG_EXCEPTIONS, NULLCP,
			    "UDP support not configured");
		    failed++;
		    continue;
#endif

	        case NA_X25:
		    if (!x25service)
			continue;
		    break;

	        case NA_NSAP:
		    if (!tp4service)
			continue;
#ifdef	CLTS
do_clts: ;
		    {
			union sockaddr_osi lo_socket;
			register union sockaddr_osi *lsock = &lo_socket;

			(void) gen2tp4 (ta, lsock);
			if ((clts = start_clts_server (lsock, 0, 0, 0))
			        == NOTOK) {
			    advise (LLOG_EXCEPTIONS, "failed",
				    "start_clts_server");
			    failed++;
			    continue;
			}
			if (clts >= nfds)
			    nfds = clts + 1;
			FD_SET (clts, &ifds);
			if (nd == NOTOK)
			    nd = clts;

			advise (LLOG_NOTICE, NULLCP,
				"listening on %s", taddr2str (ta));
			listening++;
			continue;
		    }
#else
		    break;
#endif

		default:
		    adios (NULLCP, "unknown network type 0x%x", na -> na_stack);
		    /* NOT REACHED */
	    }
	}

	advise (LLOG_NOTICE, NULLCP, "listening on %s", taddr2str (ta));

	if (TNetListen (ta, td) == NOTOK) {
	    ts_advise (td, LLOG_EXCEPTIONS, "listen failed");
	    failed++;
	}
	else
	    listening++;
    }

    if (!listening)
	adios (NULLCP, failed ? "no successful listens"
			      : "no network services selected");

#ifndef	SNMPT
    do_trap (int_SNMP_generic__trap_coldStart, 0,
	     (struct type_SNMP_VarBindList *) 0);
#endif

#ifdef	SMUX
    {
	struct sockaddr_in lo_socket;
	register struct sockaddr_in *lsock = &lo_socket;
	register struct servent *sp;
	register struct smuxReserved *sr;
	OT	ot;

	PHead -> pb_forw = PHead -> pb_back = PHead;
	THead -> tb_forw = THead -> tb_back = THead;
	for (sr = reserved; sr -> rb_text; sr++)
	    if (ot = text2obj (sr -> rb_text))
		sr -> rb_name = ot -> ot_name;

	bzero ((char *) lsock, sizeof *lsock);
	lsock -> sin_family = AF_INET;
	lsock -> sin_port = (sp = getservbyname ("smux", "tcp"))
						    ? sp -> s_port
						    : htons ((u_short) 199);

	if (smux_enabled) {
	    if ((smux = start_tcp_server (lsock, SOMAXCONN, 0, 0)) == NOTOK)
		adios ("failed", "start_tcp_server for SMUX");
	    if (smux >= nfds)
		nfds = smux + 1;
	    FD_SET (smux, &ifds);
	}
    }

    FD_ZERO (&sfds);
#endif

#ifdef	COTS
    FD_ZERO (&cfds);
#endif

    for (;;) {
	int	fd,
		secs;
#ifdef	COTS
	struct timeval tvs;
	register struct timeval *tv = &tvs;
#endif
	int	vecp;
	fd_set  rfds;
	char   *vec[4];

	secs = NOTOK;
#ifdef	COTS
	for (fd = 0; fd < nfds; fd++)
	    if (FD_ISSET (fd, &cfds)) {
		secs = IDLE_TIME + 10;
		break;
	    }
#endif

	rfds = ifds;	/* struct copy */
	if (TNetAcceptAux (&vecp, vec, &fd, NULLTA, nfds, &rfds, NULLFD,
			   NULLFD, secs, td) == NOTOK) {
	    ts_advise (td, LLOG_EXCEPTIONS, "TNetAccept failed");
	    continue;
	}

#ifdef	TCP
	if (udp != NOTOK && FD_ISSET (udp, &rfds))
	    doit_udp (udp);
#endif

#ifdef	SMUX
	if (smux != NOTOK
		&& FD_ISSET (smux, &rfds)
	        && (fd = start_smux ()) != NOTOK) {
	    if (fd >= nfds)
		nfds = fd + 1;
	    FD_SET (fd, &ifds);
	    FD_SET (fd, &sfds);
	}

	for (fd = 0; fd < nfds; fd++)
	    if (FD_ISSET (fd, &rfds) && FD_ISSET (fd, &sfds))
		doit_smux (fd);
#endif

#ifdef	CLTS
	if (clts != NOTOK && FD_ISSET (clts, &rfds))
	    doit_clts (clts);
#endif

#ifdef	COTS
	if (vecp > 0 && (fd = start_tsap (vecp, vec)) != NOTOK) {
	    if (fd >= nfds)
		nfds = fd + 1;
	    FD_SET (fd, &ifds);
	    FD_SET (fd, &cfds);
	}

	for (fd = 0; fd < nfds; fd++)
	    if (FD_ISSET (fd, &rfds) && FD_ISSET (fd, &cfds))
		doit_cots (fd);

	(void) gettimeofday (tv, (struct timezone *) 0);
	tv -> tv_sec -= (long) IDLE_TIME;

	for (fd = 0; fd < nfds; fd++)
	    if (FD_ISSET (fd, &cfds)) {
		if (timercmp (tv, &lru[fd], >)) {
		    advise (LLOG_EXCEPTIONS, NULLCP,
			    "clearing connection from %d: %s", fd,
			    taddr2str (taddrs + fd));
		    (void) TDiscRequest (fd, NULLCP, 0, td);

		    FD_CLR (fd, &ifds);
		    FD_CLR (fd, &cfds);
		    proxy_clear (fd);
		}
	    }

	for (fd = nfds - 1; fd >= 0; fd--)
	    if (FD_ISSET (fd, &ifds))
		break;
	nfds = fd + 1;
#endif
    }
}

/*  */

static void  ts_advise (td, code, event)
register struct TSAPdisconnect *td;
int	code;
char   *event;
{
    char    buffer[BUFSIZ];

    if (td -> td_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s",
		TErrString (td -> td_reason),
		td -> td_cc, td -> td_cc, td -> td_data);
    else
	(void) sprintf (buffer, "[%s]", TErrString (td -> td_reason));

    advise (code, NULLCP, "%s: %s", event, buffer);
}

/*    DOIT */

#ifdef	TCP
static	doit_udp (pd)
int	pd;
{
    int	    fd;
    char   *cp;
    struct sockaddr_in in_socket;
    register struct sockaddr_in *isock = &in_socket;
    struct NSAPaddr nas;
    register struct NSAPaddr *na = &nas;

    if ((fd = join_udp_client (pd, isock)) == NOTOK) {
	if (errno == EWOULDBLOCK)
	    return;
	adios ("failed", "join_udp_client");
    }

    (void) sprintf (source, "Internet=%s+%d+2",
		    cp = inet_ntoa (isock -> sin_addr),
		    (int) ntohs (isock -> sin_port));

    bzero ((char *) na, sizeof *na);
    na -> na_stack = NA_TCP;
    na -> na_community = ts_comm_tcp_default;
    (void) strncpy (na -> na_domain, cp, sizeof na -> na_domain - 1);
    na -> na_port = isock -> sin_port;

    doit_aux (fd, na, read_udp_socket, write_udp_socket, check_udp_socket);
#ifndef	SNMPT
    if (pqr)
	pqr -> pq_fd = fd, pqr -> pq_closefnx = close_udp_socket;
    else
#endif
	(void) close_udp_socket (fd);
}
#endif

/*  */

#ifdef	CLTS
static	doit_clts (pd)
int	pd;
{
    int	    fd;
    char   *cp;
    union sockaddr_osi in_socket;
    register union sockaddr_osi *isock = &in_socket;
    struct TSAPaddr tas;
    register struct TSAPaddr *ta = &tas;

    if ((fd = join_clts_client (pd, isock)) == NOTOK) {
	if (errno == EWOULDBLOCK)
	    return;
	adios ("failed", "join_tcp_client");
    }
    (void) tp42gen (ta, isock);

    (void) strcpy (source, taddr2str (ta));

    doit_aux (fd, ta -> ta_addrs, read_clts_socket, write_clts_socket,
	      check_clts_socket);
#ifndef	SNMPT
    if (pqr)
	pqr -> pq_fd = fd, pqr -> pq_closefnx = close_clts_socket;
    else
#endif
	(void) close_clts_socket (fd);
}
#endif

/*  */

#ifdef	COTS
static int  start_tsap (vecp, vec)
int	vecp;
char  **vec;
{
    struct TSAPstart tss;
    register struct TSAPstart *ts = &tss;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect  *td = &tds;

    if (TInit (vecp, vec, ts, td) == NOTOK) {
	ts_advise (td, LLOG_EXCEPTIONS, "T-CONNECT.INDICATION");
	return NOTOK;
    }

    advise (LLOG_XXX, NULLCP,
	    "T-CONNECT.INDICATION: <%d, %s, %s, %d, %d>",
	    ts -> ts_sd,
	    taddr2str (&ts -> ts_calling), taddr2str (&ts -> ts_called),
	    ts -> ts_expedited, ts -> ts_tsdusize);

    if (TConnResponse (ts -> ts_sd, NULLTA, 0, NULLCP, 0, NULLQOS, td)
	    == NOTOK) {
	ts_advise (td, LLOG_EXCEPTIONS, "T-CONNECT.RESPONSE");
	return NOTOK;
    }

    taddrs[ts -> ts_sd] = ts -> ts_calling;	/* struct copy */
    (void) gettimeofday (lru + ts -> ts_sd, (struct timezone *) 0);

    return ts -> ts_sd;
}

/*  */

static	doit_cots (fd)
int	fd;
{
    (void) strcpy (source, taddr2str (taddrs + fd));
    doit_aux (fd, &(taddrs[fd].ta_addrs[0]), ts_read, ts_write, NULLIFP);
#ifndef	SNMPT
    if (pqr)
	pqr -> pq_fd = fd, pqr -> pq_closefnx = NULLIFP;
#endif

    (void) gettimeofday (lru + fd, (struct timezone *) 0);
}
#endif

/*  */

static	doit_aux (fd, na, rfx, wfx, cfx)
int	fd;
struct NSAPaddr *na;
IFP	rfx,
	wfx,
	cfx;
{
    int	    result,
    	    size;
    PE	    pe;
    PS	    ps;
    struct type_SNMP_Message *msg;

#ifdef	DEBUG
    didhup = NOTOK;
#endif

    result = NOTOK;
    pe = NULLPE;
    msg = NULL;
#ifndef	SNMPT
    pqr = NULL;
#endif
    if ((ps = ps_alloc (dg_open)) == NULLPS
	    || dg_setup (ps, fd, MAXSNMP, rfx, wfx, cfx) == NOTOK) {
	if (ps == NULLPS)
	    advise (LLOG_EXCEPTIONS, NULLCP, "ps_alloc: out of memory (%s)",
		    source);
	else
	    advise (LLOG_EXCEPTIONS, NULLCP, "dg_setup: %s (%s)",
		    ps_error (ps -> ps_errno), source);

	goto out;
    }

    size = ps -> ps_byteno;
    if ((pe = ps2pe (ps)) == NULLPE) {
#ifdef	COTS
	if (rfx == ts_read) {
	    FD_CLR (fd, &ifds);
	    FD_CLR (fd, &cfds);
	    proxy_clear (fd);

	    if (ps -> ps_errno == PS_ERR_NONE) {
		advise (LLOG_XXX, NULLCP,
			"T-DISCONNECT.INDICATION: %d (%s)", fd, source);
		goto out;
	    }
	}
#endif
	advise (LLOG_EXCEPTIONS, NULLCP, "ps2pe: %s (%s)",
		ps_error (ps -> ps_errno), source);
#ifndef	SNMPT
	snmpstat.s_inpkts++;
	snmpstat.s_asnparseerrs++;
#endif
	goto out;
    }
    size = ps -> ps_byteno - size;
#ifdef	COTS
    if (rfx == ts_read)
	advise (LLOG_XXX, NULLCP, "T-DATA.INDICATION: %d (%s)", fd, source);
    else
#endif
	advise (LLOG_XXX, NULLCP, "packet from %s", source);

#ifndef	SNMPT
    snmpstat.s_inpkts++;
#endif

    if (decode_SNMP_Message (pe, 1, NULLIP, NULLVP, &msg) == NOTOK) {
	advise (LLOG_EXCEPTIONS, NULLCP, "decode_SNMP_Message: %s (%s)",
		PY_pepy, source);
#ifndef	SNMPT
	snmpstat.s_asnparseerrs++;
#endif
#ifdef	COTS
	if (rfx == ts_read) {
	    struct TSAPdisconnect tds;

	    advise (LLOG_EXCEPTIONS, NULLCP,
		    "clearing connection from %d: %s", fd,
		    taddr2str (taddrs + fd));
	    (void) TDiscRequest (fd, NULLCP, 0, &tds);

	    FD_CLR (fd, &ifds);
	    FD_CLR (fd, &cfds);
	    proxy_clear (fd);
	}
#endif
	goto out;
    }

    PLOGP (pgm_log,SNMP_Message, pe, "Message", 1);

    result = process (ps, msg, na, size);

out: ;
    if (msg) {
/* BEGIN MOBY PEPSY HACK... */

	register struct type_SNMP_VarBindList *vp =
			msg -> data -> un.get__request -> variable__bindings;

	free_SNMP_VarBindList (vp);

	msg -> data -> un.get__request -> variable__bindings = NULL;

/* END MOBY PEPSY HACK... */

	free_SNMP_Message (msg);
    }
    if (pe)
	pe_free (pe);
    if (result != OK && ps)
	ps_free (ps);

#ifdef	DEBUG
    hupser (SIGHUP);
#endif
}

/*    PROCESS */

#ifndef	SNMPT
static int  process (ps, msg, na, size)
PS	ps;
register struct type_SNMP_Message *msg;
struct NSAPaddr *na;
int	size;
{
    int	    result;
    char   *commname;
    struct community *comm;
    PE	    pe;

    if (msg -> version != int_SNMP_version_version__1) {
	advise (LLOG_EXCEPTIONS, NULLCP, "badVersion: %d (%s)",
		msg -> version, source);
	snmpstat.s_badversions++;
	return DONE;
    }

    if ((commname = qb2str (msg -> community)) == NULLCP) {
	advise (LLOG_EXCEPTIONS, NULLCP, "qb2str: out of memory (%s)", source);
	return DONE;
    }

    result = NOTOK;
    if ((comm = str2comm (commname, na)) == NULL) {
	advise (LLOG_EXCEPTIONS, NULLCP, "badCommunity: %s (%s)",
		commname, source);
	snmpstat.s_badcommunitynames++;
	if (snmpstat.s_enableauthentraps == TRAPS_ENABLED)
	    do_trap (int_SNMP_generic__trap_authenticationFailure, 0,
		     (struct type_SNMP_VarBindList *) 0);
	goto out;
    }

    if ((result = do_operation (ps, msg, comm, size)) != DONE)
	goto out;

    pe = NULLPE;

    if (encode_SNMP_Message (&pe, 1, 0, NULLCP, msg) != NOTOK) {
	PLOGP (pgm_log,SNMP_Message, pe, "Message", 0);

	if (pe2ps (ps, pe) == NOTOK)
	    advise (LLOG_EXCEPTIONS, NULLCP, "pe2ps: %s (%s)",
		    ps_error (ps -> ps_errno), source);
	else
	    snmpstat.s_outpkts++;
    }
    else
	advise (LLOG_EXCEPTIONS, NULLCP, "encode_SNMP_Message: %s (%s)",
		PY_pepy, source);

    if (pe)
	pe_free (pe);

out: ;
    free (commname);

    return result;
}

/*  */

static int  do_operation (ps, msg, comm, size)
PS	ps;
struct type_SNMP_Message *msg;
struct community *comm;
int	size;
{
    int	    idx,
	    offset;
    struct view *vu = comm -> c_view;
    register struct type_SNMP_PDUs *pdu = msg -> data;
    register struct type_SNMP_VarBindList *vp;
    struct type_SNMP_VarBindList *op = NULL;
    register struct type_SNMP_GetResponse__PDU *parm = pdu -> un.get__response;

    idx = 0;
    switch (pdu -> offset) {
	case type_SNMP_PDUs_get__request:
	    snmpstat.s_ingetrequests++;
	    if (vu == NULL || !(comm -> c_permission & OT_RDONLY)) {
access_denied: ;
		advise (LLOG_EXCEPTIONS, NULLCP,
			"authorizationFailure: %d (%s)", pdu -> offset,
			source);
/* no trap for this... */
		offset = pdu -> offset;
		pdu -> offset = type_SNMP_PDUs_get__response;
		parm -> error__status = int_SNMP_error__status_noSuchName;
		goto out;
	    }
	    break;
	case type_SNMP_PDUs_get__next__request:
	    snmpstat.s_ingetnexts++;
	    if (vu == NULL || !(comm -> c_permission & OT_RDONLY))
		goto access_denied;
	    break;

	case type_SNMP_PDUs_set__request:
	    snmpstat.s_insetrequests++;
	    if (vu == NULL || !(comm -> c_permission & OT_WRONLY))
		goto access_denied;
	    break;

	case type_SNMP_PDUs_get__response:
	    snmpstat.s_ingetresponses++;
	    if (vu == NULL)
		goto access_denied;
	    if ((comm -> c_permission & OT_YYY) && proxy2 (msg) != OK)
		return NOTOK;
	    goto bad_operation;

	case type_SNMP_PDUs_trap:
	    snmpstat.s_intraps++;
	    if (vu == NULL)
		goto access_denied;
	    advise (LLOG_EXCEPTIONS, NULLCP,
		    "unexpectedOperation: %d (%s)", pdu -> offset, source);
	    return NOTOK;

	default:
	    if (vu == NULL)
		goto access_denied;
bad_operation: ;
	    advise (LLOG_EXCEPTIONS, NULLCP,
		    "badOperation: %d (%s)", pdu -> offset, source);
	    return NOTOK;
    }

    if (vu -> v_community)
	return proxy1 (ps, msg, comm);

    offset = pdu -> offset;
    pdu -> offset = type_SNMP_PDUs_get__response;

    {
	register struct type_SNMP_VarBindList **opp;

	opp = &op;
	for (vp = msg -> data -> un.get__request -> variable__bindings;
	         vp;
	         vp = vp -> next) {
	    register struct type_SNMP_VarBindList *bind;
	    register struct type_SNMP_VarBind *v;

	    if ((bind = (struct type_SNMP_VarBindList *)
					calloc (1, sizeof *bind)) == NULL) {
no_mem: ;
		free_SNMP_VarBindList (op);
		op = NULL;
		advise (LLOG_EXCEPTIONS, NULLCP, "out of memory");

		parm -> error__status = int_SNMP_error__status_genErr;
		goto out;
	    }
	    *opp = bind, opp = &bind -> next;

	    if ((v = (struct type_SNMP_VarBind *) calloc (1, sizeof *v))
		    == NULL)
		goto no_mem;
	    bind -> VarBind = v;

	    if ((v -> name = oid_cpy (vp -> VarBind -> name)) == NULLOID)
		goto no_mem;
	    (v -> value = vp -> VarBind -> value) -> pe_refcnt++;
	}
    }

    quantum++;

    switch (offset) {
	case type_SNMP_PDUs_get__request:
	case type_SNMP_PDUs_get__next__request:
	default:
	    idx = do_pass (msg, offset, vu);
	    break;

	case type_SNMP_PDUs_set__request:
	    if (idx = do_pass (msg, offset, vu)) {
		int	status = parm -> error__status;

		(void) do_pass (msg, type_SNMP_PDUs_rollback, vu);

		parm -> error__status = status;
	    }
	    else {
		(void) do_pass (msg, type_SNMP_PDUs_commit, vu);
		gc_set ();
	    }
	    break;
    }

out: ;
    parm -> error__index = idx;
    switch (parm -> error__status) {
	case int_SNMP_error__status_noError:
	    for (vp = op; vp; vp = vp -> next)
		size -= ps_get_abs (vp -> VarBind -> value);
	    for (vp = msg -> data -> un.get__request -> variable__bindings;
		     vp;
		     vp = vp -> next)
		if ((size += ps_get_abs (vp -> VarBind -> value)) >= tooBig) {
		    parm -> error__status = int_SNMP_error__status_tooBig;
		    goto out;
		}	

	    idx = 0;
	    for (vp = msg -> data -> un.get__request -> variable__bindings;
		     vp;
		     vp = vp -> next)
		idx++;
	    switch (offset) {
	        case type_SNMP_PDUs_get__request:
	        case type_SNMP_PDUs_get__next__request:
		    snmpstat.s_totalreqvars += idx;
		    break;

	        case type_SNMP_PDUs_set__request:
		    snmpstat.s_totalsetvars += idx;
		    break;
	    }
	    free_SNMP_VarBindList (op);
	    break;

	case int_SNMP_error__status_tooBig:
	    snmpstat.s_toobigs++;
	    goto losing;

	case int_SNMP_error__status_readOnly:
	    advise (LLOG_NOTICE, NULLCP, "lurking readOnly");
	    /* and fall... */
	case int_SNMP_error__status_noSuchName:
	    snmpstat.s_nosuchnames++;
	    goto losing;

	case int_SNMP_error__status_badValue:
	    snmpstat.s_badvalues++;
	    goto losing;

	case int_SNMP_error__status_genErr:
	    snmpstat.s_generrs++;
	    goto losing;

	default:
losing: ;
	    if (op) {
		free_SNMP_VarBindList (msg -> data -> un.get__request
							-> variable__bindings);
		msg -> data -> un.get__request -> variable__bindings = op;
	    }
	    break;
    }
    snmpstat.s_outgetresponses++;

    return DONE;
}

/*  */

static int  do_pass (msg, offset, vu)
struct type_SNMP_Message *msg;
int	offset;
struct view *vu;
{
    int	    idx,
	    status;
    object_instance ois;
    register struct type_SNMP_PDUs *pdu = msg -> data;
    register struct type_SNMP_VarBindList *vp;
    register struct type_SNMP_GetResponse__PDU *parm = pdu -> un.get__response;
    IFP	    method;

    idx = 0;
    for (vp = msg -> data -> un.get__request -> variable__bindings;
	     vp;
	     vp = vp -> next) {
	register OI	oi;
	register OT	ot;
	register struct type_SNMP_VarBind *v = vp -> VarBind;

	idx++;
	
	if (offset == type_SNMP_PDUs_get__next__request) {
	    if ((oi = name2inst (v -> name)) == NULLOI
		    && (oi = next2inst (v -> name)) == NULLOI)
		goto no_name;

	    if ((ot = oi -> oi_type) -> ot_getfnx == NULLIFP
		    && ot -> ot_smux == NULL)
		goto get_next;
	}
	else {
	    if ((oi = name2inst (v -> name)) == NULLOI)
		goto no_name;
	    ot = oi -> oi_type;
	    if ((offset == type_SNMP_PDUs_get__request
		 	    ? ot -> ot_getfnx : ot -> ot_setfnx) == NULLIFP
		    && ot -> ot_smux == NULL) {
no_name: ;
		parm -> error__status = int_SNMP_error__status_noSuchName;
		return idx;
	    }
	}

try_again: ;
	switch (offset) {
	    case type_SNMP_PDUs_get__request:
	        if (!(vu -> v_mask & ot -> ot_views)) {
losing_name: ;
		    snmpstat.s_badcommunityuses++;
		    goto no_name;
		}
		method = ot -> ot_getfnx;
		break;

	    case type_SNMP_PDUs_get__next__request:
	        if (!(vu -> v_mask & ot -> ot_views))
		    goto get_next;
		method = ot -> ot_getfnx;
		break;

	    case type_SNMP_PDUs_set__request:
	    case type_SNMP_PDUs_commit:
	    case type_SNMP_PDUs_rollback:
	        if (!(vu -> v_mask & ot -> ot_views))
		    goto losing_name;
		method = ot -> ot_setfnx;
		break;
	}

#ifdef	SMUX
	if (ot -> ot_smux)
	    status = smux_method (pdu, ot,
				  ((struct smuxTree *) ot -> ot_smux)
								    -> tb_peer,
				  v, offset);
	else
#endif
	    status = (*method) (oi, v, offset);

	switch (status) {
	    case NOTOK:	    /* get-next wants a bump */
#ifdef SMUX
	        /* If current object is being handled by a SMUX sub-agent, then
		   step over the entire subtree before proceeding with the
		   'get-next' search.  This is needed to avoid descending into
		   the portion of the object tree that the SMUX sub-agent has
		   'mounted over'. (EJP) */
	        if (ot -> ot_smux) {
		    int level = ot -> ot_name -> oid_nelem;
		    
 		    while (ot -> ot_next
			       && ot -> ot_next -> ot_name -> oid_nelem
					> level)
		      ot = ot -> ot_next;
		  }
#endif
get_next: ;
		oi = &ois;
		for (;;) {
		    if ((ot = ot -> ot_next) == NULLOT) {
			parm -> error__status =
					    int_SNMP_error__status_noSuchName;
			return idx;
		    }
		    oi -> oi_name = (oi -> oi_type = ot) -> ot_name;
		    if (ot -> ot_getfnx || ot -> ot_smux)
			goto try_again;
		}

	    case int_SNMP_error__status_noError:
		break;

	    default:
		parm -> error__status = status;
		return idx;
	}
    }

    return 0;
}

/*  */

static	gc_set ()
{
#ifdef	SMUX
    register struct smuxPeer *pb,
			     *qb;
    register struct smuxTree *tb,
			     *ub;

    for (pb = PHead -> pb_forw; pb != PHead; pb = qb) {
	qb = pb -> pb_forw;

	if (pb -> pb_invalid)
	    pb_free (pb);
    }

    for (tb = THead -> tb_forw; tb != THead; tb = ub) {
	ub = tb -> tb_forw;
	
	if (tb -> tb_invalid)
	    tb_free (tb);
    }
#endif
}

/*    PROXY */

static int  proxy1 (psp, msg, comm)
PS	psp;
struct type_SNMP_Message *msg;
struct community *comm;
{
    int	    result;
    register struct view *v = comm -> c_view;
    register struct proxyque *pq;
    PE	    pe;
    PS	    ps;

    if (qb_pullup (msg -> community) == NOTOK) {
	advise (LLOG_EXCEPTIONS, NULLCP, "qb_pullup: out of memory (proxy %s)",
		source);
	return NOTOK;
    }
    if (pqs >= NPQ) {
	register struct proxyque *qp;

	for (qp = (pq = pips) + NPQ; pq < qp; pq++)
	    if (pq -> pq_age < quantum) {
		advise (LLOG_NOTICE, NULLCP, "proxy flush");
		if (pq -> pq_closefnx) {
		    ps_free (pq -> pq_ps);
		    (void) (*pq -> pq_closefnx) (pq -> pq_fd);
		}
		QBFREE (&pq -> pq_community);
		break;
	    }
	if (pq >= qp) {
	    advise (LLOG_EXCEPTIONS, NULLCP,
		    "proxy for view %s, but no proxyque slots available (%s)",
		    oid2ode (v -> v_name), source);
	    return NOTOK;
	}
    }
    else
	pq = pips + pqs++;

    pq -> pq_quantum = quantum;
    pq -> pq_age = quantum + 20;	/* who knows what a good value is?!? */
    pq -> pq_ps = psp;
    pq -> pq_community.qb_forw = pq -> pq_community.qb_back
		= &pq -> pq_community;
    insque (msg -> community -> qb_forw, &pq -> pq_community);
    free ((char *) msg -> community);
    pq -> pq_request = msg -> data -> un.get__request -> request__id;

    msg -> community = v -> v_community;
    msg -> data -> un.get__request -> request__id = pq -> pq_quantum;

    result = NOTOK;
    pe = NULLPE;
    if (encode_SNMP_Message (&pe, 1, 0, NULLCP, msg) == NOTOK) {
	advise (LLOG_EXCEPTIONS, NULLCP, "encode_SNMP_Message: %s (proxy %s)",
		PY_pepy, source);
	if (pe)
	    pe_free (pe);
	goto out;
    }
    PLOGP (pgm_log,SNMP_Message, pe, "Message", 0);

    if ((ps = ps_alloc (dg_open)) == NULLPS
	    || dg_setup (ps, udp, MAXSNMP, read_udp_socket, write_udp_socket,
 			 check_udp_socket) == NOTOK) {
	if (ps == NULLPS)
	    advise (LLOG_EXCEPTIONS, NULLCP,
		    "ps_alloc: out of memory (proxy %s)", source);
	else
	    advise (LLOG_EXCEPTIONS, NULLCP, "dg_setup: %s (proxy %s)",
		    ps_error (ps -> ps_errno), source);
    }
    else
	if (hack_dgram_socket (udp, &v -> v_sa) == NOTOK)
	    advise (LLOG_EXCEPTIONS, "failed",
		    "hack_dgram_socket(1) (proxy %s)", source);
	else
	    if (pe2ps (ps, pe) == NOTOK)
		advise (LLOG_EXCEPTIONS, NULLCP, "pe2ps: %s (proxy %s)",
			ps_error (ps -> ps_errno), source);
	    else {
		result = OK;

		if (hack_dgram_socket (udp, (struct sockaddr *) NULL) == NOTOK)
		    advise (LLOG_EXCEPTIONS, "failed",
			    "hack_dgram_socket(2) (proxy %s)", source);
	    }

    pe_free (pe);

    if (ps)
	ps_free (ps);

out: ;
    msg -> community = NULL;
    if (result == NOTOK) {
	register struct proxyque *qp = pips + --pqs;

	if (pq -> pq_closefnx) {
	    ps_free (pq -> pq_ps);
	    (void) (*pq -> pq_closefnx) (pq -> pq_fd);
	}
	QBFREE (&pq -> pq_community);

	if (pq != qp)
	    *pq = *qp;		/* struct copy */
    }
    else
	pqr = pq;

    return result;
}

/*  */

static int  proxy2 (msg)
struct type_SNMP_Message *msg;
{
    integer  request;
    register struct proxyque *pq,
			     *qp;
    PE	    pe;

    request = msg -> data -> un.get__request -> request__id;
    for (qp = (pq = pips) + pqs; pq < qp; pq++)
	if (pq -> pq_quantum == request)
	    break;
    if (pq >= qp)
	return OK;

    qb_free (msg -> community);
    msg -> community = &pq -> pq_community;
    msg -> data -> un.get__request -> request__id = pq -> pq_request;

    pe = NULLPE;
    if (encode_SNMP_Message (&pe, 1, 0, NULLCP, msg) == NOTOK) {
	advise (LLOG_EXCEPTIONS, NULLCP, "encode_SNMP_Message: %s (proxy %s)",
		PY_pepy, source);
	if (pe)
	    pe_free (pe);
	goto out;
    }
    PLOGP (pgm_log,SNMP_Message, pe, "Message", 0);

    if (pe2ps (pq -> pq_ps, pe) == NOTOK)
	advise (LLOG_EXCEPTIONS, NULLCP, "pe2ps: %s (proxy %s)",
		ps_error (pq -> pq_ps -> ps_errno), source);
    snmpstat.s_outgetresponses++;

    pe_free (pe);

out: ;
    msg -> community = NULL;

    qp = pips + --pqs;

    if (pq -> pq_closefnx) {
	ps_free (pq -> pq_ps);
	(void) (*pq -> pq_closefnx) (pq -> pq_fd);
    }
    QBFREE (&pq -> pq_community);

    if (pq != qp)
	*pq = *qp;	/* struct copy */

    return DONE;
}

/*  */

#ifdef	COTS
static	proxy_clear (fd)
int	fd;
{
    register struct proxyque *pq,
			     *qp;

again: ;
    for (qp = (pq = pips) + pqs; pq < qp; pq++)
	if (pq -> pq_fd == fd) {
	    qp = pips + --pqs;

	    if (pq -> pq_closefnx) {
		ps_free (pq -> pq_ps);
		(void) (*pq -> pq_closefnx) (pq -> pq_fd);
	    }
	    QBFREE (&pq -> pq_community);

	    if (pq != qp)
		*pq = *qp;
	    goto again;
	}
}
#endif

/*    SMUX */

#ifdef	SMUX
#include "smux.h"


static int  start_smux ()
{
    int	    fd;
    struct sockaddr_in in_socket;
    register struct sockaddr_in *isock = &in_socket;
    register struct smuxPeer *pb,
			     *qb;
    static int smux_peerno = 0;

    if ((fd = join_tcp_client (smux, isock)) == NOTOK) {
	if (errno == EWOULDBLOCK)
	    return NOTOK;
	adios ("failed", "join_tcp_client");
    }

    if ((pb = (struct smuxPeer *) calloc (1, sizeof *pb)) == NULL) {
	advise (LLOG_EXCEPTIONS, NULLCP, "doit_smux: out of memory");
out: ;
	(void) close_tcp_socket (fd);
	return NOTOK;
    }

    pb -> pb_address = *isock;		/* struct copy */

    /* Format sockets consistantly with other places in this program,
       with a plus sign between the internet address and port number.  (EJP) */
    (void) sprintf (pb -> pb_source, "%s+%d",
		    inet_ntoa (pb -> pb_address.sin_addr),
		    (int) ntohs (pb -> pb_address.sin_port));
    (void) strcpy (source, pb -> pb_source);

    if ((pb -> pb_ps = ps_alloc (fdx_open)) == NULLPS
	    || fdx_setup (pb -> pb_ps, fd) == NOTOK) {
	if (pb -> pb_ps == NULLPS)
	    advise (LLOG_EXCEPTIONS, NULLCP,
		    "ps_alloc: out of memory (SMUX %s)", source);
	else
	    advise (LLOG_EXCEPTIONS, NULLCP, "fdx_setup: %s (SMUX %s)",
		    ps_error (pb -> pb_ps -> ps_errno), source);

	pb_free (pb);
	goto out;
    }

    /* Insert new smuxPeer structure at the end of the doubly-linked
       list anchored at PHead and assign it the next sequential index
       number for use as smuxPindex in o_smuxPeer() and smuxTindex in
       o_smuxTree().  (EJP) */

    insque (pb, PHead -> pb_back);
    pb -> pb_index = ++smux_peerno;

    return (pb -> pb_fd = fd);
}

/*  */

static int  doit_smux (fd)
int	fd;
{
    PE	    pe;
    register struct smuxPeer *pb;
    struct type_SNMP_SMUX__PDUs *pdu;

    for (pb = PHead -> pb_forw; pb != PHead; pb = pb -> pb_forw)
	if (pb -> pb_fd == fd)
	    break;
    if (pb == PHead) {
	advise (LLOG_EXCEPTIONS, NULLCP, "lost smuxPeer block for %d", fd);
	FD_CLR (fd, &ifds);
	FD_CLR (fd, &sfds);

	return;
    }

    (void) strcpy (source, pb -> pb_source);

    if ((pe = ps2pe (pb -> pb_ps)) == NULLPE) {
	advise (LLOG_EXCEPTIONS, NULLCP, "ps2pe: %s (SMUX %s)",
		ps_error (pb -> pb_ps -> ps_errno), source);

out: ;
	if (pe)
	    pe_free (pe);
	pb_free (pb);
	return;
    }

    advise (LLOG_XXX, NULLCP, "SMUX packet from %s", source);

    pdu = NULL;

    if (decode_SNMP_SMUX__PDUs (pe, 1, NULLIP, NULLVP, &pdu) == NOTOK) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"decode_SNMP_SMUX__PDUs: %s (SMUX %s)", PY_pepy, source);
	goto out;
    }

    PLOGP (pgm_log,SNMP_SMUX__PDUs, pe, "SMUX Message", 1);

    if (smux_process (pb, pdu) == NOTOK)
	pb_free (pb);

    if (pdu)
	free_SNMP_SMUX__PDUs (pdu);
    if (pe)
	pe_free (pe);
}

/*  */

static	smux_process (pb, pdu)
register struct smuxPeer *pb;
struct type_SNMP_SMUX__PDUs *pdu;
{
    int	    result = OK;

    switch (pdu -> offset) {
	case type_SNMP_SMUX__PDUs_simple:
	    if (pb -> pb_identity)
		goto unexpected;
	    {
		register struct type_SNMP_SimpleOpen *simple =
							    pdu -> un.simple;
		register struct smuxEntry *se;

		if (simple -> version != int_SNMP_version_version__1) {
		    advise (LLOG_EXCEPTIONS, NULLCP,
			    "badVersion: %d (SMUX %s)",
			    simple -> version, source);
		    return NOTOK;
		}

		pb -> pb_identity = simple -> identity;
		simple -> identity = NULL;

		if ((pb -> pb_description = qb2str (simple -> description))
		        == NULL) {
		    advise (LLOG_EXCEPTIONS, NULLCP,
			    "qb2str: out of memory (SMUX %s)", source);
		    return NOTOK;
		}

		if (qb_pullup (simple -> password) == NOTOK) {
		    advise (LLOG_EXCEPTIONS, NULLCP,
			    "qb_pullup: out of memory (SMUX %s)", source);
		    return NOTOK;
		}

		if ((se = getsmuxEntrybyidentity (pb -> pb_identity)) == NULL
			|| strcmp (se -> se_password,
				   simple -> password -> qb_forw -> qb_data)) {
		    advise (LLOG_EXCEPTIONS, NULLCP,
			    "%s: %s (SMUX %s)",
			    se ? "badPassword" : "badIdentity",
			    oid2ode (simple -> identity), source);
		    if (snmpstat.s_enableauthentraps == TRAPS_ENABLED)
			do_trap (int_SNMP_generic__trap_authenticationFailure,
				 0, (struct type_SNMP_VarBindList *) 0);
		    return NOTOK;
		}

		if ((pb -> pb_priority = se -> se_priority) < 0)
		    pb -> pb_priority = 0;
		
		advise (LLOG_NOTICE, NULLCP,
			"SMUX open: %d %s \"%s\" (%d/ %s)",
			pb -> pb_index, oid2ode (pb -> pb_identity),
			pb -> pb_description, pb -> pb_fd, source);
	    }
	    break;

	case type_SNMP_SMUX__PDUs_close:
	    if (!pb -> pb_identity)
		goto unexpected;
	    advise (LLOG_NOTICE, NULLCP,
		    "SMUX close: %s (%s)",
		    smux_error (pdu -> un.close -> parm), source);
	    return NOTOK;

	case type_SNMP_SMUX__PDUs_registerRequest:
	    if (!pb -> pb_identity)
		goto unexpected;
	    {
		register struct type_SNMP_RReqPDU *rreq =
						    pdu -> un.registerRequest;
		struct type_SNMP_RRspPDU rrsp;
		struct type_SNMP_SMUX__PDUs rsp;
		register struct smuxReserved *sr;
		register struct smuxTree *tb = NULL;
		register struct smuxTree *qb;
		register OID	oid = rreq -> subtree;
		OT	ot = NULLOT;
		PE	pe;

		for (sr = reserved; sr -> rb_text; sr++)
		    if (sr -> rb_name
			    && bcmp ((char *) sr -> rb_name -> oid_elements,
				     (char *) oid -> oid_elements,
				     (sr -> rb_name -> oid_nelem
				         <= oid -> oid_nelem
				     		? sr -> rb_name -> oid_nelem
						: oid -> oid_nelem)
				         * sizeof oid -> oid_elements[0])
				    == 0) {
			advise (LLOG_EXCEPTIONS, NULLCP,
				"reservedSubTree: %s %s %s (SMUX %s)",
				oid2ode (oid),
				sr -> rb_name -> oid_nelem
				    <= oid -> oid_nelem
					? "under" : "contains",
				sr -> rb_text, source);
			goto no_dice;
		    }

		if ((ot = name2obj (oid)) == NULLOT) {
		    if (rreq -> operation == int_SNMP_operation_delete) {
			advise (LLOG_EXCEPTIONS, NULLCP,
				"noSuchSubTree: %s (SMUX %s)",
				oid2ode (oid), source);
			goto no_dice;
		    }
		
		    if ((ot = (OT) calloc (1, sizeof *ot)) == NULL
			    || (ot -> ot_text = ot -> ot_id =
					strdup (sprintoid (oid)))
			        == NULL) {
			advise (LLOG_EXCEPTIONS, NULLCP,
				"out of memory (SMUX %s)", source);
			if (ot)
			    free ((char *) ot);
			return NOTOK;
		    }
		    ot -> ot_name = rreq -> subtree;
		    rreq -> subtree = NULL;
		    ot -> ot_access = rreq -> operation;
		    ot -> ot_status = OT_OPTIONAL;
		    export_view (ot);

		    (void) add_objects (ot);
		}
		else {
		    if (rreq -> operation == int_SNMP_operation_delete) {
			for (tb = (struct smuxTree *) ot -> ot_smux;
			         tb;
			         tb = tb -> tb_next)
			    if (tb -> tb_peer == pb
				    && (rreq -> priority < 0
					    || rreq -> priority
							== tb -> tb_priority))
				break;
			if (tb)
			    tb_free (tb);
			else {
			    advise (LLOG_EXCEPTIONS, NULLCP,
				    "noSuchRegistration: %s (SMUX %s)",
				    oid2ode (oid), source);
			    ot = NULLOT;
			}
			goto no_dice;
		    }

		    if (ot -> ot_name -> oid_nelem > oid -> oid_nelem) {
			advise (LLOG_EXCEPTIONS, NULLCP,
				"badSubTree: %s (SMUX %s)",
				oid2ode (oid), source);
			ot = NULL;
			goto no_dice;
		    }

		    export_view (ot);
		}

		if ((tb = (struct smuxTree *) calloc (1, sizeof *tb))
		        == NULL) {
		    advise (LLOG_EXCEPTIONS, NULLCP,
			    "out of memory (SMUX %s)", source);
		    return NOTOK;
		}

		if ((tb -> tb_priority = rreq -> priority) < pb -> pb_priority)
		    tb -> tb_priority = pb -> pb_priority;
		for (qb = (struct smuxTree *) ot -> ot_smux;
		         qb;
		         qb = qb -> tb_next)
		    if (qb -> tb_priority > tb -> tb_priority)
			break;
		    else
			if (qb -> tb_priority == tb -> tb_priority)
			    tb -> tb_priority++;

		tb -> tb_peer = pb;

no_dice: ;
		bzero ((char *) &rsp, sizeof rsp);
		rsp.offset = type_SNMP_SMUX__PDUs_registerResponse;
		rsp.un.registerResponse = &rrsp;

		bzero ((char *) &rrsp, sizeof rrsp);
		rrsp.parm = tb ? tb -> tb_priority
				  : int_SNMP_RRspPDU_failure;

		pe = NULLPE;

		if (encode_SNMP_SMUX__PDUs (&pe, 1, 0, NULLCP, &rsp)
			!= NOTOK) {
		    PLOGP (pgm_log,SNMP_SMUX__PDUs, pe, "SMUX Message", 0);

		    if (pe2ps (pb -> pb_ps, pe) == NOTOK) {
			advise (LLOG_EXCEPTIONS, NULLCP, "pe2ps: %s (SMUX %s)",
				ps_error (pb -> pb_ps -> ps_errno), source);
			result = NOTOK;
		    }
		    else {
			if (ot)
			    advise (LLOG_NOTICE, NULLCP,
				    "SMUX register: %s %s in=%d out=%d (%s)",
				    rreq -> operation
				        == int_SNMP_operation_delete ? "delete"
					: rreq -> operation
				    		== int_SNMP_operation_readOnly
				        ? "readOnly" : "readWrite",
				    oid2ode (ot -> ot_name),
				    rreq -> priority,
				    tb ? tb -> tb_priority : -1, source);

			if (tb
			        && rreq -> operation
					!= int_SNMP_operation_delete) {
			    register int    i;
			    register unsigned int *ip,
						  *jp;
			    register struct smuxTree **qpp;

			    tb -> tb_subtree = ot;

		    /* Insert the new element into the single-linked chain
		       of smuxTree structures for this object type that is
		       anchored in the object tree.  Elements are chained
		       in order of increasing priority.  (EJP) */

 			    for (qpp = (struct smuxTree **) &ot -> ot_smux;
				     qb = *qpp;
				     qpp = &qb -> tb_next)
				if (qb -> tb_priority > tb -> tb_priority)
				    break;
			    tb -> tb_next = qb;
			    *qpp = tb;

		    /* Fill in the tb_instance field of the new element
		       with the concatenation of the size of the object name
		       followed by the object name itself followed by
		       its priority.  (EJP) */

			    ip = tb -> tb_instance;
			    jp = ot -> ot_name -> oid_elements;
			    *ip++ = ot -> ot_name -> oid_nelem;
			    for (i = ot -> ot_name -> oid_nelem; i > 0; i--)
				*ip++ = *jp++;
			    *ip++ = tb -> tb_priority;
			    tb -> tb_insize = ip - tb -> tb_instance;

		    /* Insert the new element into the doubly-linked chain
		       of all smuxTree structures that is anchored in THead.
		       Elements are chained in lexicographic order of
		       tb_instance so that the get_tbent() function will
		       work correctly for 'get next' operations.  (EJP) */

			    for (qb = THead -> tb_forw;
				     qb != THead;
				     qb = qb -> tb_forw)
			      if (elem_cmp (tb -> tb_instance, tb -> tb_insize,
					    qb -> tb_instance, qb -> tb_insize)
				      < 0)
				  break;
			    insque (tb, qb -> tb_back);
			}
		    }
		}
		else {
		    advise (LLOG_EXCEPTIONS, NULLCP,
			    "encode_SNMP_SMUX__PDUs: %s (SMUX %s)",
			    PY_pepy, source);
		    result = NOTOK;
		}

		if (pe)
		    pe_free (pe);
	    }
	    break;

	case type_SNMP_SMUX__PDUs_trap:
	    if (!pb -> pb_identity)
		goto unexpected;
	    {
		struct qbuf *qb;
		struct type_SNMP_Message msgs;
		register struct type_SNMP_Message *msg = &msgs;
		struct type_SNMP_PDUs datas;
		register struct type_SNMP_PDUs *data = &datas;
		register struct type_SNMP_Trap__PDU *parm = pdu -> un.trap;

		advise (LLOG_NOTICE, NULLCP,
			"SMUX trap: %d %d (%s)",
			parm -> generic__trap, parm -> specific__trap, source);

		bzero ((char *) msg, sizeof *msg);
		msg -> version = int_SNMP_version_version__1;
		msg -> data = data;

		bzero ((char *) data, sizeof *data);
		data -> offset = type_SNMP_PDUs_trap;
		data -> un.trap = parm;

		if (loopback_addr
			&& qb_pullup (qb = parm -> agent__addr) != NOTOK
		        && qb -> qb_len == loopback_addr -> qb_len
		        && bcmp (qb -> qb_forw -> qb_data,
				 loopback_addr -> qb_forw -> qb_data,
				 qb -> qb_len) == 0)
		    parm -> agent__addr = trap -> data -> un.trap->agent__addr;
		do_traps (msg, parm -> generic__trap, parm -> specific__trap);
		parm -> agent__addr = qb;
	    }
	    break;

	case type_SNMP_SMUX__PDUs_registerResponse:
	case type_SNMP_SMUX__PDUs_get__request:
	case type_SNMP_SMUX__PDUs_get__next__request:
	case type_SNMP_SMUX__PDUs_get__response:
	case type_SNMP_SMUX__PDUs_set__request:
unexpected: ;
	    advise (LLOG_EXCEPTIONS, NULLCP,
		    "unexpectedOperation: %d (SMUX %s)", pdu -> offset,
		    source);
	    return NOTOK;

	default:
	    advise (LLOG_EXCEPTIONS, NULLCP,
		    "badOperation: %d (SMUX %s)", pdu -> offset, source);
	    return NOTOK;
    }

    return result;
}

/*  */

static int  smux_method (pdu, ot, pb, v, offset)
struct type_SNMP_PDUs *pdu;
OT	ot;
register struct smuxPeer *pb;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    status,
	    orig_id;
    struct type_SNMP_VarBindList *orig_bindings,
				  vps;
    struct type_SNMP_SMUX__PDUs  req,
				*rsp;
    struct type_SNMP_SOutPDU cor;
    register struct type_SNMP_GetResponse__PDU *get;
    PE	    pe;

    status = int_SNMP_error__status_noError;

    bzero ((char *) &req, sizeof req);
    switch (offset) {
	case type_SNMP_PDUs_get__request:
	    req.offset = type_SNMP_SMUX__PDUs_get__request;
	    goto stuff_pdu;

	case type_SNMP_PDUs_get__next__request:
	    req.offset = type_SNMP_SMUX__PDUs_get__next__request;
	    goto stuff_pdu;

	case type_SNMP_PDUs_set__request:
	    req.offset = type_SNMP_SMUX__PDUs_set__request;
stuff_pdu: ;
	    orig_id = pdu -> un.get__request -> request__id;
	    orig_bindings = pdu -> un.get__request -> variable__bindings;

	    req.un.get__request = pdu -> un.get__request;
	    bzero ((char *) &vps, sizeof vps);
	    vps.VarBind = v;

	    pdu -> un.get__request -> request__id = quantum;
	    pdu -> un.get__request -> variable__bindings = &vps;
	    break;

	case type_SNMP_PDUs_commit:
	    cor.parm = int_SNMP_SOutPDU_commit;
	    goto stuff_cor;

	case type_SNMP_PDUs_rollback:
	    cor.parm = int_SNMP_SOutPDU_rollback;
stuff_cor: ;
	    req.offset = type_SNMP_SMUX__PDUs_commitOrRollback;
	    req.un.commitOrRollback = &cor;
	    break;
    }

    pe = NULLPE;

    if (encode_SNMP_SMUX__PDUs (&pe, 1, 0, NULLCP, &req) != NOTOK) {
	PLOGP (pgm_log,SNMP_SMUX__PDUs, pe, "SMUX Message", 0);

	if (pe2ps (pb -> pb_ps, pe) == NOTOK) {
	    advise (LLOG_EXCEPTIONS, NULLCP, "pe2ps: %s (%s, SMUX %s)",
		    ps_error (pb -> pb_ps -> ps_errno), source,
		    pb -> pb_source);

lost_peer: ;
	    pb_free (pb);
	    status = int_SNMP_error__status_genErr;
	}
    }
    else {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"encode_SNMP_SMUX__PDUs: %s (%s)", PY_pepy, source);
	status = int_SNMP_error__status_genErr;
    }

    if (pe)
	pe_free (pe);

    switch (offset) {
	case type_SNMP_PDUs_get__request:
	case type_SNMP_PDUs_get__next__request:
	case type_SNMP_PDUs_set__request:
	    pdu -> un.get__request -> request__id = orig_id;
	    pdu -> un.get__request -> variable__bindings = orig_bindings;
	    break;

	default:
	    break;
    }

    if (status != int_SNMP_error__status_noError
	    || offset == type_SNMP_PDUs_rollback)
	return status;

    if ((pe = ps2pe (pb -> pb_ps)) == NULLPE) {
	advise (LLOG_EXCEPTIONS, NULLCP, "ps2pe: %s (%s, SMUX %s)",
		ps_error (pb -> pb_ps -> ps_errno), source,
		pb -> pb_source);

	goto lost_peer;
    }


    rsp = NULL;

    if (decode_SNMP_SMUX__PDUs (pe, 1, NULLIP, NULLVP, &rsp) == NOTOK) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"decode_SNMP_SMUX__PDUs: %s (%s, SMUX %s)", PY_pepy, source,
		pb -> pb_source);

lost_peer_again: ;
	pb_free (pb);
	status = int_SNMP_error__status_genErr;
	goto out;
    }

    PLOGP (pgm_log,SNMP_SMUX__PDUs, pe, "SMUX Message", 1);

    if (rsp -> offset != type_SNMP_SMUX__PDUs_get__response) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"unexpectedOperation: %d (%s, SMUX %s)", rsp -> offset,
		source, pb -> pb_source);

	goto lost_peer_again;
    }
    get = rsp -> un.get__response;

    switch (status = get -> error__status) {
	case int_SNMP_error__status_noError:
	    {
		register struct type_SNMP_VarBindList *vp;
		register struct type_SNMP_VarBind *v2;

		if ((vp = get -> variable__bindings) == NULL) {
		    advise (LLOG_EXCEPTIONS, NULLCP,
			    "missing variable in get response (%s, SMUX %s)",
			    source, pb -> pb_source);

		    goto lost_peer_again;
		}
		v2 = vp -> VarBind;

		if (offset == type_SNMP_PDUs_get__next__request
		        && (ot -> ot_name -> oid_nelem
			    	    > v2 -> name -> oid_nelem
			        || bcmp ((char *) ot -> ot_name ->oid_elements,
					 (char *) v2 -> name -> oid_elements,
					 ot -> ot_name -> oid_nelem
				           * sizeof ot -> ot_name ->
							oid_elements[0]))) {
			status = NOTOK;
			break;
		}
		free_SNMP_ObjectName (v -> name);
		v -> name = v2 -> name;
		v2 -> name = NULL;
		free_SNMP_ObjectSyntax (v -> value);
		v -> value = v2 -> value;
		v2 -> value = NULL;
	    }
	    break;

	case int_SNMP_error__status_noSuchName:
	    if (offset == type_SNMP_PDUs_get__next__request) {
		status = NOTOK;
		break;
	    }
	    /* else fall */

        default:
	    break;
    }

out: ;
    if (rsp)
	free_SNMP_SMUX__PDUs (rsp);
    if (pe)
	pe_free (pe);

    return status;
}

/*  */

static	pb_free (pb)
register struct smuxPeer *pb;
{
    register struct smuxTree *tb,
			     *ub;

    if (pb == NULL)
	return;

    for (tb = THead -> tb_forw; tb != THead; tb = ub) {
	ub = tb -> tb_forw;
	
	if (tb -> tb_peer == pb)
	    tb_free (tb);
    }

    if (pb -> pb_ps)
	ps_free (pb -> pb_ps);

    if (pb -> pb_fd != NOTOK) {
	(void) close_tcp_socket (pb -> pb_fd);
	FD_CLR (pb -> pb_fd, &ifds);
	FD_CLR (pb -> pb_fd, &sfds);
    }

    if (pb -> pb_identity)
	oid_free (pb -> pb_identity);
    if (pb -> pb_description)
	free (pb -> pb_description);

    remque (pb);

    free ((char *) pb);
}

/*  */

static	tb_free (tb)
register struct smuxTree *tb;
{
    register struct smuxTree *tp,
			    **tpp;

    if (tb == NULL)
	return;

    for (tpp = (struct smuxTree **) &tb -> tb_subtree -> ot_smux;
	     tp = *tpp;
	     tpp = &tp -> tb_next)
	if (tp == tb) {
	    *tpp = tb -> tb_next;
	    break;
	}

    remque (tb);

    free ((char *) tb);
}
#endif	/* SMUX */

/*    VIEWS */

static	start_view () {
    register OT	    ot;

    if (ot = text2obj ("view"))
	viewTree = ot -> ot_name;

    /* Initialize the view mask of all objects in the tree, not just
       the leaves.  This is necessary so that do_pass() will allow
       SMUX sub-agents to mount their subtrees over interior objects.  (EJP) */

    for (ot = text2obj ("ccitt"); ot; ot = ot -> ot_next)
        export_view (ot);
}

/*  */

static	export_view (ot)
register OT	ot;
{
    register struct subtree *s;
    register struct view  *v;
    OID	    name = ot -> ot_name;

    ot -> ot_views = 0;
    for (v = VHead -> v_forw; v != VHead; v = v -> v_forw)
	if ((s = v -> v_subtree.s_forw) != &v -> v_subtree) {
	    for (; s != &v -> v_subtree; s = s -> s_forw)
		if (inSubtree (s -> s_subtree, name))
		    goto mark_it;
	}
	else
	    if (!viewTree || !inSubtree (viewTree, name)) {
mark_it: ;
		ot -> ot_views |= v -> v_mask;
	    }
}

/*    COMMUNITIES */

static	struct community *str2comm (name, na)
char   *name;
register struct NSAPaddr *na;
{
    register struct community *c,
			      *d;

    d = NULL;
    for (c = CHead -> c_forw; c != CHead; c = c -> c_forw)
	if (strcmp (c -> c_name, name) == 0) {
	    if (c -> c_addr.na_stack == NA_TCP
		    && strcmp (c -> c_addr.na_domain, "0.0.0.0") == 0) {
		d = c;
		continue;
	    }
	    else {
		if (c -> c_addr.na_stack != na -> na_stack)
		    continue;
		switch (na -> na_stack) {
		    case NA_TCP:
		        if (strcmp (c -> c_addr.na_domain, na -> na_domain))
			    continue;
			break;

		   case NA_X25:
			if (c -> c_addr.na_dtelen != na -> na_dtelen
			        || bcmp (c -> c_addr.na_dte,
					 na -> na_dte, na -> na_dtelen))
			    continue;
			break;

		    case NA_NSAP:
			if (c -> c_addr.na_addrlen != na -> na_addrlen
			        || bcmp (c -> c_addr.na_address,
					 na -> na_address, na -> na_addrlen))
			    continue;
			break;

		    default:
			adios (NULLCP,
			       "unknown network type (0x%x) for community \"%s\"",
			       na -> na_stack, name);
			/* NOTREACHED */
		}
	    }

	    d = c;
	    break;
	}

    if (d) {
	remque (d);
	insque (d, CHead);
    }

    return d;
}

/*    TRAPS */

static	start_trap () {
#ifdef	TCP
    char    myhost[BUFSIZ];
    register struct hostent *hp;
    struct type_SNMP_Message *msg;
    register struct type_SNMP_PDUs *pdu;
    register struct type_SNMP_Trap__PDU *parm;

    if ((msg = (struct type_SNMP_Message *) calloc (1, sizeof *msg)) == NULL) {
no_mem: ;
	advise (LLOG_EXCEPTIONS, NULLCP,
		"unable to initialize trap structure: out of memory");
out: ;
	if (msg)
	    free_SNMP_Message (msg);

	return;
    }
    msg -> version = int_SNMP_version_version__1;

    if ((pdu = (struct type_SNMP_PDUs *) calloc (1, sizeof *pdu)) == NULL)
	goto no_mem;
    msg -> data = pdu;

    pdu -> offset = type_SNMP_PDUs_trap;

    if ((parm = (struct type_SNMP_Trap__PDU *) calloc (1, sizeof *parm))
	    == NULL)
	goto no_mem;
    pdu -> un.trap = parm;

    (void) strcpy (myhost, TLocalHostName ());
    if (hp = gethostbystring (myhost)) {
	struct sockaddr_in sin;

	inaddr_copy (hp, &sin);
	if ((parm -> agent__addr = str2qb ((char *) &sin.sin_addr, 4, 1))
	        == NULL)
	    goto no_mem;
    }
    else {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"%s: unknown host, so no traps", myhost);
	goto out;
    }

    if ((parm -> time__stamp = (struct type_SNMP_TimeTicks *)
	 	calloc (1, sizeof *parm -> time__stamp)) == NULL)
	goto no_mem;

    trap = msg;

#ifdef	SMUX
    if (hp = gethostbystring ("localhost")) {
	struct sockaddr_in sin;

	inaddr_copy (hp, &sin);
	if ((loopback_addr = str2qb ((char *) &sin.sin_addr, 4, 1)) == NULL)
	    advise (LLOG_EXCEPTIONS, NULLCP,
		    "unable to initialize loopback address: out of memory");
    }
#endif
#endif
}

/*  */

#ifndef	TCP
/* ARGSUSED */
#endif

static	do_trap (generic, specific, bindings)
int	generic,
	specific;
struct type_SNMP_VarBindList *bindings;
{
#ifdef	TCP
    struct type_SNMP_Message *msg;
    register struct type_SNMP_Trap__PDU *parm;
    OT	    ot;

    if ((msg = trap) == NULL)
	return;
    parm = msg -> data -> un.trap;

    if ((ot = text2obj ("sysObjectID")) == NULLOT) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"unable to send trap: no such object: \"%s\"",
		"sysObjectID");
	return;
    }
    if ((parm -> enterprise = (OID) ot -> ot_info) == NULLOID) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"unable to send trap: no value defined for object \"%s\"",
		"sysObjectID");
	return;
    }

    parm -> generic__trap = generic;
    parm -> specific__trap = specific;
    {
	struct timeval now;

	if (gettimeofday (&now, (struct timezone *) 0) == NOTOK) {
	    advise (LLOG_EXCEPTIONS, "failed", "gettimeofday");
	    return;
	}

	parm -> time__stamp -> parm = (now.tv_sec - my_boottime.tv_sec) * 100
	    				+ ((now.tv_usec - my_boottime.tv_usec)
								      / 10000);
    }
    parm -> variable__bindings = bindings;

    do_traps (msg, (integer) generic, (integer) specific);
#endif
}

/*  */

#ifdef	TCP
static	do_traps (msg, generic, specific)
register struct type_SNMP_Message *msg;
integer	generic,
	specific;
{
    int	    mask = 1 << 7 - generic;
    register struct trap *t;

    for (t = UHead -> t_forw; t != UHead; t = t -> t_forw) {
	register struct view *v = t -> t_view;
	PE	pe;
	PS	ps;

	if (specific == 0 && !(t -> t_generics & mask))
	    continue;

	msg -> community = v -> v_community;

	pe = NULLPE;
	if (encode_SNMP_Message (&pe, 1, 0, NULLCP, msg) == NOTOK) {
	    advise (LLOG_EXCEPTIONS, NULLCP,
		    "encode_SNMP_Message: %s", PY_pepy);
	    if (pe)
		pe_free (pe);
	    continue;
	}
	PLOGP (pgm_log,SNMP_Message, pe, "Message", 0);

	if ((ps = ps_alloc (dg_open)) == NULLPS
	        || dg_setup (ps, udp, MAXSNMP, read_udp_socket,
			     write_udp_socket, check_udp_socket) == NOTOK) {
	    if (ps == NULLPS)
		advise (LLOG_EXCEPTIONS, NULLCP, "ps_alloc: out of memory");
	    else
		advise (LLOG_EXCEPTIONS, NULLCP, "dg_setup: %s",
			ps_error (ps -> ps_errno));
	}
	else {
	    if (hack_dgram_socket (udp, &v -> v_sa) == NOTOK)
		advise (LLOG_EXCEPTIONS, "failed", "hack_dgram_socket(1)");
	    else {
		if (pe2ps (ps, pe) == NOTOK)
		    advise (LLOG_EXCEPTIONS, NULLCP, "pe2ps: %s",
			    ps_error (ps -> ps_errno));
		else {
		    snmpstat.s_outpkts++, snmpstat.s_outtraps++;
 		}

		/* This function should always be called, regardless of whether
		   pe2ps() succeeds or fails.  (EJP) */
		if (hack_dgram_socket (udp, (struct sockaddr *) NULL) == NOTOK)
			advise (LLOG_EXCEPTIONS, "failed", "hack_dgram_socket(2)");
	    }
        }

	pe_free (pe);

	if (ps)
	    ps_free (ps);
	else
	    break;
    }
}
#endif
#else	/* SNMPT */

/*  */

/* ARGSUSED */

static int  process (ps, msg, na, size)
PS	ps;
register struct type_SNMP_Message *msg;
struct NSAPaddr *na;
{
    char   *cp;
    long    now;
    PE	    pe,
	    p;
    register struct type_SNMP_PDUs *pdu = msg -> data;
    register struct tm *tm;
    struct UTCtime uts;
    register struct UTCtime *ut = &uts;
    register struct type_SNMP_Audit *au;

    if (msg -> version != int_SNMP_version_version__1) {
	advise (LLOG_EXCEPTIONS, NULLCP, "badVersion: %d (%s)",
		msg -> version, source);
	return NOTOK;
    }

    if (pdu -> offset != type_SNMP_PDUs_trap) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"unexpectedOperation: %d (%s)", pdu -> offset, source);
	return NOTOK;
    }

    pe = p = NULLPE;
    au = NULL;

    if (encode_SNMP_Message (&p, 1, 0, NULLCP, msg) == NOTOK) {
	advise (LLOG_EXCEPTIONS, NULLCP, "encode_SNMP_Message: %s (%s)",
		PY_pepy, source);
	goto out;
    }

    if ((au = (struct type_SNMP_Audit *) calloc (1, sizeof *au)) == NULL)
	goto no_mem;
    au -> sizeOfEncodingWhichFollows = ps_get_abs (p);

    if ((au -> source = str2qb (source, strlen (source), 1)) == NULL) {
no_mem: ;
        advise (LLOG_EXCEPTIONS, NULLCP, "out of memory for audit (%s)",
		source);
	goto out;
    }
    (void) time (&now);

    if (tm = gmtime (&now))
	tm2ut (tm, ut);
    else {
	advise (LLOG_EXCEPTIONS, NULLCP, "gmtime failed");

	bzero ((char *) ut, sizeof *ut);
    }

    if ((cp = gent2str (ut)) == NULL
	    || (au -> dateAndTime = str2qb (cp, strlen (cp), 1)) == NULL)
	goto no_mem;

    if (encode_SNMP_Audit (&pe, 1, 0, NULLCP, au) != NOTOK) {
	PLOGP (pgm_log,SNMP_Audit, pe, "Audit", 0);
	PLOGP (pgm_log,SNMP_Message, p, "Message", 0);

	if (pe2ps (audit, pe) == NOTOK || pe2ps (audit, p) == NOTOK)
	    advise (LLOG_EXCEPTIONS, NULLCP, "pe2ps: %s (%s)",
		    ps_error (audit -> ps_errno), source);

	(void) ps_flush (audit);
    }
    else
	advise (LLOG_EXCEPTIONS, NULLCP, "encode_SNMP_Audit: %s (%s)",
		PY_pepy, source);

out: ;
    if (au)
	free_SNMP_Audit (au);
    if (pe)
	pe_free (pe);
    if (p)
	pe_free (p);

    return DONE;
}
#endif	/* SNMPT */

/*    MISCELLANY */

static	arginit (vec)
char	**vec;
{
    register char  *ap;
#ifdef	SNMPT
    char   *file = "snmp.traps";
    FILE   *fp;
#endif
#ifdef	TCP
    int	    port;
    struct NSAPaddr *tcp_na;
    register struct servent *sp;
#endif
#ifdef	X25
    struct NSAPaddr *x25_na;
#endif

    if (myname = rindex (*vec, '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = *vec;

    isodetailor (myname, 0);
    ll_hdinit (pgm_log, myname);

    bzero ((char *) tas, sizeof tas);
    tz = tas;

#ifdef	TCP
    if (!(ts_stacks & TS_TCP))
	tcpservice = 0;
    if ((sp = getservbyname ("snmp", "udp")) == NULL)
	advise (LLOG_EXCEPTIONS, NULLCP, "udp/snmp: unknown service");

    tcp_na = tz -> ta_addrs;
    tcp_na -> na_stack = NA_TCP;
    tcp_na -> na_community = ts_comm_tcp_default;
    tcp_na -> na_domain[0] = NULL;
#ifndef	SNMPT
    tcp_na -> na_port = sp ? sp -> s_port : htons ((u_short) 161);
    udport = tcp_na -> na_port;
#endif
    tz -> ta_naddr = 1;

    tz++;

    if ((sp = getservbyname ("snmp-trap", "udp")) == NULL)
	advise (LLOG_EXCEPTIONS, NULLCP, "udp/snmp-trap: unknown service");
#ifndef	SNMPT
    traport = sp ? sp -> s_port : htons ((u_short) 162);
#else
    tcp_na -> na_port = sp ? sp -> s_port : htons ((u_short) 162);
#endif
#endif

#ifdef	COTS
    bzero ((char *) taddrs, sizeof taddrs);
    bzero ((char *) lru, sizeof lru);
#endif

#ifdef	X25
    if (!(ts_stacks & TS_X25))
	x25service = 0;

    x25_na = tz -> ta_addrs;
    x25_na -> na_stack = NA_X25;
    x25_na -> na_community = ts_comm_x25_default;
    if (x25_local_dte && *x25_local_dte) {
	(void) strcpy (x25_na -> na_dte, x25_local_dte);
	x25_na -> na_dtelen = strlen (x25_na -> na_dte);
    }
#ifndef	SNMPT
    x25_na -> na_pidlen = str2sel ("03018200", -1, x25_na -> na_pid, NPSIZE);
#else
    x25_na -> na_pidlen = str2sel ("03019000", -1, x25_na -> na_pid, NPSIZE);
#endif
    tz -> ta_naddr = 1;

    tz++;
#endif

#ifdef	TP4
    if (!(ts_stacks & TS_TP4))
	tp4service = 0;

#ifndef	SNMPT
    bcopy ("snmp", tz -> ta_selector, tz -> ta_selectlen = sizeof "snmp" - 1);
#else
    bcopy ("snmp-trap", tz -> ta_selector,
	   tz -> ta_selectlen = sizeof "snmp-trap" - 1);
#endif
    tz -> ta_naddr = 0;

    tz++;
#endif

    for (vec++; ap = *vec; vec++) {
	if (*ap == '-')
	    switch (*++ap) {
		case 'b':
		    if ((ap = *++vec) == NULL
			    || *ap == '-'
			    || (tooBig = atoi (ap)) <= 128 /* MAXSNMP */)
			adios (NULLCP, "usage: %s -b size", myname);
		    continue;

		case 'd':
		    debug++;
		    continue;

#ifndef	SNMPT
		case 's':
#ifdef	SMUX
		    smux_enabled = 0;
#endif
		    continue;
#endif

		case 't':
		    ts_stacks = TS_TCP;
		    tcpservice = 1;
		    x25service = tp4service = 0;
		    continue;

		case 'x':
		    ts_stacks = TS_X25;
		    x25service = 1;
		    tcpservice = tp4service = 0;
		    continue;

		case 'z':
		    ts_stacks = TS_TP4;
		    tp4service = 1;
		    tcpservice = x25service = 0;
		    continue;

#ifndef	SNMPT
		case 'r':
		    rflag = 1;
		    continue;
#else
		case 'f':
		    if ((file = *++vec) == NULL || *file == '-')
			adios (NULLCP, "usage: %s -f audit-file", myname);
		    continue;
#endif

#ifdef	TCP
		case 'p':
		    if ((ap = *++vec) == NULL
			    || *ap == '-'
			    || (port = atoi (ap)) <= 0)
			adios (NULLCP, "usage: %s -p portno", myname);
		    tcp_na -> na_port = htons ((u_short) port);
		    continue;
#endif

#ifdef X25
		/* This permits listening on a specific subaddress. */
		case 'a':
		    if ((ap = *++vec) == NULL || *ap == '-')
			adios (NULLCP, "usage: %s -a x121address", myname);
		    (void) strcpy (x25_na -> na_dte, ap);
		    x25_na -> na_dtelen = strlen (ap);
		    continue;

		/* This permits listening on a specific protocol id.
		   In fact, SunLink X.25 lets you listen on a protocol
		   id mask, but let's keep it simple. */
		case 'i':
		    if ((ap = *++vec) == NULL || *ap == '-' )
			adios (NULLCP, "usage: %s -i pid", myname);
		    x25_na -> na_pidlen =
			str2sel (ap, -1, x25_na -> na_pid, NPSIZE);
		    continue;
#endif

		default:
		    adios (NULLCP, "-%s: unknown switch", ap);
	    }

	adios (NULLCP, "usage: %s [switches]", myname);
    }

    ps_len_strategy = PS_LEN_LONG;

#ifdef	SNMPT
    file = _isodefile (isodelogpath, file);
    if ((fp = fopen (file, "a")) == NULL)
	adios (file, "unable to append to");
    if ((audit = ps_alloc (std_open)) == NULLPS)
	adios (NULLCP, "ps_alloc(std_open): you lose");
    if (std_setup (audit, fp) == NOTOK)
	adios (NULLCP, "std_setup: %s", ps_error (audit -> ps_errno));
#endif
}

/*  */

static  envinit () {
    int     i,
            sd;
#ifndef	SNMPT
    char   *cp;
#endif
    char    file[BUFSIZ];
    FILE   *fp;

    nbits = getdtablesize ();

    if (debug == 0 && !(debug = isatty (2))) {
	for (i = 0; i < 5; i++) {
	    switch (fork ()) {
		case NOTOK:
		    sleep (5);
		    continue;

		case OK:
		    break;

		default:
		    _exit (0);
	    }
	    break;
	}

	(void) chdir ("/");

	if ((sd = open ("/dev/null", O_RDWR)) == NOTOK)
	    adios ("/dev/null", "unable to read");
	if (sd != 0)
	    (void) dup2 (sd, 0), (void) close (sd);
	(void) dup2 (0, 1);
	(void) dup2 (0, 2);

#ifdef	SETSID
	if (setsid () == NOTOK)
	    advise (LLOG_EXCEPTIONS, "failed", "setsid");
#endif
#ifdef	TIOCNOTTY
	if ((sd = open ("/dev/tty", O_RDWR)) != NOTOK) {
	    (void) ioctl (sd, TIOCNOTTY, NULLCP);
	    (void) close (sd);
	}
#else
#ifdef	SYS5
	(void) setpgrp ();
	(void) signal (SIGINT, SIG_IGN);
	(void) signal (SIGQUIT, SIG_IGN);
#endif
#endif
    }
    else
	ll_dbinit (pgm_log, myname);

#ifndef	sun		/* damn YP... */
    for (sd = 3; sd < nbits; sd++)
	if (pgm_log -> ll_fd != sd)
	    (void) close (sd);
#endif

    (void) signal (SIGPIPE, SIG_IGN);

    ll_hdinit (pgm_log, myname);

#ifndef	SNMPT
#ifndef	SNMPC
    cp = "snmpd.defs";
#else
    cp = "snmpc.defs";
#endif
    if (readobjects (cp) == NOTOK)
	adios (NULLCP, "readobjects: %s", PY_pepy);

    init_mib ();
#ifndef	SNMPC
    init_system ();	    /* Internet-standard MIB */
    init_interfaces ();
    init_ip ();
    init_icmp ();
    init_tcp ();
    init_udp ();
    init_clns ();	    /* experimental CLNS group */
#endif
    init_snmp ();
    init_view ();
#ifdef	SMUX
    init_smux ();
#endif
    init_eval ();

    readconfig ();

    fin_view ();
    fin_mib ();

    start_trap ();
    start_view ();

    o_advise = (IFP) advise;
#endif

    (void) sprintf (file, "/etc/%s.pid", myname);
    if (fp = fopen (file, "w")) {
	(void) fprintf (fp, "%d\n", getpid ());
	(void) fclose (fp);
    }

    advise (LLOG_NOTICE, NULLCP, "starting");
#ifdef	DEBUG
    (void) signal (SIGHUP, hupser);
#endif
}

/*  */

#ifdef	DEBUG
static SFD  hupser (sig)
int	sig;
{
    char    buffer[BUFSIZ];
    PE	    p;

    if (didhup == NOTOK) {
	didhup = OK;
	return;
    }

    advise (LLOG_EXCEPTIONS, NULLCP,
	    "sbrk=0x%x allocs=%d frees=%d most=%d",
	    sbrk (0), pe_allocs, pe_frees, pe_most);

    for (p = pe_active; p; p = p -> pe_link) {
	(void) sprintf (buffer, "active PE 0x%x (refcnt %d)", (caddr_t) p,
			p -> pe_refcnt);

	_vpdu (pgm_log, vunknown, p, buffer, -1);
    }
}
#endif

/*    CONFIG */

#ifndef	SNMPT

int	f_community (), f_logging (), f_proxy (), f_trap (), f_variable (),
	f_view (), f_expression ();

static struct pair {
    char   *p_name;		/* runcom directive */
    IFP	    p_handler;		/* dispatch */
}	pairs[] = {
    "community",    f_community,
    "logging",	    f_logging,
    "proxy",	    f_proxy,
    "trap",	    f_trap,
    "variable",	    f_variable,
    "view",	    f_view,
    "expression",    f_expression,

    NULL
};

/*  */

static	readconfig () {
    register char *cp;
    char    buffer[BUFSIZ],
	    line[BUFSIZ],
	   *vec[NVEC + NSLACK + 1];
    register struct pair *p;
    struct stat st;
    FILE   *fp;

#ifndef	SNMPC
    cp = "snmpd.rc";
#else
    cp = "snmpc.rc";
#endif
    if ((fp = fopen (cp, "r")) == NULL
	    && (fp = fopen (cp = isodefile (cp, 0), "r")) == NULL)
	adios (cp, "unable to read");

    if (!rflag
	    && getuid () == 0
	    && fstat (fileno (fp), &st) != NOTOK
	    && st.st_uid != 0)
	adios (NULLCP, "%s not owned by root", cp);

    while (fgets (buffer, sizeof buffer, fp)) {
	if (*buffer == '#')
	    continue;
	if (cp = index (buffer, '\n'))
	    *cp = NULL;
	(void) strcpy (line, buffer);

	bzero ((char *) vec, sizeof vec);
	if (str2vec (buffer, vec) < 1)
	    continue;
	for (p = pairs; p -> p_name; p++)
	    if (lexequ (p -> p_name, vec[0]) == 0) {
		if ((*p -> p_handler) (vec) == NOTOK)
		    advise (LLOG_EXCEPTIONS, NULLCP,
			    "malformed directive: \"%s\"", line);
		break;
	    }
	if (!p -> p_name)
	    advise (LLOG_EXCEPTIONS, NULLCP, "unknown directive: \"%s\"",
		    line);
    }

    (void) fclose (fp);
}

/*  */

static int  f_logging (vec)
char  **vec;
{
    register char  **vp;

    for (vp = ++vec; *vp; vp++)
	continue;

    log_tai (pgm_log, vec, vp - vec);

    return OK;
}

/*  */

static int  f_variable (vec)
char  **vec;
{
    if (*++vec == NULL)
	return NOTOK;

#ifndef	SNMPC
    if (lexequ (*vec, "interface") == 0) {
	char   *name;

	if ((name = *++vec) == NULL)
	    return NOTOK;
	for (vec++; *vec; vec++)
	    if (index (*vec, '='))
		set_interface (name, *vec);
	    else
		return NOTOK;

	return OK;	
    }
#endif

    if (lexequ (*vec, "snmpEnableAuthenTraps") == 0) {
	++vec;

	if (lexequ (*vec, "enabled") == 0)
	    snmpstat.s_enableauthentraps = TRAPS_ENABLED;
	else
	    if (lexequ (*vec, "disabled") == 0)
		snmpstat.s_enableauthentraps = TRAPS_DISABLED;

	return OK;
    }

    if (!vec[0] || !vec[1] || vec[2])
	return NOTOK;

    set_variable (vec[0], vec[1]);

    return OK;
}
#endif	/* SNMPT */

/*    ERRORS */

#ifndef	lint
void	adios (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _ll_log (pgm_log, LLOG_FATAL, ap);

    va_end (ap);

    _exit (1);
}
#else
/* VARARGS */

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
    va_list ap;

    va_start (ap);

    code = va_arg (ap, int);

    _ll_log (pgm_log, code, ap);

    va_end (ap);
}
#else
/* VARARGS */

void	advise (code, what, fmt)
char   *what,
       *fmt;
int	code;
{
    advise (code, what, fmt);
}
#endif
