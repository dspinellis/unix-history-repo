/* isoc.c - "minimal" ISODE client for testing */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/support/RCS/isoc.c,v 7.5 91/02/22 09:46:29 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/support/RCS/isoc.c,v 7.5 91/02/22 09:46:29 mrose Interim $
 *
 *
 * $Log:	isoc.c,v $
 * Revision 7.5  91/02/22  09:46:29  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/12/11  10:52:20  mrose
 * lock-and-load
 * 
 * Revision 7.3  90/11/11  10:53:16  mrose
 * update
 * 
 * Revision 7.2  90/10/16  12:56:05  mrose
 * stuff
 * 
 * Revision 7.1  89/12/07  21:19:51  mrose
 * stuff
 * 
 * Revision 7.0  89/11/23  22:27:15  mrose
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


#include <stdio.h>
#include <varargs.h>
#include "rosap.h"
#include "rtsap.h"
#include "acsap.h"
#include "psap2.h"
#include "ssap.h"
#include "tsap.h"
#ifdef	TCP
#include "internet.h"
#ifdef	BSD42
#include <sys/ioctl.h>
#endif
#ifdef	SVR3
#include <fcntl.h>
#endif
#endif
#include "isoservent.h"
#include "tailor.h"
#include <sys/stat.h>

#undef	TIMER
#undef	TMS
#ifdef	BSD42
#define	TIMER
#endif
#ifdef	SYS5
#define	TIMER
#ifndef	HPUX
#include <sys/times.h>
#define	TMS
#endif
#endif

#if	defined (TCP) && (defined (FIONBIO) || defined (O_NDELAY))
#define	ASYNC
#endif

/*    DATA */

#define	ISN(req) \
    (req & (SR_MINORSYNC | SR_MAJORSYNC | SR_RESYNC | SR_ACTIVITY)) \
	? (long) ((getpid () % (SERIAL_MAX - SERIAL_MIN + 1)) + SERIAL_MIN) \
	: SERIAL_NONE

static enum { echo, sink, XXX } mode = XXX;

static int   testing_queued_writes = 0;

static char *isacs = NULL;
static int   isrts = 0;

static int   status = 0;

static char *myname = "isoc";


void	adios (), advise ();
void	ts_adios (), ts_advise ();
void	ss_adios (), ss_advise ();
void	ps_adios (), ps_advise ();
void	acs_adios (), acs_advise ();
void	rts_adios (), rts_advise ();
void	ros_adios (), ros_advise ();


long	lseek ();

/*    MAIN */

#define	chkacs()	if (isacs) \
			    adios (NULLCP, "no association control for %s", \
					argv[2])

/* ARGSUSED */

main (argc, argv, envp)
int     argc;
char  **argv,
      **envp;
{
    register struct isoservent *is;

    if (myname = rindex (argv[0], '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = argv[0];

    isodetailor (myname, 1);

    if (argc != 4)
	adios (NULLCP, "usage: %s host provider entity", myname);

#ifdef	TCP
    if (strcmp (argv[2], "raw") == 0) {
	raw_main (argv[3], argv[1]);
	exit (0);
    }
#endif

    if (index (argv[3], '/')) {
	mode = strcmp (isacs = argv[3], "isode/sink") ? echo : sink;
    }
    else {
	if ((is = getisoserventbyname (argv[3], argv[2])) == NULL)
	    adios (NULLCP, "%s/%s: unknown provider/entity pair",
		    argv[2], argv[3]);

	mode = strcmp (is -> is_entity, "sink") ? echo : sink;
    }

    if (strcmp (argv[2], "tsap") == 0) {
	chkacs ();
	ts_main (is, argv[1]);
    }
    else
	if (strcmp (argv[2], "ssap") == 0) {
	    chkacs ();
	    ss_main (is, argv[1]);
	}
	else
	    if (strcmp (argv[2], "psap") == 0)
		ps_main (is, argv[1]);
	    else
		if (strcmp (argv[2], "rtsap") == 0) {
		    isrts = 1;
		    rts_main (is, argv[1]);
		}
		else
		    if (strcmp (argv[2], "rosap") == 0)
			ros_main (is, argv[1]);
		    else
			adios (NULLCP, "unknown provider: \"%s\"", argv[2]);

    exit (status);		/* NOTREACHED */
}

/*    RAW */

#ifdef	TCP
static int  raw_main (service, addr)
char   *service,
       *addr;
{
    int     sd,
            cc,
            i,
            j;
    char   *cp,
           *dp;
    register struct hostent *hp;
    register struct servent *sp;
    struct sockaddr_in  in_socket;
    register struct sockaddr_in *isock = &in_socket;
    struct stat st;

    if (strcmp (service, "sink"))
	adios (NULLCP, "only sink on raw tcp is supported");
    if ((sp = getservbyname (service, "tcp")) == NULL)
	adios (NULLCP, "%s/%s: unknown service", "tcp", service);
    if ((hp = gethostbystring (addr)) == NULL)
	adios (NULLCP, "%s: unknown host", addr);

    bzero ((char *) isock, sizeof *isock);
    isock -> sin_family = hp -> h_addrtype;
    isock -> sin_port = sp -> s_port;
    inaddr_copy (hp, isock);

    if ((sd = start_tcp_client ((struct sockaddr_in *) 0, 0)) == NOTOK)
	adios ("socket", "unable to start");
    fprintf (stderr, "%s... ", hp -> h_name);
    (void) fflush (stderr);
    if (join_tcp_server (sd, isock) == NOTOK) {
	fprintf (stderr, "failed\n");
	adios ("socket", "unable to connect");
    }
    fprintf (stderr, "connected\n");

    if (fstat (fileno (stdin), &st) == NOTOK
	    || (st.st_mode & S_IFMT) != S_IFREG
	    || (cc = st.st_size) == 0)
	adios (NULLCP, "standard input not a regular file");
    (void) lseek (fileno (stdin), 0L, 0);

    if ((cp = malloc ((unsigned) cc)) == NULL)
	adios (NULLCP, "no memory");
    for (dp = cp, j = cc; j > 0; dp += i, j -= i)
	switch (i = read (fileno (stdin), dp, j)) {
	    case NOTOK: 
		adios ("on stdin", "read failed");

	    case OK: 
		adios (NULLCP, "premature end-of-file");

	    default: 
		break;
	}

#ifdef	TIMER
    timer (0);
#endif
    if (write_tcp_socket (sd, cp, cc) != cc)
	adios ("writing", "error");
    (void) close_tcp_socket (sd);
#ifdef	TIMER
    timer (cc);
#endif
}
#endif

/*    TSAP */

static int  ts_main (is, addr)
struct isoservent *is;
char   *addr;
{
    int     sd,
	    cc,
	    i,
	    j,
	    expedited,
	    expd;
    char   *cp,
	   *dp,
	    buffer[BUFSIZ];
    struct  TSAPaddr *ta;
    struct TSAPconnect  tcs;
    register struct TSAPconnect *tc = &tcs;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect *td = &tds;
    struct stat st;

    if ((ta = is2taddr (addr, NULLCP, is)) == NULL)
	adios (NULLCP, "address translation failed");

    fprintf (stderr, "%s... ", addr);
    (void) fflush (stderr);
#ifndef	ASYNC
    if (TConnRequest (NULLTA, ta, 1, NULLCP, 0, NULLQOS, tc, td) == NOTOK) {
	fprintf (stderr, "failed\n");
	ts_adios (td, "T-CONNECT.REQUEST");
    }
    sd = tc -> tc_sd;
#else
    if ((i = TAsynConnRequest (NULLTA, ta, 1, NULLCP, 0, NULLQOS, tc, td, 1))
	    == NOTOK) {
	fprintf (stderr, "failed\n");
	ts_adios (td, "T-(ASYN-)CONNECT.REQUEST");
    }
    sd = tc -> tc_sd, cc = 0;
    while (i == CONNECTING_1 || i == CONNECTING_2) {
	int	nfds;
	fd_set	mask,
	       *rmask,
	       *wmask;

	nfds = 0;
	FD_ZERO (&mask);
	if (TSelectMask (sd, &mask, &nfds, td) == NOTOK) {
	    fprintf (stderr, "failed\n");
	    ts_adios (td, "T-(ASYN-)CONNECT.REQUEST(TSelectMask)");
	}
	rmask = (i == CONNECTING_2) ? &mask : NULLFD;
	wmask = (i == CONNECTING_2) ? NULLFD : &mask;

	fprintf (stderr, ".");
	(void) fflush (stderr);
	if (xselect (nfds, rmask, wmask, NULLFD, 1) == NOTOK) {
	    fprintf (stderr, "failed\n");
	    adios ("failed", "select");
	}

	if ((rmask && FD_ISSET (sd, rmask) == 0)
	        || (wmask && FD_ISSET (sd, wmask) == 0))
	    continue;
	    
	if ((i = TAsynRetryRequest (sd, tc, td)) == NOTOK) {
	    fprintf (stderr, "failed\n");
	    ts_adios (td, "T-ASYN-RETRY.REQUEST");
	}
    }
#endif
    fprintf (stderr, "connected\n");


    if (getenv ("TEST_QUEUED_WRITES")) {
	if (TSetQueuesOK (tc -> tc_sd, 1, td) == NOTOK)
	    ts_adios (td, "T-SET-QUEUES-OK");

	tsap_log -> ll_events |= LLOG_EXCEPTIONS;
	tsap_log -> ll_file = "-";
	(void) ll_close (tsap_log);

	testing_queued_writes = 1;
    }

    expd = tc -> tc_expedited;
#ifdef	DEBUG
    {
	advise (NULLCP, "responding TSAP address: %s",
		taddr2str (&tc -> tc_responding));

	if (tc -> tc_cc > 0)
	    advise (NULLCP, "greetings: %d octets", tc -> tc_cc);
    }
#endif

    if (fstat (fileno (stdin), &st) != NOTOK
	    && (st.st_mode & S_IFMT) == S_IFREG
	    && (cc = st.st_size) != 0) {
	(void) lseek (fileno (stdin), 0L, 0);

	if ((cp = malloc ((unsigned) cc)) == NULL)
	    adios (NULLCP, "no memory");
	for (dp = cp, j = cc; j > 0; dp += i, j -= i)
	    switch (i = read (fileno (stdin), dp, j)) {
		case NOTOK:
		    adios ("on stdin", "read failed");

		case OK:
		    adios (NULLCP, "premature end-of-file");
		    
		default:
		    break;
	    }
	for (i = 10; i > 0; i--) {
#ifdef	TIMER
	    timer (0);
#endif
	    ts_datarequest (sd, cp, cc, 0);
#ifdef	TIMER
	    timer (cc);
#endif
	}
	free (cp);
    }
    else
	for (expedited = 0;
		fgets (buffer, sizeof buffer, stdin);
		expedited = !expedited) {
	    if ((cc = strlen (buffer) + 1) > TX_SIZE && expedited)
		expedited = 0;

	    ts_datarequest (sd, buffer, cc, expd ? expedited : 0);
	}

    if (TDiscRequest (sd, NULLCP, 0, td) == NOTOK)
	ts_adios (td, "T-DISCONNECT.REQUEST");
}

/*  */

static int  ts_datarequest (sd, data, cc, expedited)
int	sd;
char   *data;
int	cc,
	expedited;
{
    struct TSAPdata txs;
    register struct TSAPdata   *tx = &txs;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect *td = &tds;

    if ((expedited ? TExpdRequest (sd, data, cc, td)
		: TDataRequest (sd, data, cc, td)) == NOTOK)
	if (expedited)
	    ts_adios (td, "T-EXPEDITED-DATA.REQUEST");
	else
	    ts_adios (td, "T-DATA.REQUEST");

    if (mode == echo) {
	if (testing_queued_writes) {
	    int	    vecp;
	    char   *vec[4];
	    fd_set  rfds;

	    FD_ZERO (&rfds);
	    FD_SET (sd, &rfds);
	    if (TNetAccept (&vecp, vec, sd + 1, &rfds, NULLFD, NULLFD, NOTOK,
			    td) == NOTOK)
		ts_adios (td, "T-NET-ACCEPT");
	}

	if (TReadRequest (sd, tx, NOTOK, td) == NOTOK)
	    ts_adios (td, "T-READ.REQUEST");
	if (cc != tx -> tx_cc) {
	    advise (NULLCP, "length mismatch, orig=%d echo=%d",
		    cc, tx -> tx_cc);
	    status++;
	}
	else
	    if (qcmp (data, &tx -> tx_qbuf, cc))
		status++;
	TXFREE (tx)
    }
}

/*  */

static void  ts_adios (td, event)
register struct TSAPdisconnect *td;
char   *event;
{
    ts_advise (td, event);

    _exit (1);
}


static void  ts_advise (td, event)
register struct TSAPdisconnect *td;
char   *event;
{
    char    data[BUFSIZ];

    if (td -> td_cc > 0) {
	(void) sprintf (data, "[%s] %*.*s",
		TErrString (td -> td_reason),
		td -> td_cc, td -> td_cc, td -> td_data);
    }
    else
	(void) sprintf (data, "[%s]", TErrString (td -> td_reason));

    advise (NULLCP, "%s: %s", event, data);
}

/*    SSAP */

static int requirements = SR_HALFDUPLEX | SR_DUPLEX | SR_EXPEDITED
		| SR_MINORSYNC | SR_MAJORSYNC | SR_RESYNC | SR_ACTIVITY
		| SR_NEGOTIATED | SR_CAPABILITY | SR_EXCEPTIONS | SR_TYPEDATA;

static int owned = 0;
static int avail = 0;

static long ssn;

static int nmodes;
static int datamodes[4];

static char userdata[1024];

/*  */

static int  ss_main (is, addr)
struct isoservent *is;
char   *addr;
{
    int     sd,
	    cc,
	    i,
	    j,
	    k,
	    l,
	    tokens;
    char   *cp,
	   *dp,
	    buffer[BUFSIZ];
    struct SSAPactid    ids;
    register struct SSAPactid  *id = &ids;
    register struct SSAPaddr *sz;
    struct SSAPref  sfs;
    register struct SSAPref *sf;
    struct SSAPconnect  scs;
    register struct SSAPconnect *sc = &scs;
    struct SSAPrelease  srs;
    register struct SSAPrelease *sr = &srs;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;
    struct stat st;

    bzero (userdata, sizeof userdata);

    if ((sz = is2saddr (addr, NULLCP, is)) == NULL)
	adios (NULLCP, "address translation failed");
    if ((sf = addr2ref (SLocalHostName ())) == NULL) {
	sf = &sfs;
	(void) bzero ((char *) sf, sizeof *sf);
    }

    tokens = 0;
#define dotoken(requires,shift,bit,type) \
{ \
    if (requirements & requires) \
	tokens |= ST_CALL_VALUE << shift; \
}
    dotokens ();
#undef	dotoken

    fprintf (stderr, "%s... ", addr);
    (void) fflush (stderr);
#ifndef	ASYNC
    if (SConnRequest (sf, NULLSA, sz, requirements, tokens, ISN (requirements),
	    userdata, sizeof userdata /*SS_SIZE*/, NULLQOS, sc, si) == NOTOK) {
	fprintf (stderr, "failed\n");
	ss_adios (sa, "S-CONNECT.REQUEST");
    }
    sd = sc -> sc_sd;
#else
    if ((i = SAsynConnRequest (sf, NULLSA, sz, requirements, tokens,
	    ISN (requirements), userdata, sizeof userdata /*SS_SIZE*/, NULLQOS,
	    sc, si, 1))
	  == NOTOK) {
	fprintf (stderr, "failed\n");
	ss_adios (sa, "S-(ASYN-)CONNECT.REQUEST");
    }
    sd = sc -> sc_sd, cc = 0;
    while (i == CONNECTING_1 || i == CONNECTING_2) {
	int	nfds;
	fd_set	mask,
	       *rmask,
	       *wmask;

	nfds = 0;
	FD_ZERO (&mask);
	if (SSelectMask (sd, &mask, &nfds, si) == NOTOK) {
	    fprintf (stderr, "failed\n");
	    ss_adios (sa, "S-(ASYN-)CONNECT.REQUEST(SSelectMask)");
	}
	rmask = (i == CONNECTING_2) ? &mask : NULLFD;
	wmask = (i == CONNECTING_2) ? NULLFD : &mask;

	fprintf (stderr, ".");
	if (xselect (nfds, rmask, wmask, NULLFD, 1) == NOTOK) {
	    fprintf (stderr, "failed\n");
	    adios ("failed", "select");
	}

	if ((rmask && FD_ISSET (sd, rmask) == 0)
	        || (wmask && FD_ISSET (sd, wmask) == 0))
	    continue;
	    
	if ((i = SAsynRetryRequest (sd, sc, si)) == NOTOK) {
	    fprintf (stderr, "failed\n");
	    ss_adios (sa, "S-ASYN-RETRY.REQUEST");
	}
    }
#endif

    if (sc -> sc_result != SC_ACCEPT) {
	fprintf (stderr, "failed\n");
	if (sc -> sc_cc > 0)
	    adios (NULLCP, "connection rejected: [%s] %*.*s",
			SErrString (sc -> sc_result),
			sc -> sc_cc, sc -> sc_cc, sc -> sc_data);
	else
	    adios (NULLCP, "connection rejected: [%s]",
			SErrString (sc -> sc_result));
    }
    fprintf (stderr, "connected\n");

#ifdef	DEBUG
    {
	advise (NULLCP, "responding SSAP address: %s",
		saddr2str (&sc -> sc_responding));

	if (sc -> sc_cc > 0)
	    advise (NULLCP, "greetings: %d octets", sc -> sc_cc);
    }
#endif
    requirements = sc -> sc_requirements;
    nmodes = 0;
    datamodes[nmodes++] = SX_NORMAL;
    if (requirements & SR_EXPEDITED)
	datamodes[nmodes++] = SX_EXPEDITED;
    if ((requirements & SR_CAPABILITY) && (requirements & SR_ACTIVITY))
	datamodes[nmodes++] = SX_CAPDIND;
    if (requirements & SR_TYPEDATA)
	datamodes[nmodes++] = SX_TYPED;

#define dotoken(requires,shift,bit,type) \
{ \
    if (requirements & requires) \
	switch (sc -> sc_settings & (ST_MASK << shift)) { \
	    case ST_CALL_VALUE: \
		adios (NULLCP, "%s token: choice", type); \
 \
	    case ST_INIT_VALUE: \
		owned |= bit, avail |= bit; \
		break; \
 \
	    case ST_RESP_VALUE: \
		avail |= bit; \
		break; \
 \
	    default: \
		adios (NULLCP, "%s token: reserved", type); \
	} \
}
	dotokens ();
#undef	dotoken

    if (requirements & SR_ACTIVITY) {
	(void) strcpy (id -> sd_data, mode == echo ? "echo" : "sink");
	id -> sd_len = strlen (id -> sd_data);
	if (SActStartRequest (sd, id, userdata, SV_SIZE, si) == NOTOK)
	    ss_adios (sa, "S-ACTIVITY-START.REQUEST");
    }

    if (fstat (fileno (stdin), &st) != NOTOK
	    && (st.st_mode & S_IFMT) == S_IFREG
	    && (cc = st.st_size) != 0) {
	(void) lseek (fileno (stdin), 0L, 0);

	if ((cp = malloc ((unsigned) cc)) == NULL)
	    adios (NULLCP, "no memory");
	for (dp = cp, j = cc; j > 0; dp += i, j -= i)
	    switch (i = read (fileno (stdin), dp, j)) {
		case NOTOK:
		    adios ("on stdin", "read failed");

		case OK:
		    adios (NULLCP, "premature end-of-file");
		    
		default:
		    break;
	    }
	for (i = 10; i > 0; i--) {
#ifdef	TIMER
	    timer (0);
#endif
	    ss_datarequest (sd, cp, cc, SX_NORMAL, 0);
#ifdef	TIMER
	    timer (cc);
#endif
	}
	free (cp);
    }
    else {
	for (j = l = 0; fgets (buffer, sizeof buffer, stdin); ) {
	    k = j >= nmodes ? SX_EXPEDITED : datamodes[j++ % nmodes];
	    if ((cc = strlen (buffer) + 1) > SX_EXSIZE && k == SX_EXPEDITED) {
		if ((k = datamodes[j++ % nmodes]) == SX_EXPEDITED)
		    k = datamodes[j++ % nmodes];
	    }

	    switch (k) {
		case SX_CAPDIND:
		    if (!(requirements & SR_RESYNC) || l++ & 0x01) {
			ss_waitfor (sd, ST_ACT_TOKEN);
			if (l & 0x03) {
			    if (SActIntrRequest (sd, SP_SEQUENCE, si)
				    == NOTOK)
				ss_adios (sa, "S-ACTIVITY-INTERRUPT.REQUEST");
			}
			else {
			    if (SActDiscRequest (sd, SP_SEQUENCE, si)
				    == NOTOK)
				ss_adios (sa, "S-ACTIVITY-DISCARD.REQUEST");
			}
			ss_waitfor (sd, -1);
			goto push_data;
		    }
		    if (!(requirements & SR_RESYNC))
			break;
		    tokens = 0;
#define dotoken(requires,shift,bit,type) \
{ \
		    if (requirements & requires) \
			tokens |= ST_CALL_VALUE << shift; \
}
		    dotokens ();
#undef	dotoken
		    if (SReSyncRequest (sd, SYNC_SET, ssn - 1, tokens,
				userdata, SN_SIZE, si) == NOTOK)
			ss_adios (sa, "S-RESYNCHRONIZE.REQUEST");
		    ss_waitfor (sd, -1);
		    break;

		case SX_EXPEDITED:
		    if (j >= nmodes)
			j = j % nmodes;/* fall... */
		    if (!(requirements & SR_EXPEDITED))
			k = SX_NORMAL;	/* fall... */
		default:
push_data: ;
		    ss_datarequest (sd, buffer, cc, k, 1);
		    if (k == SX_CAPDIND
			    && SActResumeRequest (sd, id, id, 
				    (long) (getpid () % (SERIAL_MAX - SERIAL_MIN + 1))
				    + SERIAL_MIN, sf, userdata, SV_SIZE, si)
				  == NOTOK)
			ss_adios (sa, "S-ACTIVITY-RESUME.REQUEST");
		    break;
	    }
	}

	if (requirements & SR_EXCEPTIONS) {
	    if (owned & ST_DAT_TOKEN)
		if (SGTokenRequest (sd, ST_DAT_TOKEN, si) == NOTOK)
		    ss_adios (sa, "S-TOKEN-GIVE.REQUEST");
		else
		    owned &= ~ST_DAT_TOKEN;
	    if (SUReportRequest (sd, SP_NOREASON, userdata, SP_SIZE, si)
		    == NOTOK)
		ss_adios (sa, "S-U-EXCEPTION-REPORT.REQUEST");
	    ss_waitfor (sd, -1);
	}    
    }

    if ((requirements & SR_MAJORSYNC) && !(requirements & SR_ACTIVITY)) {
	if (SMajSyncRequest (sd, &ssn, userdata, SN_SIZE, si) == NOTOK)
	    switch (sa -> sa_reason) {
		case SC_OPERATION:
		    ss_waitfor (sd, ST_DAT_TOKEN | ST_MIN_TOKEN
			    | ST_MAJ_TOKEN);
		    if (SMajSyncRequest (sd, &ssn, userdata, SN_SIZE, si)
			    == OK)
			break;	/* else fall */

		default:
		    ss_adios (sa, "S-MAJOR-SYNC.REQUEST");
	    }

	ss_waitfor (sd, -1);
    }

    if (requirements & SR_ACTIVITY) {
	if (SActEndRequest (sd, &ssn, userdata, SV_SIZE, si) == NOTOK)
	    switch (sa -> sa_reason) {
		case SC_OPERATION:
		    ss_waitfor (sd, avail);
		    if (SActEndRequest (sd, &ssn, userdata, SV_SIZE, si) == OK)
			break;	/* else fall */

		default:
		    ss_adios (sa, "S-ACTIVITY-END.REQUEST");
	    }

	ss_waitfor (sd, -1);
		
	if (SGControlRequest (sd, si) == NOTOK)
	    switch (sa -> sa_reason) {
		case SC_OPERATION:
		    ss_waitfor (sd, avail);
		    if (SGControlRequest (sd, si) == OK)
			break;	/* else fall */

		default:
		    ss_adios (sa, "S-CONTROL-GIVE.REQUEST");
	    }

	owned = 0;

	ss_waitfor (sd, -1);
    }

    if (SRelRequest (sd, userdata, SF_SIZE, NOTOK, sr, si) == NOTOK)
	switch (sa -> sa_reason) {
	    case SC_OPERATION: 
	    case SC_WAITING: 
		ss_waitfor (sd, avail);
		if (SRelRequest (sd, userdata, SF_SIZE, NOTOK, sr, si) == OK)
		    break;	/* else fall */

	    default: 
		ss_adios (sa, "S-RELEASE.REQUEST");
	}

	if (!sr -> sr_affirmative) {
	    (void) SUAbortRequest (sd, NULLCP, 0, si);

	    if (sr -> sr_cc > 0)
		adios (NULLCP, "release rejected by peer: %*.*s",
			sr -> sr_cc, sr -> sr_cc, sr -> sr_data);
	    else
		adios (NULLCP, "release rejected by peer");
	}
	SRFREE (sr);
}

/*  */

static int ss_datarequest (sd, data, cc, dm, sync)
int	sd;
char   *data;
int	cc,
	dm,
	sync;
{
    int     result;
    struct SSAPdata sxs;
    register struct SSAPdata   *sx = &sxs;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;

    switch (dm) {
	default: 
	    if (SDataRequest (sd, data, cc, si) == NOTOK)
		switch (sa -> sa_reason) {
		    case SC_OPERATION: 
			ss_waitfor (sd, ST_DAT_TOKEN);
			if (SDataRequest (sd, data, cc, si) == OK)
			    break;/* else fall */

		    default: 
			ss_adios (sa, "S-DATA.REQUEST");
		}
	    break;

	case SX_EXPEDITED: 
	    if (SExpdRequest (sd, data, cc, si) == NOTOK)
		ss_adios (sa, "S-EXPEDITED-DATA.REQUEST");
	    break;

	case SX_CAPDIND: 
	    if (SCapdRequest (sd, data, cc, si) == NOTOK)
		switch (sa -> sa_reason) {
		    case SC_OPERATION: 
			ss_waitfor (sd, avail & ~ST_RLS_TOKEN);
			if (SCapdRequest (sd, data, cc, si) == OK)
			    break;/* else fall */

		    default: 
			ss_adios (sa, "S-CAPABILITY-DATA.REQUEST");
		}
	    break;

	case SX_TYPED: 
	    if (STypedRequest (sd, data, cc, si) == NOTOK)
		ss_adios (sa, "S-TYPED-DATA.REQUEST");
	    break;
    }

    if (mode == echo || dm == SX_CAPDIND)
	for (;;) {
	    switch (result = SReadRequest (sd, sx, NOTOK, si)) {
		case NOTOK: 
		    ss_adios (sa, "S-READ.REQUEST");

		case OK: 
		    if ((dm != SX_CAPDIND ? dm : SX_CAPDCNF)
			    != sx -> sx_type) {
			advise (NULLCP,
				"data indication type mismatch, orig=%d echo=%d",
				dm, sx -> sx_type);
			status++;
		    }
		    if (cc != sx -> sx_cc) {
			advise (NULLCP, "length mismatch, orig=%d echo=%d",
				cc, sx -> sx_cc);
			status++;
		    }
		    else
			if (qcmp (data, &sx -> sx_qbuf, cc))
			    status++;
		    SXFREE (sx)
		    break;

		case DONE: 
		    ss_event (sd, si);
		    continue;

		default: 
		    adios (NULLCP, "unknown return from SReadRequest=%d",
			    result);
	    }
	    break;
	}

    if (sync &&
	    (requirements & SR_MINORSYNC) && !(requirements & SR_ACTIVITY)) {
	if (SMinSyncRequest (sd, SYNC_CONFIRM, &ssn, userdata, SN_SIZE, si)
		== NOTOK)
	    switch (sa -> sa_reason) {
		case SC_OPERATION: 
		    ss_waitfor (sd, ST_DAT_TOKEN | ST_MIN_TOKEN);
		    if (SMinSyncRequest (sd, SYNC_CONFIRM, &ssn, userdata,
			    SN_SIZE, si) == OK)
			break;	/* else fall */

		default: 
		    ss_adios (sa, "S-MINOR-SYNC.REQUEST");
	    }

	ss_waitfor (sd, -1);
    }
    else
	if (sync
		&& (requirements & SR_ACTIVITY)
		&& (requirements & SR_MAJORSYNC)
		&& dm == SX_NORMAL) {
	    if (SMajSyncRequest (sd, &ssn, userdata, SN_SIZE, si) == NOTOK)
		switch (sa -> sa_reason) {
		    case SC_OPERATION: 
			ss_waitfor (sd, ST_DAT_TOKEN | ST_MIN_TOKEN
				| ST_MAJ_TOKEN);
			if (SMajSyncRequest (sd, &ssn, userdata, SN_SIZE, si)
				== OK)
			    break;/* else fall */

		    default: 
			ss_adios (sa, "S-MAJOR-SYNC.REQUEST");
		}

	    ss_waitfor (sd, -1);
	}
}

/*  */

static int  ss_waitfor (sd, want)
int	sd,
	want;
{
    int     result,
	    tokens;
    char    buffer[BUFSIZ];
    struct SSAPdata sxs;
    register struct SSAPdata   *sx = &sxs;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;

    for (;;) {
	if (want == -1) {
	    want = avail;
	    goto read_it;
	}

	tokens = 0;
#define dotoken(requires,shift,bit,type) \
{ \
	if ((want & bit) && !(owned & bit)) \
	    tokens |= bit; \
}
	dotokens ();
#undef	dotoken
	if (tokens == 0)
	    return;

	if (SPTokenRequest (sd, tokens, userdata, ST_SIZE, si) == NOTOK)
	    ss_adios (sa, "S-TOKEN-PLEASE.REQUEST");

read_it: ;
	switch (result = SReadRequest (sd, sx, NOTOK, si)) {
	    case NOTOK: 
		ss_adios (sa, "S-READ.REQUEST");

	    case OK: 
		(void) strcpy (buffer, "protocol screw-up");
		if (SUAbortRequest (sd, buffer, strlen (buffer) + 1, si) == NOTOK)
		    ss_adios (sa, "S-U-ABORT.REQUEST");
		adios (NULLCP, "%s, data indication type=0x%x",
			buffer, sx -> sx_type);

	    case DONE: 
		ss_event (sd, si);
		break;

	    default: 
		adios (NULLCP, "unknown return from SReadRequest=%d",
			result);
	}
    }
}

/*  */

static	ss_event (sd, si)
int	sd;
register struct SSAPindication *si;
{
    register struct SSAPabort  *sa = &si -> si_abort;
    register struct SSAPactivity *sv = &si -> si_activity;
    register struct SSAPfinish *sf = &si -> si_finish;
    register struct SSAPreport *sp = &si -> si_report;
    register struct SSAPsync   *sn = &si -> si_sync;
    register struct SSAPtoken  *st = &si -> si_token;

    switch (si -> si_type) {
	case SI_TOKEN: 
	    switch (st -> st_type) {
		case ST_GIVE: 
		case ST_CONTROL: 
		    owned = st -> st_owned;
		    break;

		case ST_PLEASE: 
		    if (SGTokenRequest (sd,
				(int) st -> st_tokens, si)
			    == NOTOK)
			ss_adios (sa, "S-TOKEN-GIVE.REQUEST");
		    else
			owned &= ~st -> st_tokens;
		    break;

		default: 
		    adios (NULLCP,
			    "unknown token indication type=0x%x, %d bytes",
			    st -> st_type, st -> st_cc);
	    }
	    STFREE (st);
	    break;

	case SI_SYNC: 
	    switch (sn -> sn_type) {
		case SN_MAJORIND: 
		    adios (NULLCP, "majorsync indication %d, %d bytes",
			    sn -> sn_ssn, sn -> sn_cc);
		    break;

		case SN_MAJORCNF: 
		    break;

		case SN_MINORIND: 
		    adios (NULLCP, "minorsync indication %d%s, %d bytes",
			    sn -> sn_ssn, sn -> sn_options == SYNC_CONFIRM
			    ? " (wants confirmation)" : NULLCP, sn -> sn_cc);
		    break;

		case SN_MINORCNF: 
		    break;

		case SN_RESETIND: 
#define	dotoken(requires,shift,bit,type) \
{ \
		    if (requirements & requires) \
			switch (sn -> sn_settings & (ST_MASK << shift)) { \
			    case ST_CALL_VALUE << shift: \
				sn -> sn_settings &= ~(ST_MASK << shift); \
				sn -> sn_settings |= ST_RESP_VALUE << shift; \
			    case ST_RESP_VALUE << shift: \
				owned &= ~bit; \
				break; \
 \
			    case ST_INIT_VALUE << shift: \
				owned |= bit; \
				break; \
 \
			    default: \
				adios (NULLCP, "%s token: reserved", type); \
				break; \
			} \
}
		    dotokens ();
#undef	dotoken
		    if (SReSyncResponse (sd, sn -> sn_ssn, sn -> sn_settings,
				userdata, SN_SIZE, si) == NOTOK)
			ss_adios (sa, "S-RESYNCHRONIZE.RESPONSE");
		    break;

		case SN_RESETCNF: 
		    break;

		default: 
		    adios (NULLCP,
			    "unknown sync indication=0x%x, ssn=%d, %d bytes",
			    sn -> sn_type, sn -> sn_ssn, sn -> sn_cc);
	    }
	    SNFREE (sn);
	    break;

	case SI_ACTIVITY: 
	    switch (sv -> sv_type) {
		case SV_START: 
		    adios (NULLCP,
			    "activity start indication: %*.*s, %d bytes",
			    sv -> sv_id.sd_len, sv -> sv_id.sd_len,
			    sv -> sv_id.sd_data, sv -> sv_cc);

		case SV_RESUME: 
		    adios (NULLCP, 
			    "activity resume indication: id=%*.*s oid=%*.*s connect=%s ssn=%d, %d bytes",
			    sv -> sv_id.sd_len, sv -> sv_id.sd_len,
			    sv -> sv_id.sd_data, sv -> sv_oid.sd_len,
			    sv -> sv_oid.sd_len, sv -> sv_oid.sd_data,
			    sprintref (&sv -> sv_connect), sv -> sv_ssn,
			    sv -> sv_cc);

		case SV_INTRIND: 
		    adios (NULLCP,
			    "activity interrupt indication %d, %d bytes",
			    sv -> sv_reason, sv -> sv_cc);

		case SV_INTRCNF: 
		    break;

		case SV_DISCIND: 
		    adios (NULLCP,
			    "activity discard indication %d, %d bytes",
			    sv -> sv_reason, sv -> sv_cc);

		case SV_DISCCNF: 
		    break;

		case SV_ENDIND: 
		    adios (NULLCP, "activity end indication %d, %d bytes",
			    sv -> sv_ssn, sv -> sv_cc);

		case SV_ENDCNF: 
		    break;

		default: 
		    adios (NULLCP,
			    "unknown activity indication=0x%x, %d bytes",
			    sv -> sv_type, sv -> sv_cc);
	    }
	    SVFREE (sv);
	    break;

	case SI_REPORT: 
	    if (requirements & SR_DAT_EXISTS) {
		if (SGTokenRequest (sd, ST_DAT_TOKEN, si) == NOTOK)
		    ss_adios (sa, "S-TOKEN-GIVE.REQUEST (to clear exception)");
		else
		    owned &= ~ST_DAT_TOKEN;
	    }
	    else
		if (SUAbortRequest (sd, NULLCP, 0, si) == NOTOK)
		    ss_adios (sa, "S-U-ABORT.REQUEST");
		else
		    adios (NULLCP, "aborted");
	    SPFREE (sp);
	    break;

	case SI_FINISH: 
	    if (SRelResponse (sd, SC_REJECTED, NULLCP, 0, si) == NOTOK)
		ss_adios (sa, "S-RELEASE.RESPONSE");
	    SFFREE (sf);
	    break;

	default: 
	    adios (NULLCP, "unknown indication type=0x%x",
		    si -> si_type);
    }
}

/*  */

static void  ss_adios (sa, event)
register struct SSAPabort *sa;
char   *event;
{
    ss_advise (sa, event);

    _exit (1);
}


static void  ss_advise (sa, event)
register struct SSAPabort *sa;
char   *event;
{
    char    buffer[BUFSIZ];

    if (sa -> sa_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s",
		SErrString (sa -> sa_reason),
		sa -> sa_cc, sa -> sa_cc, sa -> sa_data);
    else
	(void) sprintf (buffer, "[%s]", SErrString (sa -> sa_reason));

    advise (NULLCP, "%s: %s%s", event, buffer,
	    sa -> sa_peer ? " (peer initiated)" : "");

    SAFREE (sa);
}

/*    PSAP */

static int prequirements = 0;
#define	srequirements requirements

static int nctxs;
static int datactxs[NPCTX];

/*  */

static int  ps_main (is, addr)
struct isoservent *is;
char   *addr;
{
    int     sd,
	    cc,
	    i,
	    j,
	    k,
	    l,
	    m,
	    tokens;
    char   *cp,
	   *dp,
	    buffer[BUFSIZ];
    register struct PSAPaddr   *pz;
    struct SSAPactid    ids;
    register struct SSAPactid  *id = &ids;
    struct SSAPref  sfs;
    register struct SSAPref *sf;
    struct PSAPconnect  pcs;
    register struct PSAPconnect *pc = &pcs;
    struct PSAPctxlist pls;
    register struct PSAPctxlist *pl = &pls;
    struct PSAPrelease  prs;
    register struct PSAPrelease *pr = &prs;
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort  *pa = &pi -> pi_abort;
    struct AcSAPconnect  accs;
    register struct AcSAPconnect *acc = &accs;
    struct AcSAPrelease acrs;
    register struct AcSAPrelease *acr = &acrs;
    struct AcSAPindication   acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort  *aca = &aci -> aci_abort;
    register PE	    pe;
    PE	    udata[NPDATA];
    AEI	    aei;
    OID	    oid,
	    ode;
    struct stat st;

    if (isacs) {
	if ((aei = str2aei (addr, isacs)) == NULLAEI)
	    adios (NULLCP, "%s-%s: unknown application-entity", addr, isacs);
	if ((pz = aei2addr (aei)) == NULLPA)
	    adios (NULLCP, "address translation failed");

	cp = mode == echo ? "isode echo pci" : "isode sink pci";
	if ((ode = ode2oid (cp)) == NULLOID)
	    adios (NULLCP, "%s: unknown object descriptor", cp);
	ode = oid_cpy (ode);
    }
    else
	if ((pz = is2paddr (addr, NULLCP, is)) == NULL)
	    adios (NULLCP, "address translation failed");
    if ((sf = addr2ref (PLocalHostName ())) == NULL) {
	sf = &sfs;
	(void) bzero ((char *) sf, sizeof *sf);
    }

    tokens = 0;
#define dotoken(requires,shift,bit,type) \
{ \
    if (srequirements & requires) \
	tokens |= ST_CALL_VALUE << shift; \
}
    dotokens ();
#undef	dotoken

    for (i = (pl -> pc_nctx = NPCTX - (isacs ? 1 : 0)) - 1; i >= 0; i--) {
	pl -> pc_ctx[i].pc_id = i * 2 + 1;
	if ((oid = ode2oid ("iso asn.1 abstract syntax")) == NULLOID)
	    adios (NULLCP, "iso asn.1 abstract syntax: unknown");
	pl -> pc_ctx[i].pc_asn = oid_cpy (oid);
	pl -> pc_ctx[i].pc_atn = NULLOID;
    }

    for (i = 0; i < NPDATA; i++) {
	if ((pe = int2prim (i)) == NULLPE)
	    adios (NULLCP, "unable to allocate hello");
	pe -> pe_context = (i % pl -> pc_nctx) * 2 + 1;
	udata[i] = pe;
    }

    fprintf (stderr, "%s... ", addr);
    (void) fflush (stderr);
    if (isacs) {
#ifndef	ASYNC
	if (AcAssocRequest (ode, NULLAEI, aei, NULLPA, pz, pl, ode,
		prequirements, srequirements, ISN (srequirements), tokens, sf,
		udata, NACDATA, NULLQOS, acc, aci)
		== NOTOK) {
	    fprintf (stderr, "failed\n");
	    acs_adios (aca, "A-ASSOCIATE.REQUEST");
	}
	sd = acc -> acc_sd;
#else
	if ((i = AcAsynAssocRequest (ode, NULLAEI, aei, NULLPA, pz, pl, ode,
		prequirements, srequirements, ISN (srequirements), tokens, sf,
		udata, NACDATA, NULLQOS, acc, aci, 1)) == NOTOK) {
	    fprintf (stderr, "failed\n");
	    acs_adios (aca, "A-(ASYN-)ASSOCIATE.REQUEST");
	}
	sd = acc -> acc_sd, cc = 0;
	while (i == CONNECTING_1 || i == CONNECTING_2) {
	    int	    nfds;
	    fd_set  mask,
		   *rmask,
		   *wmask;

	    nfds = 0;
	    FD_ZERO (&mask);
	    if (PSelectMask (sd, &mask, &nfds, pi) == NOTOK) {
		fprintf (stderr, "failed\n");
		acs_adios (aca, "A-(ASYN-)ASSOCIATE.REQUEST(PSelectMask)");
	    }
	    rmask = (i == CONNECTING_2) ? &mask : NULLFD;
	    wmask = (i == CONNECTING_2) ? NULLFD : &mask;

	    fprintf (stderr, ".");
	    if (xselect (nfds, rmask, wmask, NULLFD, 1) == NOTOK) {
		fprintf (stderr, "failed\n");
		adios ("failed", "select");
	    }

	    if ((rmask && FD_ISSET (sd, rmask) == 0)
	            || (wmask && FD_ISSET (sd, wmask) == 0))
		continue;
	    
	    if ((i = AcAsynRetryRequest (sd, acc, aci)) == NOTOK) {
		fprintf (stderr, "failed\n");
		acs_adios (aca, "A-ASYN-RETRY.REQUEST");
	    }
	}
#endif

	if (acc -> acc_result != ACS_ACCEPT) {
	    fprintf (stderr, "failed\n");
	    adios (NULLCP, "connection rejected: [%s], %d elements",
		    AcErrString (acc -> acc_result), acc -> acc_ninfo);
	}
    }
    else {
#ifndef	ASYNC
	if (PConnRequest (NULLPA, pz, pl, NULLOID, prequirements,
		srequirements, srequirements
		    & (SR_MINORSYNC | SR_MAJORSYNC | SR_RESYNC | SR_ACTIVITY)
		? (long) (getpid () % (SERIAL_MAX - SERIAL_MIN + 1)) + SERIAL_MIN
		: SERIAL_NONE, tokens, sf, udata, NPDATA, NULLQOS, pc, pi) == NOTOK) {
	    fprintf (stderr, "failed\n");
	    ps_adios (pa, "P-CONNECT.REQUEST");
	}
	sd = pc -> pc_sd;
#else
	if ((i = PAsynConnRequest (NULLPA, pz, pl, NULLOID, prequirements,
		srequirements, ISN (srequirements), tokens, sf, udata, NPDATA,
		NULLQOS, pc, pi, 1)) == NOTOK) {
	    fprintf (stderr, "failed\n");
	    ps_adios (pa, "P-CONNECT.REQUEST");
	}
	sd = pc -> pc_sd, cc = 0;
	while (i == CONNECTING_1 || i == CONNECTING_2) {
	    int	    nfds;
	    fd_set  mask,
		   *rmask,
		   *wmask;

	    nfds = 0;
	    FD_ZERO (&mask);
	    if (PSelectMask (sd, &mask, &nfds, pi) == NOTOK) {
		fprintf (stderr, "failed\n");
		ps_adios (pa, "P-CONNECT.REQUEST(PSelectMask)");
	    }
	    rmask = (i == CONNECTING_2) ? &mask : NULLFD;
	    wmask = (i == CONNECTING_2) ? NULLFD : &mask;

	    fprintf (stderr, ".");
	    if (xselect (nfds, rmask, wmask, NULLFD, 1) == NOTOK) {
		fprintf (stderr, "failed\n");
		adios ("failed", "select");
	    }

	    if ((rmask && FD_ISSET (sd, rmask) == 0)
	            || (wmask && FD_ISSET (sd, wmask) == 0))
		continue;

	    if ((i = PAsynRetryRequest (sd, pc, pi)) == NOTOK) {
		fprintf (stderr, "failed\n");
		ps_adios (pa, "P-ASYN-RETRY.REQUEST");
	    }
	}
#endif

	if (pc -> pc_result != PC_ACCEPT) {
	    fprintf (stderr, "failed\n");
	    adios (NULLCP, "connection rejected: [%s], %d elements",
		    PErrString (pc -> pc_result), pc -> pc_ninfo);
	}
    }
    fprintf (stderr, "connected\n");

    if (isacs) {
	pc = &acc -> acc_connect;

#ifdef	DEBUG
	{
	    advise (NULLCP, "context: %s", oid2ode (acc -> acc_context));

	    advise (NULLCP,
		    "responding AE title: %s, responding PSAP address: %s",
		    sprintaei (&acc -> acc_respondtitle),
		    paddr2str (&pc -> pc_responding, NULLNA));

	    advise (NULLCP, "greetings: %d elements", acc -> acc_ninfo);

	    pl = &pc -> pc_ctxlist;
	    for (i = 0; i < pl -> pc_nctx; i++)
		advise (NULLCP, "ctx %d: 0x%x 0x%x %d",
			pl -> pc_ctx[i].pc_id, pl -> pc_ctx[i].pc_asn,
			pl -> pc_ctx[i].pc_atn, pl -> pc_ctx[i].pc_result);
	    advise (NULLCP, "default %d", pc -> pc_defctxresult);
	    advise (NULLCP, "p/s requirements 0x%x/0x%x",
		    pc -> pc_prequirements, pc -> pc_srequirements);
	}
#endif

	pl = &pc -> pc_ctxlist;

	if (mode == echo) {
	    if (acc -> acc_ninfo != NACDATA)
		adios (NULLCP, "expecting %d hellos, got %d elements",
			NACDATA, acc -> acc_ninfo);
	    for (i = 0; i < NACDATA; i++) {
		if ((pe = acc -> acc_info[i]) == NULLPE)
		    adios (NULLCP, "hello %d: NULL", i);
		if ((j = prim2num (pe)) == NOTOK
			&& pe -> pe_errno != PE_ERR_NONE)
		    adios (NULLCP, "hello %d: %s", i,
			    pe_error (pe -> pe_errno));
		if (j != i)
		    adios (NULLCP, "hello %d: value %d", i, j);
		if (pe -> pe_context != udata[i] -> pe_context)
		    adios (NULLCP, "hello %d: context of %d instead of %d",
			i, pe -> pe_context, udata[i] -> pe_context);
	    }
	}

	goto do_release;
    }
    else {
#ifdef	DEBUG
	{
	    advise (NULLCP, "responding PSAP address: %s",
		    paddr2str (&pc -> pc_responding, NULLNA));

	    advise (NULLCP, "greetings: %d elements", pc -> pc_ninfo);

	    pl = &pc -> pc_ctxlist;
	    for (i = 0; i < pl -> pc_nctx; i++)
		advise (NULLCP, "ctx %d: 0x%x 0x%x %d",
			pl -> pc_ctx[i].pc_id, pl -> pc_ctx[i].pc_asn,
			pl -> pc_ctx[i].pc_atn, pl -> pc_ctx[i].pc_result);
	    advise (NULLCP, "default %d", pc -> pc_defctxresult);
	    advise (NULLCP, "p/s requirements 0x%x/0x%x",
		    pc -> pc_prequirements, pc -> pc_srequirements);
	}
#endif

	if (mode == echo) {
	    if (pc -> pc_ninfo != NPDATA)
		adios (NULLCP, "expecting %d hellos, got %d elements",
			NPDATA, pc -> pc_ninfo);
	    for (i = 0; i < NPDATA; i++) {
		if ((pe = pc -> pc_info[i]) == NULLPE)
		    adios (NULLCP, "hello %d: NULL", i);
		if ((j = prim2num (pe)) == NOTOK
			&& pe -> pe_errno != PE_ERR_NONE)
		    adios (NULLCP, "hello %d: %s", i,
			    pe_error (pe -> pe_errno));
		if (j != i)
		    adios (NULLCP, "hello %d: value %d", i, j);
		if (pe -> pe_context != udata[i] -> pe_context)
		    adios (NULLCP, "hello %d: context of %d instead of %d",
			i, pe -> pe_context, udata[i] -> pe_context);
	    }
	}
    }

    nctxs = 0;
    pl = &pc -> pc_ctxlist;
    for (i = 0; i < pl -> pc_nctx; i++)
	if (pl -> pc_ctx[i].pc_result == PC_ACCEPT)
		datactxs[nctxs++] = pl -> pc_ctx[i].pc_id;

    srequirements = pc -> pc_srequirements;
    nmodes = 0;
    datamodes[nmodes++] = SX_NORMAL;
    if (srequirements & SR_EXPEDITED)
	datamodes[nmodes++] = SX_EXPEDITED;
    if ((srequirements & SR_CAPABILITY) && (srequirements & SR_ACTIVITY))
	datamodes[nmodes++] = SX_CAPDIND;
    if (srequirements & SR_TYPEDATA)
	datamodes[nmodes++] = SX_TYPED;

#define dotoken(requires,shift,bit,type) \
{ \
    if (srequirements & requires) \
	switch (pc -> pc_settings & (ST_MASK << shift)) { \
	    case ST_CALL_VALUE: \
		adios (NULLCP, "%s token: choice", type); \
 \
	    case ST_INIT_VALUE: \
		owned |= bit, avail |= bit; \
		break; \
 \
	    case ST_RESP_VALUE: \
		avail |= bit; \
		break; \
 \
	    default: \
		adios (NULLCP, "%s token: reserved", type); \
	} \
}
	dotokens ();
#undef	dotoken

    if (isacs)
	ACCFREE (acc)
    else
	PCFREE (pc);

    if (srequirements & SR_ACTIVITY) {
	(void) strcpy (id -> sd_data, mode == echo ? "echo" : "sink");
	id -> sd_len = strlen (id -> sd_data);
	if (PActStartRequest (sd, id, udata, NPDATA, pi) == NOTOK)
	    ps_adios (pa, "P-ACTIVITY-START.REQUEST");
    }

    if (fstat (fileno (stdin), &st) != NOTOK
	    && (st.st_mode & S_IFMT) == S_IFREG
	    && (cc = st.st_size) != 0) {
	(void) lseek (fileno (stdin), 0L, 0);

	if ((cp = malloc ((unsigned) cc)) == NULL)
	    adios (NULLCP, "no memory");
	for (dp = cp, j = cc; j > 0; dp += i, j -= i)
	    switch (i = read (fileno (stdin), dp, j)) {
		case NOTOK:
		    adios ("on stdin", "read failed");

		case OK:
		    adios (NULLCP, "premature end-of-file");
		    
		default:
		    break;
	    }
	if ((pe = oct2prim (cp, cc)) == NULLPE)
	    adios (NULLCP, "unable to allocate PSDU");
	free (cp);
	if (nctxs)
	    pe -> pe_context = datactxs[0];
	for (i = 10; i > 0; i--) {
#ifdef	TIMER
	    timer (0);
#endif
	    ps_datarequest (sd, pe, SX_NORMAL, 0);
#ifdef	TIMER
	    timer (cc);
#endif
	}
	pe_free (pe);
    }
    else {
	for (j = l = m = 0; fgets (buffer, sizeof buffer, stdin); ) {
	    k = j >= nmodes ? SX_EXPEDITED : datamodes[j++ % nmodes];
	    if ((cc = strlen (buffer) + 1) > SX_EXSIZE - 7
		    && k == SX_EXPEDITED) {
		if ((k = datamodes[j++ % nmodes]) == SX_EXPEDITED)
		    k = datamodes[j++ % nmodes];
	    }

	    switch (k) {
		case SX_CAPDIND:
		    if (!(requirements & SR_RESYNC) || l++ & 0x01) {
			ps_waitfor (sd, ST_ACT_TOKEN);
			if (l & 0x03) {
			    if (PActIntrRequest (sd, SP_SEQUENCE, pi)
				    == NOTOK)
				ps_adios (pa, "P-ACTIVITY-INTERRUPT.REQUEST");
			}
			else {
			    if (PActDiscRequest (sd, SP_SEQUENCE, pi)
				    == NOTOK)
				ps_adios (pa, "P-ACTIVITY-DISCARD.REQUEST");
			}
			ps_waitfor (sd, -1);
			goto push_data;
		    }
		    tokens = 0;
#define dotoken(requires,shift,bit,type) \
{ \
		    if (requirements & requires) \
			tokens |= ST_CALL_VALUE << shift; \
}
		    dotokens ();
#undef	dotoken
		    if (PReSyncRequest (sd, SYNC_SET, ssn - 1, tokens,
				udata, NPDATA, pi) == NOTOK)
			ps_adios (pa, "P-RESYNCHRONIZE.REQUEST");
		    ps_waitfor (sd, -1);
		    break;

		case SX_EXPEDITED:
		    if (j >= nmodes)
			j = j % nmodes;
		    if (!(srequirements & SR_EXPEDITED))
			k = SX_NORMAL;	/* fall... */
		default:
push_data: ;
		    if ((pe = oct2prim (buffer, cc)) == NULLPE)
			adios (NULLCP, "unable to allocate PSDU");
		    if (nctxs && k != SX_EXPEDITED)
			pe -> pe_context = datactxs[m++ % nctxs];
		    ps_datarequest (sd, pe, k, 1);
		    pe_free (pe);
		    if (k == SX_CAPDIND
			    && PActResumeRequest (sd, id, id, 
				    (long) (getpid () % (SERIAL_MAX - SERIAL_MIN + 1))
				    + SERIAL_MIN, sf, udata, NPDATA, pi)
				    == NOTOK)
			ps_adios (pa, "P-ACTIVITY-RESUME.REQUEST");
		    break;
	    }
	}

	if (requirements & SR_EXCEPTIONS) {
	    if (owned & ST_DAT_TOKEN)
		if (PGTokenRequest (sd, ST_DAT_TOKEN, pi) == NOTOK)
		    ps_adios (pa, "P-TOKEN-GIVE.REQUEST");
		else
		    owned &= ~ST_DAT_TOKEN;
	    if (PUReportRequest (sd, SP_NOREASON, udata, NPDATA, pi) == NOTOK)
		ps_adios (pa, "P-U-EXCEPTION-REPORT.REQUEST");
	    ps_waitfor (sd, -1);
	}    
    }

    if ((requirements & SR_MAJORSYNC) && !(requirements & SR_ACTIVITY)) {
	if (PMajSyncRequest (sd, &ssn, udata, NPDATA, pi) == NOTOK)
	    switch (pa -> pa_reason) {
		case PC_OPERATION:
		    ps_waitfor (sd, ST_DAT_TOKEN | ST_MIN_TOKEN
			    | ST_MAJ_TOKEN);
		    if (PMajSyncRequest (sd, &ssn, udata, NPDATA, pi) == OK)
			break;	/* else fall */

		default:
		    ps_adios (pa, "P-MAJOR-SYNC.REQUEST");
	    }

	ps_waitfor (sd, -1);
    }

    if (requirements & SR_ACTIVITY) {
	if (PActEndRequest (sd, &ssn, udata, NPDATA, pi) == NOTOK)
	    switch (pa -> pa_reason) {
		case PC_OPERATION:
		    ps_waitfor (sd, avail);
		    if (PActEndRequest (sd, &ssn, udata, NPDATA, pi) == OK)
			break;	/* else fall */

		default:
		    ps_adios (pa, "P-ACTIVITY-END.REQUEST");
	    }

	ps_waitfor (sd, -1);
		
	if (PGControlRequest (sd, pi) == NOTOK)
	    switch (pa -> pa_reason) {
		case PC_OPERATION:
		    ps_waitfor (sd, avail);
		    if (PGControlRequest (sd, pi) == OK)
			break;	/* else fall */

		default:
		    ps_adios (pa, "P-CONTROL-GIVE.REQUEST");
	    }

	owned = 0;

	ps_waitfor (sd, -1);
    }

do_release: ;
    if (isacs) {
	if (AcRelRequest (sd, ACF_NORMAL, udata, NACDATA, NOTOK, acr, aci)
		== NOTOK)
	    switch (aca -> aca_reason) {
		case ACS_OPERATION:
		    ps_waitfor (sd, avail);
		    if (AcRelRequest (sd, ACF_NORMAL, udata, NACDATA, NOTOK,
				      acr, aci) == OK)
			break;	/* else fall */

		default:
		    acs_adios (aca, "A-RELEASE.REQUEST");
	}

	if (!acr -> acr_affirmative) {
	    (void) AcUAbortRequest (sd, NULLPEP, 0, aci);
	    adios (NULLCP, "release rejected by peer: %d, %d elements",
			acr -> acr_reason, acr -> acr_ninfo);
	}

#ifdef	DEBUG
	advise (NULLCP, "A-RELEASE.CONFIRMATION: %d, %d elements",
		acr -> acr_reason, acr -> acr_ninfo);
#endif		
	ACRFREE (acr);
    }
    else {
	if (PRelRequest (sd, udata, NPDATA, NOTOK, pr, pi) == NOTOK)
	    switch (pa -> pa_reason) {
		case PC_OPERATION: 
		case PC_WAITING: 
		    ps_waitfor (sd, avail);
		    if (PRelRequest (sd, udata, NPDATA, NOTOK, pr, pi) == OK)
			break;	/* else fall */

		default: 
		    ps_adios (pa, "P-RELEASE.REQUEST");
	    }

	if (!pr -> pr_affirmative) {
	    (void) PUAbortRequest (sd, NULLPEP, 0, pi);
	    adios (NULLCP, "release rejected by peer: %d elements",
		    pr -> pr_ninfo);
	}
	PRFREE (pr);
    }

    for (i = 0; i < NPDATA; i++)
	pe_free (udata[i]);
}

/*  */

static int ps_datarequest (sd, pe, dm, sync)
int	sd;
PE	pe;
int	dm,
	sync;
{
    int     result;
    struct PSAPdata pxs;
    register struct PSAPdata   *px = &pxs;
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort  *pa = &pi -> pi_abort;

    switch (dm) {
	default: 
	    if (PDataRequest (sd, &pe, 1, pi) == NOTOK)
		switch (pa -> pa_reason) {
		    case PC_OPERATION: 
			ps_waitfor (sd, ST_DAT_TOKEN);
			if (PDataRequest (sd, &pe, 1, pi) == OK)
			    break;/* else fall */

		    default: 
			ps_adios (pa, "P-DATA.REQUEST");
		}
	    break;

	case SX_EXPEDITED: 
	    if (PExpdRequest (sd, &pe, 1, pi) == NOTOK)
		ps_adios (pa, "P-EXPEDITED-DATA.REQUEST");
	    break;

	case SX_CAPDIND: 
	    if (PCapdRequest (sd, &pe, 1, pi) == NOTOK)
		switch (pa -> pa_reason) {
		    case PC_OPERATION: 
			ps_waitfor (sd, avail & ~ST_RLS_TOKEN);
			if (PCapdRequest (sd, &pe, 1, pi) == OK)
			    break;/* else fall */

		    default: 
			ps_adios (pa, "P-CAPABILITY-DATA.REQUEST");
		}
	    break;

	case SX_TYPED: 
	    if (PTypedRequest (sd, &pe, 1, pi) == NOTOK)
		ps_adios (pa, "P-TYPED-DATA.REQUEST");
	    break;
    }

    if (mode == echo || dm == SX_CAPDIND)
	for (;;) {
	    switch (result = PReadRequest (sd, px, NOTOK, pi)) {
		case NOTOK: 
		    ps_adios (pa, "P-READ.REQUEST");

		case OK: 
		    if ((dm != SX_CAPDIND ? dm : SX_CAPDCNF)
			    != px -> px_type) {
			advise (NULLCP,
				"data indication type mismatch, orig=%d echo=%d",
				dm, px -> px_type);
			status++;
		    }
		    if (px -> px_ninfo != 1) {
			advise (NULLCP, "length mismatch, orig=%d echo=%d",
				1, px -> px_ninfo);
			status++;
		    }
		    if (pe -> pe_context != (*px -> px_info) -> pe_context) {
			advise (NULLCP, "context mismatch, orig=%d echo=%d",
				pe -> pe_context,
				(*px -> px_info) -> pe_context);
			status++;
		    }
		    if (pe_cmp (pe, *px -> px_info)) {
			advise (NULLCP, "data mismatch (1)");
			vunknown (pe);
			advise (NULLCP, "---------");
			vunknown (*px -> px_info);
			advise (NULLCP, "---------");
			status++;
		    }
		    PXFREE (px)
		    break;

		case DONE: 
		    ps_event (sd, pi);
		    continue;

		default: 
		    adios (NULLCP, "unknown return from PReadRequest=%d",
			    result);
	    }
	    break;
	}

    if (sync &&
	    (srequirements & SR_MINORSYNC) && !(srequirements & SR_ACTIVITY)) {
	if (PMinSyncRequest (sd, SYNC_CONFIRM, &ssn, NULLPEP, 0, pi) == NOTOK)
	    switch (pa -> pa_reason) {
		case PC_OPERATION: 
		    ps_waitfor (sd, ST_DAT_TOKEN | ST_MIN_TOKEN);
		    if (PMinSyncRequest (sd, SYNC_CONFIRM, &ssn, NULLPEP, 0,
			    pi) == OK)
			break;	/* else fall */

		default: 
		    ps_adios (pa, "P-MINOR-SYNC.REQUEST");
	    }

	ps_waitfor (sd, -1);
    }
    else
	if (sync
		&& (srequirements & SR_ACTIVITY)
		&& (srequirements & SR_MAJORSYNC)
		&& dm == SX_NORMAL) {
	    if (PMajSyncRequest (sd, &ssn, NULLPEP, 0, pi) == NOTOK)
		switch (pa -> pa_reason) {
		    case PC_OPERATION: 
			ps_waitfor (sd, ST_DAT_TOKEN | ST_MIN_TOKEN
				| ST_MAJ_TOKEN);
			if (PMajSyncRequest (sd, &ssn, NULLPEP, 0, pi) == OK)
			    break;/* else fall */

		    default: 
			ps_adios (pa, "P-MAJOR-SYNC.REQUEST");
		}

	    ps_waitfor (sd, -1);
	}
}

/*  */

static int  ps_waitfor (sd, want)
int	sd,
	want;
{
    int     result,
	    tokens;
    struct PSAPdata pxs;
    register struct PSAPdata   *px = &pxs;
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort  *pa = &pi -> pi_abort;

    for (;;) {
	if (want == -1) {
	    want = avail;
	    goto read_it;
	}

	tokens = 0;
#define dotoken(requires,shift,bit,type) \
{ \
	if ((want & bit) && !(owned & bit)) \
	    tokens |= bit; \
}
	dotokens ();
#undef	dotoken
	if (tokens == 0)
	    return;

	if (PPTokenRequest (sd, tokens, NULLPEP, 0, pi) == NOTOK)
	    ps_adios (pa, "P-TOKEN-PLEASE.REQUEST");

read_it: ;
	switch (result = PReadRequest (sd, px, NOTOK, pi)) {
	    case NOTOK: 
		ps_adios (pa, "P-READ.REQUEST");

	    case OK: 
		ps_abort (sd, "protocol screw-up");

	    case DONE: 
		ps_event (sd, pi);
		break;

	    default: 
		adios (NULLCP, "unknown return from PReadRequest=%d",
			result);
	}
    }
}

/*  */

static	ps_event (sd, pi)
int	sd;
register struct PSAPindication *pi;
{
    register struct PSAPabort  *pa = &pi -> pi_abort;
    register struct PSAPactivity  *pv = &pi -> pi_activity;
    register struct PSAPfinish  *pf = &pi -> pi_finish;
    register struct PSAPreport  *pp = &pi -> pi_report;
    register struct PSAPsync   *pn = &pi -> pi_sync;
    register struct PSAPtoken  *pt = &pi -> pi_token;
    struct AcSAPindication  acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;
    register struct AcSAPfinish *acf = &aci -> aci_finish;

    switch (pi -> pi_type) {
	case PI_TOKEN: 
	    switch (pt -> pt_type) {
		case ST_GIVE: 
		case ST_CONTROL: 
		    owned = pt -> pt_owned;
		    break;

		case ST_PLEASE: 
		    if (PGTokenRequest (sd,
				(int) pt -> pt_tokens, pi)
			    == NOTOK)
			ps_adios (pa, "P-TOKEN-GIVE.REQUEST");
		    else
			owned &= ~pt -> pt_tokens;
		    break;

		default: 
		    adios (NULLCP,
			    "unknown token indication type=0x%x",
			    pt -> pt_type);
	    }
	    PTFREE (pt);
	    break;

	case PI_SYNC: 
	    switch (pn -> pn_type) {
		case SN_MAJORIND: 
		    adios (NULLCP, "majorsync indication %d",
			    pn -> pn_ssn);
		    break;

		case SN_MAJORCNF: 
		    break;

		case SN_MINORIND: 
		    adios (NULLCP, "minorsync indication %d%s",
			    pn -> pn_ssn, pn -> pn_options == SYNC_CONFIRM
			    ? " (wants confirmation)" : NULLCP);
		    break;

		case SN_MINORCNF: 
		    break;

		case SN_RESETIND: 
#define	dotoken(requires,shift,bit,type) \
{ \
		    if (srequirements & requires) \
			switch (pn -> pn_settings & (ST_MASK << shift)) { \
			    case ST_CALL_VALUE << shift: \
				pn -> pn_settings &= ~(ST_MASK << shift); \
				pn -> pn_settings |= ST_RESP_VALUE << shift; \
			    case ST_RESP_VALUE << shift: \
				owned &= ~bit; \
				break; \
 \
			    case ST_INIT_VALUE << shift: \
				owned |= bit; \
				break; \
 \
			    default: \
				adios (NULLCP, "%s token: reserved", type); \
				break; \
			} \
}
		    dotokens ();
#undef	dotoken
		    if (PReSyncResponse (sd, pn -> pn_ssn, pn -> pn_settings,
				NULLPEP, 0, pi) == NOTOK)
			ps_adios (pa, "P-RESYNCHRONIZE.RESPONSE");
		    break;

		case SN_RESETCNF: 
		    break;

		default: 
		    adios (NULLCP, "unknown sync indication=0x%x, ssn=%d",
			    pn -> pn_type, pn -> pn_ssn);
	    }
	    PNFREE (pn);
	    break;

	case PI_ACTIVITY: 
	    switch (pv -> pv_type) {
		case SV_START: 
		    adios (NULLCP, "activity start indication: %*.*s",
			    pv -> pv_id.sd_len, pv -> pv_id.sd_len,
			    pv -> pv_id.sd_data);

		case SV_RESUME: 
		    adios (NULLCP, 
			    "activity resume indication: id=%*.*s oid=%*.*s connect=%s ssn=%d",
			    pv -> pv_id.sd_len, pv -> pv_id.sd_len,
			    pv -> pv_id.sd_data, pv -> pv_oid.sd_len,
			    pv -> pv_oid.sd_len, pv -> pv_oid.sd_data,
			    sprintref (&pv -> pv_connect), pv -> pv_ssn);

		case SV_INTRIND: 
		    adios (NULLCP, "activity interrupt indication %d",
			    pv -> pv_reason);

		case SV_INTRCNF: 
		    break;

		case SV_DISCIND: 
		    adios (NULLCP, "activity discard indication %d",
			    pv -> pv_reason);

		case SV_DISCCNF: 
		    break;

		case SV_ENDIND: 
		    adios (NULLCP, "activity end indication %d",
			    pv -> pv_ssn);

		case SV_ENDCNF: 
		    break;

		default: 
		    adios (NULLCP, "unknown activity indication=0x%x",
			    pv -> pv_type);
	    }
	    PVFREE (pv);
	    break;

	case PI_REPORT: 
	    advise (NULLCP, "%s report %d",
		    pp -> pp_peer ? "user" : "provider", pp -> pp_reason);
	    if (srequirements & SR_DAT_EXISTS) {
		if (PGTokenRequest (sd, ST_DAT_TOKEN, pi) == NOTOK)
		    ps_adios (pa, "P-TOKEN-GIVE.REQUEST (to clear exception)");
		else
		    owned &= ~ST_DAT_TOKEN;
	    }
	    else
		ps_abort (sd, "aborted");
	    PPFREE (pp);
	    break;

	case PI_FINISH: 
	    if (isacs) {
		if (AcFINISHser (sd, pf, aci) == NOTOK)
		    acs_adios (aca, "AcFINISHser");
#ifdef	DEBUG
		advise (NULLCP, "A-RELEASE.INDICATION %d, %d elements",
			acf -> acf_reason, acf -> acf_ninfo);
		if (AcRelResponse (sd, ACS_USER_NOREASON, ACR_NOTFINISHED,
				NULLPEP, 0, aci) == NOTOK)
		    acs_adios (aca, "A-RELEASE.RESPONSE");
#endif

		ACFFREE (acf);
	    }
	    else {
		if (PRelResponse (sd, PC_REJECTED, NULLPEP, 0, pi) == NOTOK)
		    ps_adios (pa, "P-RELEASE.RESPONSE");

		PFFREE (pf);
	    }
	    break;

	default: 
	    adios (NULLCP, "unknown indication type=0x%x", pi -> pi_type);
    }
}

/*  */

static  ps_abort (sd, reason)
int     sd;
char   *reason;
{
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort  *pa = &pi -> pi_abort;
    struct AcSAPindication  acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;

    if (isacs) {
	if (AcUAbortRequest (sd, NULLPEP, 0, aci) == NOTOK)
	    acs_adios (aca, "A-U-ABORT.REQUEST");
    }
    else {
	if (PUAbortRequest (sd, NULLPEP, 0, pi) == NOTOK)
	    ps_adios (pa, "P-U-ABORT.REQUEST");
    }

    adios (NULLCP, "%s", reason);
}

/*  */

static void  ps_adios (pa, event)
register struct PSAPabort *pa;
char   *event;
{
    ps_advise (pa, event);

    _exit (1);
}


static void  ps_advise (pa, event)
register struct PSAPabort *pa;
char   *event;
{
    char    buffer[BUFSIZ];

    if (pa -> pa_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s",
		PErrString (pa -> pa_reason),
		pa -> pa_cc, pa -> pa_cc, pa -> pa_data);
    else
	(void) sprintf (buffer, "[%s]", PErrString (pa -> pa_reason));

    advise (NULLCP, "%s: %s%s", event, buffer,
	    pa -> pa_peer ? " (peer initiated)" : "");
}

/*    AcSAP */

static void  acs_adios (aca, event)
register struct AcSAPabort *aca;
char   *event;
{
    acs_advise (aca, event);

    _exit (1);
}


static void  acs_advise (aca, event)
register struct AcSAPabort *aca;
char   *event;
{
    char    buffer[BUFSIZ];

    if (aca -> aca_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s",
		AcErrString (aca -> aca_reason),
		aca -> aca_cc, aca -> aca_cc, aca -> aca_data);
    else
	(void) sprintf (buffer, "[%s]", AcErrString (aca -> aca_reason));

	advise (NULLCP, "%s: %s (source %d)", event, buffer,
		aca -> aca_source);
}

/*    RtSAP */

static int turn = 0;

/*  */

static int  rts_main (is, addr)
struct isoservent *is;
char *addr;
{
    int     sd,
            cc,
            i,
            j,
	    ros;
    char   *cp,
           *dp,
            buffer[BUFSIZ];
    register struct PSAPaddr   *pa;
    struct PSAPctxlist pls;
    register struct PSAPctxlist *pl = &pls;
    struct AcSAPrelease acrs;
    register struct AcSAPrelease *acr = &acrs;
    struct RtSAPaddr    rtzs;
    register struct RtSAPaddr  *rtz = &rtzs;
    struct RtSAPconnect rtcs;
    register struct RtSAPconnect   *rtc = &rtcs;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;
    register struct RtSAPabort   *rta = &rti -> rti_abort;
#ifdef	DEBUG
    struct AcSAPconnect *acc= &rtc -> rtc_connect;
    struct PSAPconnect *pc = &acc -> acc_connect;
#endif
    register PE	    pe;
    AEI	    aei;
    OID	    oid,
	    ode;
    struct stat st;

    if ((pe = int2prim (i = getpid ())) == NULLPE)
	adios (NULLCP, "unable to allocate hello");

    turn = mode == sink;

    if (isacs) {
	if (ros = strncmp (isacs, "isode/ros_", strlen ("isode/ros_")) == 0)
	    mode = strcmp (isacs, "isode/ros_sink") ? echo : sink;
	else
	    mode = strcmp (isacs, "isode/rtse sink") ? echo : sink;
	turn = mode == sink;

	if ((aei = str2aei (addr, isacs)) == NULLAEI)
	    adios (NULLCP, "%s-%s: unknown application-entity", addr, isacs);
	if ((pa = aei2addr (aei)) == NULLPA)
	    adios (NULLCP, "address translation failed");

	cp = mode == echo ? "isode echo pci" : "isode sink pci";
	if ((ode = ode2oid (cp)) == NULLOID)
	    adios (NULLCP, "%s: unknown object descriptor", cp);
	ode = oid_cpy (ode);

	for (j = (pl -> pc_nctx = NPCTX - 2) - 1; j >= 0; j--) {
	    pl -> pc_ctx[j].pc_id = j * 2 + 1;
	    if ((oid = ode2oid ("iso asn.1 abstract syntax")) == NULLOID)
		adios (NULLCP, "iso asn.1 abstract syntax: unknown");
	    pl -> pc_ctx[j].pc_asn = oid_cpy (oid);
	    pl -> pc_ctx[j].pc_atn = NULLOID;
	}

	fprintf (stderr, "%s... ", addr);
	(void) fflush (stderr);
	if (RtOpenRequest (RTS_TWA, turn ? RTS_INITIATOR : RTS_RESPONDER,
		    ode, NULLAEI, aei, NULLPA, pa, pl, ode, pe,
		    NULLQOS, rtc, rti) == NOTOK) {
	    fprintf (stderr, "failed\n");
	    rts_adios (rta, "RT-OPEN.REQUEST");
	}
    }
    else {
	register struct SSAPaddr *sa;

	if (ros = strncmp (is -> is_entity, "ros_", strlen ("ros_")) == 0) {
	    mode = strcmp (is -> is_entity, "ros_sink") ? echo : sink;
	    turn = mode == sink;
	}

	rtz -> rta_port = is -> is_port;	/* yikes! */
	if ((is = getisoserventbyname ("rts", "ssap")) == NULL)
	    adios (NULLCP, "ssap/rts: unknown entity");
	if ((sa = is2saddr (addr, NULLCP, is)) == NULLSA)
	    adios (NULLCP, "address translation failed");
	rtz -> rta_addr = *sa;	/* struct copy */

	fprintf (stderr, "%s... ", addr);
	(void) fflush (stderr);
	if (RtBeginRequest (rtz, RTS_TWA, turn ? RTS_INITIATOR : RTS_RESPONDER,
		    pe, rtc, rti) == NOTOK) {
	    fprintf (stderr, "failed\n");
	    rts_adios (rta, "RT-BEGIN.REQUEST");
	}
    }

    pe_free (pe);

    if (rtc -> rtc_result != RTS_ACCEPT) {
	fprintf (stderr, "failed\n");
	adios (NULLCP, "association rejected: [%s]",
		RtErrString (rtc -> rtc_result));
    }
    fprintf (stderr, "connected\n");

#ifdef	DEBUG
    advise (NULLCP, "sent greetings of %d", i);
#endif

    sd = rtc -> rtc_sd;
    if (rtc -> rtc_data) {
	if ((i = prim2num (rtc -> rtc_data)) == NOTOK
		&& rtc -> rtc_data -> pe_errno != PE_ERR_NONE)
	    adios (NULLCP, "error decoding hello: %s (%d)",
		    pe_error (rtc -> rtc_data -> pe_errno), i);
#ifdef	DEBUG
	advise (NULLCP, "received greetings of %d", i);
#endif
    }

#ifdef	DEBUG
    if (isacs) {
	    advise (NULLCP, "context: %s", oid2ode (acc -> acc_context));

	    advise (NULLCP,
		    "responding AE title: %s, responding PSAP address: %s",
		    sprintaei (&acc -> acc_respondtitle),
		    paddr2str (&pc -> pc_responding, NULLNA));

	    advise (NULLCP, "greetings: %d elements", acc -> acc_ninfo);

	    pl = &pc -> pc_ctxlist;
	    for (i = 0; i < pl -> pc_nctx; i++)
		advise (NULLCP, "ctx %d: 0x%x 0x%x %d",
			pl -> pc_ctx[i].pc_id, pl -> pc_ctx[i].pc_asn,
			pl -> pc_ctx[i].pc_atn, pl -> pc_ctx[i].pc_result);
	    advise (NULLCP, "default %d", pc -> pc_defctxresult);
	    advise (NULLCP, "p/s requirements 0x%x/0x%x",
		    pc -> pc_prequirements, pc -> pc_srequirements);
	}
#endif

    RTCFREE (rtc);

    if (ros) {
	struct RoSAPindication rois;
	register struct RoSAPpreject *rop = &rois.roi_preject;

	if (RoSetService (sd, RoRtService, &rois) == NOTOK)
	    ros_adios (rop, "set RO/PT fails");

	do_ros (sd);
	return;
    }

    if (fstat (fileno (stdin), &st) != NOTOK
	    && (st.st_mode & S_IFMT) == S_IFREG
	    && (cc = st.st_size) != 0) {
	(void) lseek (fileno (stdin), 0L, 0);

	if ((cp = malloc ((unsigned) cc)) == NULL)
	    adios (NULLCP, "no memory");
	for (dp = cp, j = cc; j > 0; dp += i, j -= i)
	    switch (i = read (fileno (stdin), dp, j)) {
		case NOTOK: 
		    adios ("on stdin", "read failed");

		case OK: 
		    adios (NULLCP, "premature end-of-file");

		default: 
		    break;
	    }
	if ((pe = oct2prim (cp, cc)) == NULLPE)
	    adios (NULLCP, "unable to allocate APDU");
	free (cp);
	for (i = 10; i > 0; i--) {
#ifdef	TIMER
	    timer (0);
#endif
	    rts_transferequest (sd, pe);
#ifdef	TIMER
	    timer (cc);
#endif
	}
	pe_free (pe);
    }
    else
	while (fgets (buffer, sizeof buffer, stdin)) {
	    if ((pe = oct2prim (buffer, strlen (buffer) + 1)) == NULLPE)
		adios (NULLCP, "unable to allocate APDU");
	    rts_transferequest (sd, pe);
	    pe_free (pe);
	}

    if (isacs) {
	if ((pe = int2prim (i = getpid ())) == NULLPE)
	    adios (NULLCP, "unable to allocate hello");

	if (RtCloseRequest (sd, ACF_NORMAL, pe, acr, rti) == NOTOK)
	    switch (rta -> rta_reason) {
		case RTS_OPERATION: 
		case RTS_WAITING: 
		    rts_waitfor (sd);
		    if (RtCloseRequest (sd, ACF_NORMAL, pe, acr, rti)
			    == OK)
			break;	/* else fall */

		default: 
		    rts_adios (rta, "RT-CLOSE.REQUEST");
	    }

	if (!acr -> acr_affirmative) {
	    (void) RtUAbortRequest (sd, NULLPE, rti);
	    adios (NULLCP, "release rejected by peer: %d, %d elements",
			acr -> acr_reason, acr -> acr_ninfo);
	}

	if (mode == echo) {
	    if (acr -> acr_ninfo != 1)
		advise (NULLCP, "got %d elements returned on close",
			    acr -> acr_ninfo);
		if (pe_cmp (pe, acr -> acr_info[0])) {
		    advise (NULLCP, "data mismatch (2)");
		    vunknown (pe);
		    advise (NULLCP, "---------");
		    vunknown (acr -> acr_info[0]);
		    advise (NULLCP, "---------");
		    status++;
		}
	}

	ACRFREE (acr);

	pe_free (pe);
    }
    else
	if (RtEndRequest (sd, rti) == NOTOK)
	    switch (rta -> rta_reason) {
		case RTS_OPERATION: 
		case RTS_WAITING: 
		    rts_waitfor (sd);
		    if (RtEndRequest (sd, rti) == OK)
			break;	/* else fall */

		default: 
		    rts_adios (rta, "RT-END.REQUEST");
	    }
}

/*  */

static int  rts_transferequest (sd, pe)
int	sd;
register PE	pe;
{
    int     result;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;
    register struct RtSAPabort *rta = &rti -> rti_abort;
    register struct RtSAPtransfer  *rtt = &rti -> rti_transfer;

    if (RtTransferRequest (sd, pe, NOTOK, rti) == NOTOK)
	switch (rta -> rta_reason) {
	    case RTS_OPERATION: 
		rts_waitfor (sd);
		if (RtTransferRequest (sd, pe, NOTOK, rti) == OK)
		    break;	/* else fall */

	    default: 
		if (RTS_FATAL (rta -> rta_reason))
		    rts_adios (rta, "RT-TRANSFER.REQUEST");
		rts_advise (rta, "RT-TRANSFER.REQUEST");
		return;
	}

    if (mode == echo)
	for (;;)
	    switch (result = RtWaitRequest (sd, NOTOK, rti)) {
		case NOTOK: 
		    rts_adios (rta, "RT-WAIT.REQUEST");

		case OK: 
		    if (pe_cmp (pe, rtt -> rtt_data)) {
			advise (NULLCP, "data mismatch (3)");
			vunknown (pe);
		        advise (NULLCP, "---------");
			vunknown (rtt -> rtt_data);
		        advise (NULLCP, "---------");
			status++;
		    }
		    RTTFREE (rtt);
		    return;

		case DONE: 
		    rts_event (sd, rti);
		    break;

		default: 
		    adios (NULLCP, "unknown return from RtWaitRequest=%d",
			    result);
	    }
}

/*  */

static int  rts_waitfor (sd)
int	sd;
{
    int     result;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;
    register struct RtSAPabort *rta = &rti -> rti_abort;
    static int  priority = 1;

    if (turn)
	return;

    if (RtPTurnRequest (sd, priority++, rti) == NOTOK)
	rts_adios (rta, "RT-TURN-PLEASE.REQUEST");

    while (!turn)
	switch (result = RtWaitRequest (sd, NOTOK, rti)) {
	    case NOTOK: 
		rts_adios (rta, "RT-WAIT.REQUEST");

	    case OK: 
		adios (NULLCP, "protocol screw-up");

	    case DONE: 
		rts_event (sd, rti);
		break;

	    default: 
		adios (NULLCP, "unknown return from RtWaitRequest=%d",
			result);
	}
}

/*  */

static int  rts_event (sd, rti)
int	sd;
register struct RtSAPindication *rti;
{
    register struct RtSAPabort *rta = &rti -> rti_abort;
    register struct RtSAPturn  *rtu = &rti -> rti_turn;

    switch (rti -> rti_type) {
	case RTI_TURN: 
	    if (rtu -> rtu_please) {
		if (RtGTurnRequest (sd, rti) == NOTOK)
		    rts_adios (rta, "RT-TURN-GIVE.REQUEST");
		turn = 0;
	    }
	    else
		turn = 1;
	    break;

	case RTI_CLOSE: 
	    adios (NULLCP, "got RT-END.INDICATION");

	case RTI_FINISH:
	    adios (NULLCP, "got RT-CLOSE.INDICATION");

	default: 
	    adios (NULLCP, "unknown indication type=0x%x",
		    rti -> rti_type);
    }
}

/*  */

static void  rts_adios (rta, event)
register struct RtSAPabort *rta;
char   *event;
{
    rts_advise (rta, event);

    _exit (1);
}


static void  rts_advise (rta, event)
register struct RtSAPabort *rta;
char   *event;
{
    char    buffer[BUFSIZ];

    if (rta -> rta_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s", RtErrString (rta -> rta_reason),
		rta -> rta_cc, rta -> rta_cc, rta -> rta_data);
    else
	(void) sprintf (buffer, "[%s]", RtErrString (rta -> rta_reason));

    advise (NULLCP, "%s: %s", event, buffer);
}

/*    RoSAP */

static int  ros_main (is, addr)
struct isoservent *is;
char *addr;
{
    int     sd,
            i;
    char   *cp;
    struct SSAPref  sfs;
    register struct SSAPref *sf;
    register struct PSAPaddr   *pa;
    struct AcSAPconnect accs;
    register struct AcSAPconnect   *acc = &accs;
    struct AcSAPindication  acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;
    struct RoSAPaddr    roas;
    register struct RoSAPaddr  *roa = &roas;
    struct RoSAPconnect rocs;
    register struct RoSAPconnect   *roc = &rocs;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject   *rop = &roi -> roi_preject;
#ifdef	DEBUG
    struct PSAPconnect *pc = &acc -> acc_connect;
#endif
    struct PSAPctxlist pls;
    register struct PSAPctxlist *pl = &pls;
    AEI	    aei;
    OID     oid,
	    ode;
    register PE	    pe;

    if (isacs) {
	if ((aei = str2aei (addr, isacs)) == NULLAEI)
	    adios (NULLCP, "%s-%s: unknown application-entity", addr, isacs);
	if ((pa = aei2addr (aei)) == NULLPA)
	    adios (NULLCP, "address translation failed");

	cp = mode == echo ? "isode echo pci" : "isode sink pci";
	if ((ode = ode2oid (cp)) == NULLOID)
	    adios (NULLCP, "%s: unknown object descriptor", cp);
	ode = oid_cpy (ode);

	if ((sf = addr2ref (PLocalHostName ())) == NULL) {
	    sf = &sfs;
	    (void) bzero ((char *) sf, sizeof *sf);
	}
	pl -> pc_nctx = 1;
	pl -> pc_ctx[0].pc_id = 1;
	if ((oid = ode2oid ("iso asn.1 abstract syntax")) == NULLOID)
	    adios (NULLCP, "iso asn.1 abstract syntax: unknown");
	pl -> pc_ctx[0].pc_asn = oid_cpy (oid);
	pl -> pc_ctx[0].pc_atn = NULLOID;

	fprintf (stderr, "%s... ", addr);
	(void) fflush (stderr);
	if (AcAssocRequest (ode, NULLAEI, aei, NULLPA, pa, pl, ode,
		    0, ROS_MYREQUIRE, SERIAL_NONE, 0, sf, NULLPEP, 0, NULLQOS,
		    acc, aci)
		== NOTOK)
	    acs_adios (aca, "A-ASSOCIATE.REQUEST");

	if (acc -> acc_result != ACS_ACCEPT) {
	    fprintf (stderr, "failed\n");
	    adios (NULLCP, "association rejected: [%s]",
		    AcErrString (acc -> acc_result));
	}
	fprintf (stderr, "connected\n");

	sd = acc -> acc_sd;

#ifdef	DEBUG
	pa = &pc -> pc_responding;
	{
	    advise (NULLCP, "context: %s", oid2ode (acc -> acc_context));

	    advise (NULLCP,
		    "responding AE title: %s, responding PSAP address: %s",
		    sprintaei (&acc -> acc_respondtitle),
		    paddr2str (&pc -> pc_responding, NULLNA));

	    advise (NULLCP, "greetings: %d elements", acc -> acc_ninfo);

	    pl = &pc -> pc_ctxlist;
	    for (i = 0; i < pl -> pc_nctx; i++)
		advise (NULLCP, "ctx %d: 0x%x 0x%x %d",
			pl -> pc_ctx[i].pc_id, pl -> pc_ctx[i].pc_asn,
			pl -> pc_ctx[i].pc_atn, pl -> pc_ctx[i].pc_result);
	    advise (NULLCP, "default %d", pc -> pc_defctxresult);
	    advise (NULLCP, "p/s requirements 0x%x/0x%x",
		    pc -> pc_prequirements, pc -> pc_srequirements);
	}
#endif

	ACCFREE (acc);

	if (RoSetService (sd, RoPService, roi) == NOTOK)
	    ros_adios (rop, "set RO/PT fails");
    }
    else {
	register struct SSAPaddr *sa;

	roa -> roa_port = is -> is_port;	/* yikes! */
	if ((is = getisoserventbyname ("ros", "ssap")) == NULL)
	    adios (NULLCP, "ssap/ros: unknown entity");
	if ((sa = is2saddr (addr, NULLCP, is)) == NULLSA)
	    adios (NULLCP, "address translation failed");
	roa -> roa_addr = *sa;	/* struct copy */

	if ((pe = int2prim (i = getpid ())) == NULLPE)
	    adios (NULLCP, "unable to allocate hello");

	fprintf (stderr, "%s... ", addr);
	(void) fflush (stderr);
	if (RoBeginRequest (roa, pe, roc, roi) == NOTOK) {
	    fprintf (stderr, "failed\n");
	    ros_adios (rop, "RO-BEGIN.REQUEST");
	}

	pe_free (pe);

	if (roc -> roc_result != ROS_ACCEPT) {
	    fprintf (stderr, "failed\n");
	    adios (NULLCP, "association rejected: [%s]",
		    RoErrString (roc -> roc_result));
	}
	fprintf (stderr, "connected\n");

#ifdef	DEBUG
	advise (NULLCP, "sent greetings of %d", i);
#endif

	sd = roc -> roc_sd;
	if (roc -> roc_data) {
	    if ((i = prim2num (roc -> roc_data)) == NOTOK
		    && roc -> roc_data -> pe_errno != PE_ERR_NONE)
		adios (NULLCP, "error decoding hello: %s (%d)",
			pe_error (roc -> roc_data -> pe_errno), i);
#ifdef	DEBUG
	    advise (NULLCP, "received greetings of %d", i);
#endif
	}
	ROCFREE (roc);
    }

    do_ros (sd);
}

/*  */

static int  do_ros (sd)
int	sd;
{
    int     cc,
            i,
            j;
    char   *cp,
           *dp,
            buffer[BUFSIZ];
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject   *rop = &roi -> roi_preject;
    struct AcSAPrelease acrs;
    register struct AcSAPrelease *acr = &acrs;
    register PE	pe;
    struct stat st;
    
    if (fstat (fileno (stdin), &st) != NOTOK
	    && (st.st_mode & S_IFMT) == S_IFREG
	    && (cc = st.st_size) != 0) {
	(void) lseek (fileno (stdin), 0L, 0);

	if ((cp = malloc ((unsigned) cc)) == NULL)
	    adios (NULLCP, "no memory");
	for (dp = cp, j = cc; j > 0; dp += i, j -= i)
	    switch (i = read (fileno (stdin), dp, j)) {
		case NOTOK: 
		    adios ("on stdin", "read failed");

		case OK: 
		    adios (NULLCP, "premature end-of-file");

		default: 
		    break;
	    }
	if ((pe = oct2prim (cp, cc)) == NULLPE)
	    adios (NULLCP, "unable to allocate invocation argument");
	free (cp);
	for (i = 10; i > 0; i--) {
#ifdef	TIMER
	    timer (0);
#endif
	    ros_invokerequest (sd, pe);
#ifdef	TIMER
	    timer (cc);
#endif
	}
	pe_free (pe);
    }
    else
	while (fgets (buffer, sizeof buffer, stdin)) {
	    if ((pe = oct2prim (buffer, strlen (buffer) + 1)) == NULLPE)
		adios (NULLCP, "unable to allocate invocation argument");
	    ros_invokerequest (sd, pe);
	    pe_free (pe);
	}

    if (isrts) {
	struct RtSAPindication  rtis;
	register struct RtSAPindication *rti = &rtis;
	register struct RtSAPabort *rta = &rti -> rti_abort;

	if (isacs) {
	    if (RtCloseRequest (sd, ACF_NORMAL, NULLPE, acr, rti) == NOTOK)
		switch (rta -> rta_reason) {
		    case RTS_OPERATION:
		    case RTS_WAITING: 
			rts_waitfor (sd);
			if (RtCloseRequest (sd, ACF_NORMAL, NULLPE, acr, rti)
				== OK)
			    break;	/* else fall */

		    default:
			rts_adios (rta, "RT-CLOSE.REQUEST");
		}

	    if (!acr -> acr_affirmative) {
		(void) RtUAbortRequest (sd, NULLPE, rti);
		adios (NULLCP, "release rejected by peer: %d, %d elements",
			    acr -> acr_reason, acr -> acr_ninfo);
	    }
	}
	else
	    if (RtEndRequest (sd, rti) == NOTOK)
		switch (rta -> rta_reason) {
		    case RTS_OPERATION: 
		    case RTS_WAITING: 
			rts_waitfor (sd);
			if (RtEndRequest (sd, rti) == OK)
			    break;	/* else fall */

		    default: 
			rts_adios (rta, "RT-END.REQUEST");
		}
    }
    else
	if (isacs) {
	    struct AcSAPindication  acis;
	    register struct AcSAPindication *aci = &acis;
	    register struct AcSAPabort *aca = &aci -> aci_abort;

	    if (AcRelRequest (sd, ACF_NORMAL, NULLPEP, 0, NOTOK, acr, aci)
		    == NOTOK)
		acs_adios (aca, "A-RELEASE.REQUEST");

	    if (!acr -> acr_affirmative) {
		(void) AcUAbortRequest (sd, NULLPEP, 0, aci);
		adios (NULLCP, "release rejected by peer: %d, %d elements",
			    acr -> acr_reason, acr -> acr_ninfo);
	    }
	    ACRFREE (acr);
	}
	else
	    if (RoEndRequest (sd, ROS_NOPRIO, roi) == NOTOK)
		ros_adios (rop, "RO-END.REQUEST");
}

/*  */

static int  ros_invokerequest (sd, pe)
int	sd;
PE	pe;
{
    int     result;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject   *rop = &roi -> roi_preject;
    static int  id = 0;
    static int  op = 0;

    switch (result = RoInvokeRequest (sd, op++, ROS_SYNC, pe, ++id, NULLIP,
		ROS_NOPRIO, roi)) {
	case NOTOK: 
	    if (ROS_FATAL (rop -> rop_reason))
		ros_adios (rop, "RO-INVOKE.REQUEST");
	    ros_advise (rop, "RO-INVOKE.REQUEST");
	    break;

	case OK: 
	    switch (roi -> roi_type) {
		case ROI_INVOKE: 
		    adios (NULLCP, "got RO-INVOKE.INDICATION");

		case ROI_RESULT: 
		    {
			register struct RoSAPresult *ror = &roi -> roi_result;

			if (ror -> ror_id != id) {
			    advise (NULLCP, "id mismatch (wanted %d, got %d)",
				    id, ror -> ror_id);
			    status++;
			    if (RoURejectRequest (sd, &ror -> ror_id,
				    ROS_RRP_UNRECOG, ROS_NOPRIO, roi) == NOTOK)
				ros_adios (rop, "RO-REJECT-U.REQUEST");
			}
			else
			    if (mode == echo
				    && pe_cmp (pe, ror -> ror_result)) {
				advise (NULLCP, "data mismatch (4)");
				vunknown (pe);
		        	advise (NULLCP, "---------");
				vunknown (ror -> ror_result);
		        	advise (NULLCP, "---------");
				status++;
			    }

			RORFREE (ror);
		    }
		    break;

		case ROI_ERROR: 
		    {
			register struct RoSAPerror *roe = &roi -> roi_error;

			if (roe -> roe_id != id) {
			    advise (NULLCP, "id mismatch (wanted %d, got %d)",
				    id, roe -> roe_id);
			    status++;
			    if (RoURejectRequest (sd, &roe -> roe_id,
				    ROS_REP_UNRECOG, ROS_NOPRIO, roi) == NOTOK)
				ros_adios (rop, "RO-REJECT-U.REQUEST");
			}
			else
			    if (mode == echo
				    && pe_cmp (pe, roe -> roe_param)) {
				advise (NULLCP, "data mismatch (5)");
				vunknown (pe);
		        	advise (NULLCP, "---------");
				vunknown (roe -> roe_param);
		        	advise (NULLCP, "---------");
				status++;
			    }

			ROEFREE (roe);
		    }
		    break;

		case ROI_UREJECT: 
		    {
			register struct RoSAPureject *rou = &roi -> roi_ureject;

			if (rou -> rou_noid)
			    advise (NULLCP, "RO-REJECT-U.INDICATION: %s",
				    RoErrString (rou -> rou_reason));
			else
			    advise (NULLCP, "RO-REJECT-U.INDICATION: %s (id=%d)",
				    RoErrString (rou -> rou_reason),
				    rou -> rou_id);
			if (!rou -> rou_noid && rou -> rou_id != id) {
			    advise (NULLCP, "id mismatch (wanted %d, got %d)",
				    id, rou -> rou_id);
			    status++;
			}
		    }
		    break;

		default: 
		    adios (NULLCP, "unknown indication type=%d",
			    roi -> roi_type);
	    }
	    if (isrts)
		turn = 0;
	    break;

	case DONE: 
	    adios (NULLCP, "got RO-END.INDICATION");

	default: 
	    adios (NULLCP, "unknown return from RoInvokeRequest=%d", result);
    }
}


/*  */

static void  ros_adios (rop, event)
register struct RoSAPpreject *rop;
char   *event;
{
    ros_advise (rop, event);

    _exit (1);
}


static void  ros_advise (rop, event)
register struct RoSAPpreject *rop;
char   *event;
{
    char    buffer[BUFSIZ];

    if (rop -> rop_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s", RoErrString (rop -> rop_reason),
		rop -> rop_cc, rop -> rop_cc, rop -> rop_data);
    else
	(void) sprintf (buffer, "[%s]", RoErrString (rop -> rop_reason));

    advise (NULLCP, "%s: %s", event, buffer);
}

/*    TIMER */

#ifdef	TIMER

#ifndef	NBBY
#define	NBBY	8
#endif


#ifndef	TMS
static  timer (cc)
int     cc;
{
    int     bytes;
    long    ms;
    float   bs;
    struct timeval  stop,
                    td;
    static struct timeval   start;

    if (cc == 0) {
	(void) gettimeofday (&start, (struct timezone *) 0);
	return;
    }
    else
	(void) gettimeofday (&stop, (struct timezone  *) 0);

    tvsub (&td, &stop, &start);
    ms = (td.tv_sec * 1000) + (td.tv_usec / 1000);
    bytes = mode == echo ? cc * 2 : cc;
    bs = (((float) bytes * NBBY * 1000) / (float) (ms ? ms : 1)) / NBBY;

    advise (NULLCP, "%d bytes %s in %d.%02d seconds (%.2f Kbytes/s)",
	    cc, mode == echo ? "echoed" : "sunk",
	    td.tv_sec, td.tv_usec / 10000, bs / 1024);
}


static  tvsub (tdiff, t1, t0)
register struct timeval *tdiff,
			*t1,
			*t0;
{

    tdiff -> tv_sec = t1 -> tv_sec - t0 -> tv_sec;
    tdiff -> tv_usec = t1 -> tv_usec - t0 -> tv_usec;
    if (tdiff -> tv_usec < 0)
	tdiff -> tv_sec--, tdiff -> tv_usec += 1000000;
}
#else
long	times ();


static	timer (cc)
int	cc;
{
    int	    bytes;
    long    ms;
    float   bs;
    long    stop,
	    td,
	    secs,
	    msecs;
    struct tms tm;
    static long start;

    if (cc == 0) {
	start = times (&tm);
	return;
    }
    else
	stop = times (&tm);

    td = stop - start;
    secs = td / 60, msecs = (td % 60) * 1000 / 60;
    ms = (secs * 1000) +  msecs;
    bytes = mode == echo ? cc * 2 : cc;
    bs = (((float) bytes * NBBY * 1000) / (float) (ms ? ms : 1)) / NBBY;
    
    advise (NULLCP, "%d bytes %s in %d.%02d seconds (%.2f KBytes/s)",
	    cc, mode == echo ? "echoed" : "sunk",
	    secs, msecs / 10, bs / 1024);
}
#endif
#endif

/*    QBUF */

static int  qcmp (b, qb, l)
register char *b;
register struct qbuf *qb;
register int l;
{
    register struct qbuf   *qp;

    for (qp = qb -> qb_forw; qp != qb; qp = qp -> qb_forw) {
	if ((l -= qp -> qb_len) < 0) {
	    advise (NULLCP, "length mismatch(1)");
	    return NOTOK;
	}

	if (bcmp (b, qp -> qb_data, qp -> qb_len)) {
	    advise (NULLCP, "data mismatch (6)");
	    return NOTOK;
	}

	b += qp -> qb_len;
    }

    if (l != 0) {
	advise (NULLCP, "length mismatch(2)");
	return NOTOK;
    }

    return OK;
}

/*    ERRORS */

#ifndef	lint
void	_advise ();


void	adios (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _advise (ap);

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
    va_list ap;

    va_start (ap);

    _advise (ap);

    va_end (ap);
}


static void  _advise (ap)
va_list	ap;
{
    char    buffer[BUFSIZ];

    asprintf (buffer, ap);

    (void) fflush (stdout);

    fprintf (stderr, "%s: ", myname);
    (void) fputs (buffer, stderr);
    (void) fputc ('\n', stderr);

    (void) fflush (stderr);
}
#else
/* VARARGS */

void	advise (what, fmt)
char   *what,
       *fmt;
{
    advise (what, fmt);
}
#endif
