/* tsapd.c - OSI transport listener */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/support/RCS/tsapd.c,v 7.12 91/02/22 09:46:59 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/support/RCS/tsapd.c,v 7.12 91/02/22 09:46:59 mrose Interim $
 *
 *
 * $Log:	tsapd.c,v $
 * Revision 7.12  91/02/22  09:46:59  mrose
 * Interim 6.8
 * 
 * Revision 7.11  90/12/19  09:16:14  mrose
 * touch-up
 * 
 * Revision 7.10  90/12/17  22:13:27  mrose
 * -call
 * 
 * Revision 7.9  90/12/11  10:52:29  mrose
 * lock-and-load
 * 
 * Revision 7.8  90/11/05  14:10:32  mrose
 * oops
 * 
 * Revision 7.7  90/10/30  14:25:32  mrose
 * update
 * 
 * Revision 7.6  90/10/29  18:37:25  mrose
 * updates
 * 
 * Revision 7.5  90/10/16  11:21:04  mrose
 * update
 * 
 * Revision 7.4  90/10/15  22:54:26  mrose
 * typo
 * 
 * Revision 7.3  90/10/15  18:18:47  mrose
 * iaed
 * 
 * Revision 7.2  90/07/09  14:51:00  mrose
 * sync
 * 
 * Revision 7.1  90/02/19  13:09:53  mrose
 * update
 * 
 * Revision 7.0  89/11/23  22:27:46  mrose
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
#include "manifest.h"
#include <sys/ioctl.h>
#include <sys/stat.h>
#ifdef	BSD42
#include <sys/file.h>
#endif
#ifdef	SYS5
#include <fcntl.h>
#endif
#ifndef	X_OK
#define	X_OK	1
#endif

#ifdef	IAE
#include <quipu/util.h>
#include <quipu/bind.h>
#include <quipu/list.h>
#include <quipu/ds_search.h>

#define	NOGOSIP
#endif

#ifndef	NOGOSIP
#include "rosap.h"
#include "rtsap.h"
#include "psap2.h"
#include "ssap.h"
#endif
#include "tpkt.h"

#ifdef	TCP
#include "internet.h"
#endif
#ifdef	X25
#include "x25.h"
#endif
#ifdef	TP4
#include "tp4.h"
#endif
#ifndef	IAE
#include "isoservent.h"
#endif
#include "tailor.h"

/*  */

static int  debug = 0;
static int  nbits = FD_SETSIZE;

static LLog _pgm_log = {
#ifndef	IAE
    "tsapd.log",
#else
    "iaed.log",
#endif
    NULLCP, NULLCP, LLOG_FATAL | LLOG_EXCEPTIONS | LLOG_NOTICE,
    LLOG_FATAL, -1, LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *pgm_log = &_pgm_log;

static char *pgmname = "tsapd";
static char myhost[BUFSIZ];

static int	tcpservice = 1;
static int	x25service = 1;
static int	bridgeservice = 1;
static int	tp4service = 1;

static int     *services[] = {
    &tp4service, &tcpservice, &x25service, &bridgeservice
};


#define	NTADDRS	FD_SETSIZE

static	int	listening = 0;

static struct TSAPaddr *tz;
static struct TSAPaddr  tas[NTADDRS];


#ifdef	IAE
#define	IAETIME	(24 * 60 * 60L)

static	int	isbound = 0;
static	int	main_dsa = NOTOK;
static	int	referral_dsa = NOTOK;

static	long	nextime;

static	DN	userdn = NULL;
static	char	passwd[DBA_MAX_PASSWD_LEN];

static	AttributeType t_ev;
static	AttributeType t_pa;

static	struct ds_search_arg search_arg;

struct IAEntry {
    struct TSAPaddr is_addr;

    char    *is_vector;
    char   **is_vec;
    char   **is_tail;
};

#define	NENTRIES	100
static	struct IAEntry *iz;
static	struct IAEntry  iae[NENTRIES];


int	str2dnY ();


extern	int	dsa_ad;
extern	int	dsa_dead;
extern	char   *local_dit;
extern	struct PSAPaddr dsa_bound;

extern	int	as_print (), dn_print (), de_print (), fi_print ();
#endif


void	adios (), advise ();
void	ts_advise ();

#ifdef	NOGOSIP
#define	ssapd	NULLIFP
#else
int	ssapd (), psapd ();
#endif


extern int  errno;

#ifdef	IAE
long	time ();
#endif

/*  */

/* ARGSUSED */

main (argc, argv, envp)
int     argc;
char  **argv,
      **envp;
{
    int	    failed,
	    vecp;
    char   *vec[4];
    register struct TSAPaddr  *ta;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect  *td = &tds;

    arginit (argv);
    envinit ();

    failed = 0;

    for (ta = tas; ta < tz; ta++) {
	register struct NSAPaddr *na;

	if (ta -> ta_naddr) {
	    if (((na = ta -> ta_addrs) -> na_stack < 0
		     || na -> na_stack
				    >= sizeof services / sizeof services[0]))
		adios (NULLCP, "unknown network type 0x%x", na -> na_stack);
	}
	else
	    na = NULLNA;
	    
	if (!*services[na ? na -> na_stack : NA_NSAP])
	    continue;

	advise (LLOG_NOTICE, NULLCP, "listening on %s", taddr2str (ta));

	if (TNetListen (ta, td) == NOTOK) {
	    ts_advise (td, LLOG_EXCEPTIONS, "TNetListen failed");
	    failed++;
	}
	else
	    listening++;
    }

    if (!listening)
	adios (NULLCP, failed ? "no successful listens"
			      : "no network services selected");

    for (;;) {
#ifdef	IAE
	int	secs;
	long	now;

	(void) time (&now);
	now++;

	if ((secs = (int) (nextime - now)) <= 0) {
	    search_directory (0);

	    secs = IAETIME;
	}
#else
#define	secs	NOTOK
#endif

	if (TNetAccept (&vecp, vec, 0, NULLFD, NULLFD, NULLFD, secs, td)
		== NOTOK) {
	    ts_advise (td, LLOG_EXCEPTIONS, "TNetAccept failed");
	    continue;
	}

	if (vecp <= 0)
	    continue;

	if (debug)
	    break;

	switch (TNetFork (vecp, vec, td)) {
	    case OK:
#ifdef	IAE
		(void) signal (SIGHUP, SIG_DFL);
#endif
	        ll_hdinit (pgm_log, pgmname);
		break;

	    case NOTOK:
		ts_advise (td, LLOG_EXCEPTIONS, "TNetFork failed");
	    default:
		continue;
	}
	break;
    }

    tsapd (vecp, vec);
}

/*  */

static char buffer1[4096];
static char buffer2[32768];


static int  tsapd (vecp, vec)
int	vecp;
char  **vec;
{
    char    buffer[BUFSIZ];
#ifndef	IAE
    register struct isoservent *is;
#else
    register struct IAEntry *is;
#endif
    struct tsapblk *tb;
    struct TSAPstart   tss;
    register struct TSAPstart *ts = &tss;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect  *td = &tds;
    IFP	    hook;

/* begin UGLY */
    (void) strcpy (buffer1, vec[1]);
    (void) strcpy (buffer2, vec[2]);
/* end UGLY */

    if (TInit (vecp, vec, ts, td) == NOTOK) {
	ts_advise (td, LLOG_EXCEPTIONS, "T-CONNECT.INDICATION");
	return;
    }

/* used to print this in ssapd()... */
    advise (LLOG_NOTICE, NULLCP,
	    "T-CONNECT.INDICATION: <%d, %s, %s, %d, %d>",
	    ts -> ts_sd,
	    taddr2str (&ts -> ts_calling), taddr2str (&ts -> ts_called),
	    ts -> ts_expedited, ts -> ts_tsdusize);

    hook = ssapd;
    if (ts -> ts_called.ta_selectlen) {
#ifndef	IAE
	if ((is = getisoserventbyselector ("tsap", ts -> ts_called.ta_selector,
					   ts -> ts_called.ta_selectlen))
		== NULL) {
#else
	for (is = iae; is < iz; is++)
	    if (is -> is_addr.ta_selectlen == ts -> ts_called.ta_selectlen
		    && bcmp (is -> is_addr.ta_selector,
			     ts -> ts_called.ta_selector,
			     is -> is_addr.ta_selectlen) == 0)
		break;
	if (is >= iz) {
#endif
	    (void) sprintf (buffer, "OSI service tsap/%s not found",
			    sel2str (ts -> ts_called.ta_selector,
				     ts -> ts_called.ta_selectlen, 1));
	    goto out;
	}
    }
    else
#ifndef	IAE
	if (hook == NULLIFP
	        || (is = getisoserventbyname ("session", "tsap")) == NULL)
#endif
       {
	    (void) strcpy (buffer, "default session service not found");
	    goto out;
	}

#ifndef	IAE
    *is -> is_tail++ = buffer1;
    *is -> is_tail++ = buffer2;
    *is -> is_tail = NULL;
#else
    is -> is_tail[0] = buffer1;
    is -> is_tail[1] = buffer2;
    is -> is_tail[2] = NULL;
#endif

    if (tb = findtblk (ts -> ts_sd))
	tb -> tb_fd = NOTOK;
    switch (hook ? (*hook) (is, td) : OK) {
	case NOTOK:
	    ts_advise (td, LLOG_EXCEPTIONS, "service not started at tsap");
	case DONE:
	    exit (1);
	    /* NOTREACHED */

	case OK:
	default:
	    (void) setperms (is);
	    (void) execv (*is -> is_vec, is -> is_vec);
	    (void) sprintf (buffer, "unable to exec %s: %s",
			    *is -> is_vec, sys_errname (errno));
	    SLOG (pgm_log, LLOG_FATAL, NULLCP, ("%s", buffer));
	    break;
    }

out: ;
    advise (LLOG_EXCEPTIONS, NULLCP, "%s", buffer);
    if (strlen (buffer) >= TD_SIZE)
	buffer[0] = NULL;
    (void) TDiscRequest (ts -> ts_sd, buffer, strlen (buffer) + 1, td);

    exit (1);
}

/*  */

static int  setperms (is)
#ifndef	IAE
register struct isoservent *is;
#else
register struct IAEntry *is;
#endif
{
    struct stat st;

    if (stat (*is -> is_vec, &st) == NOTOK) {
	(void) setgid (1);
	(void) setuid (1);
    }
    else {
	(void) setgid (st.st_gid);
	(void) setuid (st.st_uid);
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

/*  */

#ifndef	NOGOSIP
static int  ssapd (is, td)
register struct isoservent *is;
register struct TSAPdisconnect *td;
{
    int	    sd;
    struct TSAPstart    tss;
    register struct TSAPstart  *ts = &tss;
    struct SSAPindication sis;
    register struct SSAPabort *sa = &sis.si_abort;

    if (strcmp (is -> is_entity, "session")
	    || strcmp (is -> is_provider, "tsap"))
	return OK;

    if (TInit (is -> is_tail - is -> is_vec, is -> is_vec, ts, td) == NOTOK)
	return NOTOK;

    sd = ts -> ts_sd;

    if (TConnResponse (sd, &ts -> ts_called, ts -> ts_expedited, NULLCP, 0,
		       NULLQOS, td) == NOTOK)
	return NOTOK;

    if (SExec (ts, &sis, psapd, setperms) == NOTOK) {
	advise (LLOG_EXCEPTIONS, NULLCP, "service not started at ssap: %s",
		SErrString (sa -> sa_reason));
	if (sa -> sa_cc > 0)
	    advise (LLOG_EXCEPTIONS, NULLCP, "   %*.*s",
		    sa -> sa_cc, sa -> sa_cc, sa -> sa_data);

	SAFREE (sa);
    }
		
    return DONE;
}

/*  */

#define	RMASK \
    "\020\01HALFDUPLEX\02DUPLEX\03EXPEDITED\04MINORSYNC\05MAJORSYNC\06RESYNC\
\07ACTIVITY\010NEGOTIATED\011CAPABILITY\012EXCEPTIONS\013TYPEDATA"


static int  psapd (is, si)
register struct isoservent *is;
register struct SSAPindication *si;
{
    struct SSAPstart    sss;
    register struct SSAPstart  *ss = &sss;
    struct PSAPindication  pis;
    register struct PSAPabort *pa = &pis.pi_abort;
    struct RtSAPindication  rtis;
    register struct RtSAPabort *rta = &rtis.rti_abort;
    struct RoSAPindication  rois;
    register struct RoSAPpreject   *rop = &rois.roi_preject;

    if (strcmp (is -> is_provider, "ssap"))
	return OK;

    if (strcmp (is -> is_entity, "presentation")
	    && strcmp (is -> is_entity, "rts")
	    && strcmp (is -> is_entity, "ros"))
	return OK;

/* begin UGLY */
    (void) strcpy (buffer1, *(is -> is_tail - 2));
    (void) strcpy (buffer2, *(is -> is_tail - 1));
/* end UGLY */
    if (SInit (is -> is_tail - is -> is_vec, is -> is_vec, ss, si) == NOTOK)
	return NOTOK;
    advise (LLOG_NOTICE, NULLCP,
	    "S-CONNECT.INDICATION: <%d, %s, %s, %s, %s, %ld, %d>",
	    ss -> ss_sd, sprintref (&ss -> ss_connect),
	    saddr2str (&ss -> ss_calling), saddr2str (&ss -> ss_called),
	    sprintb (ss -> ss_requirements, RMASK), ss -> ss_isn,
	    ss -> ss_ssdusize);

    if (strcmp (is -> is_entity, "presentation") == 0) {
	if (PExec (ss, &pis, buffer1, buffer2, NULLIFP, setperms) == NOTOK) {
		advise (LLOG_EXCEPTIONS, NULLCP,
			"service not started at psap: %s",
			PErrString (pa -> pa_reason));
		if (pa -> pa_cc > 0)
		    advise (LLOG_EXCEPTIONS, NULLCP, "   %*.*s",
			pa -> pa_cc, pa -> pa_cc, pa -> pa_data);
	}

	return DONE;
    }

    if (strcmp (is -> is_entity, "rts") == 0) {
	if (RtExec (ss, &rtis, buffer1, buffer2, NULLIFP, setperms) == NOTOK) {
	    advise (LLOG_EXCEPTIONS, NULLCP,
		    "service not started at rtsap: %s",
		    RtErrString (rta -> rta_reason));
	    if (rta -> rta_cc > 0)
		advise (LLOG_EXCEPTIONS, NULLCP, "   %*.*s",
			rta -> rta_cc, rta -> rta_cc, rta -> rta_data);
	}
    }
    else {
	if (RoExec (ss, &rois, buffer1, buffer2, NULLIFP, setperms) == NOTOK) {
	    advise (LLOG_EXCEPTIONS, NULLCP,
		    "service not started at rosap: %s",
		    RoErrString (rop -> rop_reason));
	    if (rop -> rop_cc > 0)
		advise (LLOG_EXCEPTIONS, NULLCP, "   %*.*s",
			rop -> rop_cc, rop -> rop_cc, rop -> rop_data);
	}
    }

    return DONE;
}
#endif

/*  */

#ifndef	IAE
static	arginit (vec)
char	**vec;
{
    int	    rflag;
    register char  *ap;
#ifdef	TCP
    int	    port;
    struct NSAPaddr *tcp_na;
    register struct servent *sp;
#endif
#ifdef	X25
    struct NSAPaddr *x25_na;
#endif
#ifdef	BRIDGE_X25
    struct NSAPaddr *bridge_na;
#endif
#ifdef	TP4
    register struct isoservent *is;
#endif
    struct stat st;

    if (pgmname = rindex (*vec, '/'))
	pgmname++;
    if (pgmname == NULL || *pgmname == NULL)
	pgmname = *vec;

    isodetailor (pgmname, 0);
    ll_hdinit (pgm_log, pgmname);

    rflag = 0;

    (void) strcpy (myhost, TLocalHostName ());

    bzero ((char *) tas, sizeof tas);
    tz = tas;

#ifdef	TCP
    if (!(ts_stacks & TS_TCP))
	tcpservice = 0;
    if ((sp = getservbyname ("tsap", "tcp")) == NULL
	    && (sp = getservbyname ("iso-tsap", "tcp")) == NULL)
	advise (LLOG_EXCEPTIONS, NULLCP, "tcp/tsap: unknown service");

    tcp_na = tz -> ta_addrs;
    tcp_na -> na_stack = NA_TCP;
    tcp_na -> na_community = ts_comm_tcp_default;
    tcp_na -> na_domain[0] = NULL;
    tcp_na -> na_port = sp ? sp -> s_port : htons ((u_short) 102);
    tz -> ta_naddr = 1;

    tz++;
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
    if (x25_local_pid && *x25_local_pid)
	x25_na -> na_pidlen =
	    str2sel (x25_local_pid, -1, x25_na -> na_pid, NPSIZE);
    tz -> ta_naddr = 1;

    tz++;
#endif

#ifdef	BRIDGE_X25
    if (!(ts_stacks & TS_BRG))
	bridgeservice = 0;

    bridge_na = tz -> ta_addrs;
    bridge_na -> na_stack = NA_BRG;
    bridge_na -> na_community = ts_comm_x25_default;
    if (x25_bridge_listen && *x25_bridge_listen) {
	(void) strcpy (bridge_na -> na_dte, x25_bridge_listen);
	bridge_na -> na_dtelen = strlen (bridge_na -> na_dte);
    }
    if (x25_bridge_pid && *x25_bridge_pid)
	bridge_na -> na_pidlen =
	    str2sel (x25_bridge_pid, -1, bridge_na -> na_pid, NPSIZE);
    tz -> ta_naddr = 1;

    tz++;
#endif

#ifdef	TP4
    if (!(ts_stacks & TS_TP4))
	tp4service = 0;

    (void) setisoservent (0);
    while (is = getisoservent ())
	if (strcmp (is -> is_provider, "tsap") == 0
		&& (strcmp (*is -> is_vec, "tsapd-bootstrap") == 0
			|| access (*is -> is_vec, X_OK) != NOTOK)) {
	    if (strcmp (is -> is_entity, "isore") == 0)
		continue;

	    if (tz >= tas + NTADDRS) {
		advise (LLOG_EXCEPTIONS, NULLCP,
			"too many services, starting with %s",
			is -> is_entity);
		break;
	    }

	    bcopy (is -> is_selector, tz -> ta_selector,
		   tz -> ta_selectlen = is -> is_selectlen);
	    tz -> ta_naddr = 0;

	    tz++;
	}
    (void) endisoservent ();
#endif

    for (vec++; ap = *vec; vec++) {
	if (*ap == '-')
	    switch (*++ap) {
		case 'd':
		    debug++;
		    continue;
		
		case 't': 
		    ts_stacks = TS_TCP;
		    tcpservice = 1;
		    x25service = bridgeservice = tp4service = 0;
		    continue;

		case 'x': 
		    ts_stacks = TS_X25;
		    x25service = 1;
		    tcpservice = bridgeservice = tp4service = 0;
		    continue;

		case 'z': 
		    ts_stacks = TS_TP4;
		    tp4service = 1;
		    tcpservice = x25service = bridgeservice = 0;
		    continue;

		case 'b':
		    ts_stacks = TS_BRG;
		    bridgeservice = 1;
		    tcpservice = x25service = tp4service = 0;
		    continue;

		case 'r':
		    rflag = 1;
		    continue;

#ifdef	TCP
		case 'p': 
		    if ((ap = *++vec) == NULL
			    || *ap == '-'
			    || (port = atoi (ap)) <= 0)
			adios (NULLCP, "usage: %s -p portno", pgmname);
		    tcp_na -> na_port = htons ((u_short) port);
		    continue;
#endif

#ifdef X25
		/* This permits listening on a specific subaddress. */
		case 'a':
		    if ((ap = *++vec) == NULL || *ap == '-')
			adios (NULLCP, "usage: %s -a x121address", pgmname);
		    (void) strcpy (x25_na -> na_dte, ap);
		    x25_na -> na_dtelen = strlen (ap);
		    continue;

		/* This permits listening on a specific protocol id.
		   In fact, SunLink X.25 lets you listen on a protocol
		   id mask, but let's keep it simple. */
		case 'i':
		    if ((ap = *++vec) == NULL || *ap == '-' )
			adios (NULLCP, "usage: %s -i pid", pgmname);
		    x25_na -> na_pidlen =
			str2sel (ap, -1, x25_na -> na_pid, NPSIZE);
		    continue;
#endif

#ifdef BRIDGE_X25
		case 'A':
		    if ((ap = *++vec) == NULL ||
			*ap == '-')
			adios (NULLCP, "usage: %s -A x121address", pgmname);
		    (void) strcpy (bridge_na -> na_dte, ap);
		    bridge_na -> na_dtelen = strlen (ap);
		    continue;

		case 'I':
		    if ((ap = *++vec) == NULL || *ap == '-')
			adios (NULLCP, "usage: %s -I pid", pgmname);
		    bridge_na -> na_pidlen =
			str2sel (ap, -1, bridge_na -> na_pid, NPSIZE);
		    continue;
#endif

		default: 
		    adios (NULLCP, "-%s: unknown switch", ap);
	    }

	adios (NULLCP, "usage: %s [switches]", pgmname);
    }

    if (!rflag
	    && getuid () == 0
	    && stat (ap = isodefile ("isoservices", 0), &st) != NOTOK
	    && st.st_uid != 0)
	adios (NULLCP, "%s not owned by root", ap);
}

/*  */

#else
static	arginit (vec)
char	**vec;
{
    int	    argp,
	    options;
    register char *ap;
    char    base[BUFSIZ],
	  **argptr,
	   *args[4];

    if (pgmname = rindex (*vec, '/'))
	pgmname++;
    if (pgmname == NULL || *pgmname == NULL)
	pgmname = *vec;

    isodetailor (pgmname, 0);
    ll_hdinit (pgm_log, pgmname);

    quipu_syntaxes ();

    argp = 0;
    args[argp++] = pgmname;
    for (argptr = vec, argptr++; ap = *argptr; argptr++) {
	if (*ap == '-')
	    switch (*++ap) {
		case 'D':
		case 'u':
		case 'p':
		    if ((ap = *++argptr) == NULL || *ap == '-')
			break;
		    continue;

		case 'c':
		    if ((ap = *++argptr) == NULL || *ap == '-')
			break;
		    args[argp++] = "-c";
		    args[argp++] = ap;
		    break;

		default:
		    continue;
	     }

	break;
    }
    args[argp] = NULLCP;

    dsap_init (&argp, (argptr = args, &argptr));

    (void) strcpy (myhost, TLocalHostName ());
    (void) strcpy (base, local_dit);

    if (!(ts_stacks & TS_TCP))
	tcpservice = 0;
    if (!(ts_stacks & TS_X25))
	x25service = 0;
    if (!(ts_stacks & TS_BRG))
	bridgeservice = 0;
    if (!(ts_stacks & TS_TP4))
	tp4service = 0;

    options = SVC_OPT_PREFERCHAIN;
    userdn = NULLDN, passwd[0] = NULL;
    for (vec++; ap = *vec; vec++) {
	if (*ap == '-')
	    switch (*++ap) {
		case 'd':
		    debug++;
		    ll_dbinit (pgm_log, pgmname);
		    continue;
		
		case 't': 
		    ts_stacks = TS_TCP;
		    tcpservice = 1;
		    x25service = bridgeservice = tp4service = 0;
		    continue;

		case 'x': 
		    ts_stacks = TS_X25;
		    x25service = 1;
		    tcpservice = bridgeservice = tp4service = 0;
		    continue;

		case 'z': 
		    ts_stacks = TS_TP4;
		    tp4service = 1;
		    tcpservice = x25service = bridgeservice = 0;
		    continue;

		case 'b':
		    ts_stacks = TS_BRG;
		    bridgeservice = 1;
		    tcpservice = x25service = tp4service = 0;
		    continue;

		case 'r':	/* ignored... */
		    continue;

		case 'D':
		    if ((ap = *++vec) == NULL || *ap == '-')
			adios (NULLCP, "usage: %s -D DIT", pgmname);
		    if (*ap == '@')
			(void) strcpy (base, ap);
		    else
			(void) sprintf (base, "%s@%s", local_dit, ap);
		    continue;

		case 'm':
		    options |= SVC_OPT_DONTUSECOPY;
		    continue;

		case 'c':
		    if ((ap = *++vec) == NULL || *ap == '-')
			adios (NULLCP, "usage: %s -c DSA-name-or-address",
			       pgmname);
		    continue;

		case 'u':
		    if ((ap = *++vec) == NULL || *ap == '-')
			adios (NULLCP, "usage: %s -u username", pgmname);
		    if ((userdn = str2dn (*ap != '@' ? ap : ap + 1)) == NULLDN)
			adios (NULLCP, "invalid DN for username: %s", ap);
		    bzero ((char *) ap, strlen (ap));
		    continue;

		case 'p':
		    if ((ap = *++vec) == NULL || *ap == '-')
			adios (NULLCP, "usage: %s -p password", pgmname);
		    (void) strcpy (passwd, ap);
		    bzero ((char *) ap, strlen (ap));
		    continue;

		default: 
		    adios (NULLCP, "-%s: unknown switch", ap);
	    }

	adios (NULLCP, "usage: %s [switches]", pgmname);
    }

    {
	Attr_Sequence as;
	AttributeType t_oc;
	DN	    local_dn;
	register Filter	fi;
	register struct ds_search_arg *sa = &search_arg;

	if ((t_ev = str2AttrT ("execVector")) == NULL)
	    adios (NULLCP, "unknown attribute type \"%s\"", "execVector");
	if ((t_oc = str2AttrT ("objectClass")) == NULL)
	    adios (NULLCP, "unknown attribute type \"%s\"", "objectClass");
	if ((t_pa = str2AttrT ("presentationAddress")) == NULL)
	    adios (NULLCP, "unknown attribute type \"%s\"",
		   "presentationAddress");

	if (str2dnY (*base != '@' ? base : base + 1, &local_dn) == NOTOK)
	    adios (NULLCP, "DIT subtree invalid: \"%s\"", base);

	fi = filter_alloc ();
	bzero ((char *) fi, sizeof *fi);

	fi -> flt_type = FILTER_ITEM;
	fi -> FUITEM.fi_type = FILTERITEM_EQUALITY;
	fi -> FUITEM.UNAVA.ava_type = AttrT_cpy (t_oc);
	if ((fi -> FUITEM.UNAVA.ava_value =
			str2AttrV ("iSODEApplicationEntity",
				   fi -> FUITEM.UNAVA.ava_type -> oa_syntax))
	        == NULL)
	    adios (NULLCP, "unknown attribute value \"%s\" for \"%s\"",
		   "iSODEApplicationEntity", "objectClass");

	as = as_merge (as_comp_new (AttrT_cpy (t_ev), NULLAV, NULLACL_INFO),
		       as_comp_new (AttrT_cpy (t_pa), NULLAV, NULLACL_INFO));

	bzero ((char *) sa, sizeof *sa);

	sa -> sra_common.ca_servicecontrol.svc_options = options;
	sa -> sra_common.ca_servicecontrol.svc_timelimit = SVC_NOTIMELIMIT;
	sa -> sra_common.ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT;
	sa -> sra_baseobject = local_dn;
	sa -> sra_subset = SRA_WHOLESUBTREE;
	sa -> sra_filter = fi;
	sa -> sra_searchaliases = TRUE;
	sa -> sra_eis.eis_allattributes = FALSE;
	sa -> sra_eis.eis_select = as;
	sa -> sra_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES;

	search_directory (1);
    }
}

/*  */

static	search_directory (firstime)
int	firstime;
{
    int	    i;
    register struct ds_search_arg *sa = &search_arg;
    struct ds_search_result search_result;
    register struct ds_search_result *sr = &search_result;
    struct DSError error;
    register struct DSError *se = &error;
    register EntryInfo *ptr;
    register struct IAEntry  *ia;
    register struct TSAPaddr *ta,
			     *tb,
			     *ty;
    struct TSAPaddr tys[NTADDRS];

    advise (LLOG_NOTICE, NULLCP,
	    "searching directory for iSODEApplicationEntity objects");

    while (rebind_to_directory () == NOTOK) {
	if (!firstime)
	    return;
	
	if (debug)
	    advise (LLOG_DEBUG, NULLCP, "sleeping for 5 minutes...");
	sleep ((unsigned) 5 * 60);
    }

    for (;;) {
	if (debug) {
	    pslog (pgm_log, LLOG_DEBUG, "performing subtree search of",
		   dn_print, (caddr_t) sa -> sra_baseobject);
	    pslog (pgm_log, LLOG_DEBUG, "  for", fi_print,
		   (caddr_t) sa -> sra_filter);
	}

	if (ds_search (sa, se, sr) == DS_OK)
	    break;
	if (do_error (se) == NOTOK) {
	    if (!firstime)
		return;
	    
	    adios (NULLCP, "search failed");
	}

	sa -> sra_baseobject =
			    se -> ERR_REFERRAL.DSE_ref_candidates -> cr_name;
    }

    if (sr -> srr_correlated != TRUE)
	correlate_search_results (sr);

    if (!firstime)
	for (ia = iae; ia < iz; ia++) {
	    free (ia -> is_vector);
	    free (ia -> is_vec[0]);
	    free ((char *) ia -> is_vec);
	}

    bzero ((char *) iae, sizeof iae);
    iz = iae;

    bzero ((char *) tys, sizeof tys);
    ty = tys;

    i = 0;
    for (ptr = sr -> CSR_entries; ptr; ptr = ptr -> ent_next) {
	Attr_Sequence eptr;
	AV_Sequence   avs;

	if (iz >= iae + NENTRIES) {
	    pslog (pgm_log, LLOG_EXCEPTIONS,
		   "too many services, starting with", dn_print,
		   (caddr_t) ptr -> ent_dn);
	    break;
	}

	if (debug) {
	    pslog (pgm_log, LLOG_DEBUG, "processing", dn_print,
		   (caddr_t) ptr -> ent_dn);
	    pslog (pgm_log, LLOG_DEBUG, "  attributes", as_print,
		   (caddr_t) ptr -> ent_attr);
	}

	for (eptr = ptr -> ent_attr; eptr; eptr = eptr -> attr_link) {
	    if (AttrT_cmp (eptr -> attr_type, t_pa) == 0) {
		if (avs = eptr -> attr_value) {
		    register struct PSAPaddr *pa =
				(struct PSAPaddr *) avs -> avseq_av.av_struct;

		    if ((ta = &pa -> pa_addr.sa_addr) -> ta_selectlen == 0
			    || pa -> pa_selectlen > 0
			    || pa -> pa_addr.sa_selectlen > 0)
			continue;
						
		    iz -> is_addr = *ta;	/* struct copy */
		}

		continue;
	    }
	    
	    if (AttrT_cmp (eptr -> attr_type, t_ev) == 0) {
		if (avs = eptr -> attr_value) {
		    int	    vecp;
		    register char  **vp;
		    char   *cp,
			   *evec[NVEC + NSLACK + 1];

		    cp = (char *) avs -> avseq_av.av_struct;
		    if ((iz -> is_vector =
				    malloc ((unsigned) (strlen (cp) + 1)))
			    == NULL)
			adios (NULLCP, "out of memory allocating iaeVector");
		    (void) strcpy (iz -> is_vector, cp);

		    if ((vecp = str2vec (iz -> is_vector, evec)) < 1)
			goto losing_iae;
		    if ((iz -> is_vec =
				    (char **) calloc ((unsigned) (vecp + 3),
						      sizeof *iz -> is_vec))
			    == NULL)
			adios (NULLCP, "out of memory allocating execVector");
		    iz -> is_tail = iz -> is_vec, vp = evec;
		    while (*iz -> is_tail++ = *vp++)
			continue;
		    iz -> is_tail--;

		    if (access (cp = isodefile (iz -> is_vec[0], 1), X_OK)
			    == NOTOK) {
			advise (LLOG_EXCEPTIONS, cp, "unable to find program");
			iz -> is_vec[0] = NULL;
			goto losing_iae;
		    }
		    if ((iz -> is_vec[0] =
					malloc ((unsigned) (strlen (cp) + 1)))
			    == NULL)
			adios (NULLCP, "out of memory allocating pgmVector");
		    (void) strcpy (iz -> is_vec[0], cp);
		}
		continue;
	    }
	}
	if ((ta = &iz -> is_addr) -> ta_selectlen == 0
	        || iz -> is_vector == NULL) {
	    pslog (pgm_log, LLOG_EXCEPTIONS, "invalid entry", dn_print,
		   (caddr_t) ptr -> ent_dn);

losing_iae: ;
	    if (iz -> is_vector)
		free (iz -> is_vector);
	    if (iz -> is_vec) {
		if (iz -> is_vec[0])
		    free (iz -> is_vec[0]);
		free ((char *) iz -> is_vec);
	    }
	    bzero ((char *) iz, sizeof *iz);
	    continue;
	}

	if (ta -> ta_naddr == 0)
	    *ty++ = *ta;			/* struct copy */
	else {
	    register int n = ta -> ta_naddr;
	    register struct NSAPaddr *na = ta -> ta_addrs;

	    for (; n > 0; na++, n--) {
		for (tb = tys; tb < ty; tb++)
		    if (tb -> ta_naddr != 0
			    && bcmp ((char *) na, (char *) tb -> ta_addrs,
				     sizeof *na) == 0)
			break;
		if (tb >= ty) {
		    if (na -> na_type == NA_NSAP)
			bcopy (ta -> ta_selector, ty -> ta_selector,
			       ty -> ta_selectlen = ta -> ta_selectlen);
		    else
			ty -> ta_selectlen = 0;
		    ty -> ta_naddr = 1;
		    ty -> ta_addrs[0] = *na;	/* struct copy */

		    ty++;
		}
	    }
	}

	for (ia = iae; ia < iz; ia++) {
	    tb = &ia -> is_addr;

	    if (ta -> ta_selectlen == tb -> ta_selectlen
		    && bcmp (ta -> ta_selector, tb -> ta_selector,
			     ta -> ta_selectlen)  == 0) {
		char    buffer[BUFSIZ];

		(void) strcpy (buffer, taddr2str (tb));
		advise (LLOG_EXCEPTIONS, NULLCP,
			"two services with the same transport selector: %s and %s",
			buffer, taddr2str (ta));
		pslog (pgm_log, LLOG_EXCEPTIONS, "starting with", dn_print,
		       (caddr_t) ptr -> ent_dn);
		adios (NULLCP, "you lose big");
	    }
	}

	iz++, i++;
    }

    if (sr -> CSR_cr) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"partial results only (not all DSAs could be reached)");
    }
    else
	if (sr -> CSR_limitproblem != LSR_NOLIMITPROBLEM) {
	    advise (LLOG_EXCEPTIONS, NULLCP, "%s limit exceeded",
		    sr -> CSR_limitproblem == LSR_TIMELIMITEXCEEDED
		     	    ? "time"
			    : sr -> CSR_limitproblem == LSR_SIZELIMITEXCEEDED
				    ? "size" : "administrative");
	}

    if (i == 0)
	adios (NULLCP, "search failed to find anything");
    else
	if (debug)
	    advise (LLOG_DEBUG, NULLCP, "%d match%s found",
		    i, i != 1 ? "es" : "");

    dn_free (sr -> CSR_object);
    entryinfo_free (sr -> CSR_entries, 0);
    crefs_free (sr -> CSR_cr);

    (void) unbind_from_directory ();

    if (!firstime) {
	int	failed = 0;
	struct TSAPdisconnect tds;

	for (ta = tas; ta < tz; ta++) {
	    for (tb = tys; tb < ty; tb++)
		if (bcmp ((char *) ta, (char *) tb, sizeof *ta) == 0)
		    break;
	    if (tb >= ty) {
		advise (LLOG_NOTICE, NULLCP, "closing %s", taddr2str (ta));

		if (TNetClose (ta, &tds) == NOTOK)
		    ts_advise (&tds, LLOG_EXCEPTIONS, "TNetClose failed");

		listening--;
	    }
	}

	for (ta = tys; ta < ty; ta++) {
	    for (tb = tas; tb < tz; tb++)
		if (bcmp ((char *) ta, (char *) tb, sizeof *ta) == 0)
		    break;
	    if (tb >= tz) {
		register struct NSAPaddr *na;

		if (ta -> ta_naddr) {
		    if (((na = ta -> ta_addrs) -> na_stack < 0
			     || na -> na_stack
				    >= sizeof services / sizeof services[0]))
			adios (NULLCP, "unknown network type 0x%x",
			       na -> na_stack);
		}
		else
		    na = NULLNA;

		if (!*services[na ? na -> na_stack : NA_NSAP])
		    continue;

		advise (LLOG_NOTICE, NULLCP, "listening on %s",
			taddr2str (ta));

		if (TNetListen (ta, &tds) == NOTOK) {
		    ts_advise (&tds, LLOG_EXCEPTIONS, "TNetListen failed");
		    failed++;
		}
		else
		    listening++;
	    }
	}

	if (!listening)
	    adios (NULLCP, failed ? "no successful listens"
				  : "no network services selected");

    }
    bzero ((char *) tas, sizeof tas);
    tz = tas;

    for (ta = tys; ta < ty; *tz++ = *ta++)	/* struct copy */
	continue;

    if (debug) {
	advise (LLOG_DEBUG, NULLCP, "application entitites...");
	for (ia = iae; ia < iz; ia++)
	    advise (LLOG_DEBUG, NULLCP, "  addr=%s vector=%s",
		    taddr2str (&ia -> is_addr), ia -> is_vector);

	advise (LLOG_DEBUG, NULLCP, "transport addresses...");
	for (ta = tas; ta < tz; ta++)
	    advise (LLOG_DEBUG, NULLCP, "  addr=%s", taddr2str (ta));
    }

    (void) time (&nextime);
    nextime += IAETIME;
}

/*  */

static	bind_to_directory ()
{
    struct ds_bind_arg bind_arg,
		       bind_result;
    register struct ds_bind_arg *ba = &bind_arg,
				*br = &bind_result;
    struct ds_bind_error bind_error;
    register struct ds_bind_error *be = &bind_error;
    static int very_first_time = 1;

    (void) unbind_from_directory ();

    make_bind_args (ba, br, be);

    if (debug)
	advise (LLOG_DEBUG, NULLCP, "connecting to DSA...");

    if (secure_ds_bind (ba, be, br) != DS_OK) {
	if (very_first_time)
	    very_first_time = 0;
	else
	    advise (LLOG_EXCEPTIONS, NULLCP, "unable to connect: %s",
		    be -> dbe_type == DBE_TYPE_SECURITY ? "security error"
						        : "DSA unavailable");

	isbound = 0;

	return;
    }
    very_first_time = 0;
    dn_free (br -> dba_dn);

    main_dsa = dsap_ad;

    advise (LLOG_NOTICE, NULLCP, "connected to %s", pa2str (&dsa_bound));

    isbound = 1;
}

/*  */

static int  rebind_to_directory ()
{
    if (referral_dsa != NOTOK) {
	if (debug)
	    advise (LLOG_DEBUG, NULLCP, "dap_unbind from referral dsa");

	(void) dap_unbind (referral_dsa);
	referral_dsa = NOTOK;
	dsap_ad = main_dsa;
    }

    if (!isbound)
	(void) bind_to_directory ();

    return (isbound ? OK : NOTOK);
}

/*  */

static	int	make_bind_args (ba, br, be)
register struct ds_bind_arg *ba,
			    *br;
register struct ds_bind_error *be;
{
    bzero ((char *) ba, sizeof *ba);
    bzero ((char *) br, sizeof *br);
    bzero ((char *) be, sizeof *be);

    ba -> dba_version = DBA_VERSION_V1988;
    if (ba -> dba_dn = userdn)
	ba -> dba_auth_type = DBA_AUTH_SIMPLE;
    if (ba -> dba_passwd_len = strlen (passwd))
	(void) strcpy (ba -> dba_passwd, passwd);
}

/*  */

static int  unbind_from_directory ()
{
    int	    wasbound;

    if (wasbound = isbound) {
	if (referral_dsa != NOTOK) {
	    if (debug)
		advise (LLOG_DEBUG, NULLCP, "dap_unbind from referral dsa");

	    (void) dap_unbind (referral_dsa);
	    referral_dsa = NOTOK;
	    dsap_ad = main_dsa;
	}

	(void) ds_unbind ();
	isbound = 0;
    }

    dsa_dead = 0;

    return wasbound;
}

/*  */

static int  do_error (de)
register struct DSError *de;
{
    if (de -> dse_type == DSE_REFERRAL
	    && de -> ERR_REFERRAL.DSE_ref_candidates) {
	register struct access_point *ap;
	struct ds_bind_arg bind_arg,
			   bind_result;
	register struct ds_bind_arg *ba = &bind_arg,
				    *br = &bind_result;
	struct ds_bind_error bind_error;
	register struct ds_bind_error *be = &bind_error;

	ap = de -> ERR_REFERRAL.DSE_ref_candidates -> cr_accesspoints;

	if (referral_dsa != NOTOK) {
	    if (debug)
		advise (LLOG_DEBUG, NULLCP, "dap_unbind from referral dsa");

	    (void) dap_unbind (referral_dsa);
	    referral_dsa = NOTOK;
	    dsap_ad = main_dsa;
	}

	make_bind_args (ba, br, be);	

	pslog (pgm_log, LLOG_NOTICE, "referring to", dn_print,
	       (caddr_t) ap -> ap_name);

	if (dap_bind (&referral_dsa, ba, be, br, ap -> ap_address) != DS_OK) {
	    advise (LLOG_EXCEPTIONS, NULLCP, "unable to connect: %s",
		    be -> dbe_type == DBE_TYPE_SECURITY ? "security error"
						        : "DSA unavailable");

	    dsap_ad = main_dsa;

	    ds_error_free (de);
	    return NOTOK;
	}
	dsap_ad = referral_dsa;

	if (debug)
	    advise (LLOG_DEBUG, NULLCP, "referral in progress");

	ds_error_free (de);
	return OK;
    }

    pslog (pgm_log, LLOG_EXCEPTIONS, "DAP error:", de_print, (caddr_t) de);

    if (dsa_dead) {
	dsa_dead = 0;

	if (referral_dsa != NOTOK) {
	    if (debug)
		advise (LLOG_DEBUG, NULLCP, "dap_unbind from referral dsa");

	    (void) dap_unbind (referral_dsa);
	    referral_dsa = NOTOK;
	    dsap_ad = main_dsa;
	}
	else
	    (void) unbind_from_directory ();
    }

    return NOTOK;
}

/*  */

int	str2dnY (str, dn)
char   *str;
DN     *dn;
{
    if (*str == NULL) {
	*dn = NULLDN;
	return OK;
    }

    return ((*dn = str2dn (str)) != NULLDN ? OK : NOTOK);
}

/*  */

#ifdef	BSD42
/* ARGSUSED */
#endif

static SFD  hupser (sig)
int	sig;
{
#ifndef	BSD42
    (void) signal (sig, hupser);
#endif

    search_directory (0);
}
#endif

/*  */

static  envinit () {
    int     i,
            sd;

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
	ll_dbinit (pgm_log, pgmname);

#ifndef	sun		/* damn YP... */
    for (sd = 3; sd < nbits; sd++)
	if (pgm_log -> ll_fd != sd)
	    (void) close (sd);
#endif

    (void) signal (SIGPIPE, SIG_IGN);

    ll_hdinit (pgm_log, pgmname);
    advise (LLOG_NOTICE, NULLCP, "starting");

#ifdef	IAE
	(void) signal (SIGHUP, hupser);
#endif
}

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
