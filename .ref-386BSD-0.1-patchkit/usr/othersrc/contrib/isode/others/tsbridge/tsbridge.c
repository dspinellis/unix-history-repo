/* tsbridge.c: transport bridge - jpo version ! */

#ifndef lint
static char *rcsid = "$Header: /f/osi/others/tsbridge/RCS/tsbridge.c,v 7.10 91/02/22 09:34:37 mrose Interim $";
#endif

/*
 * $Header: /f/osi/others/tsbridge/RCS/tsbridge.c,v 7.10 91/02/22 09:34:37 mrose Interim $
 *
 * Contributed by Julian Onions, Nottingham University in the UK
 *
 *
 * $Log:	tsbridge.c,v $
 * Revision 7.10  91/02/22  09:34:37  mrose
 * Interim 6.8
 * 
 * Revision 7.9  91/01/24  14:52:19  mrose
 * update
 * 
 * Revision 7.8  90/11/04  19:15:29  mrose
 * update
 * 
 * Revision 7.6  90/08/08  14:04:48  mrose
 * stuff
 * 
 * Revision 7.5  90/07/09  14:43:01  mrose
 * sync
 * 
 * Revision 7.4  90/03/19  14:27:00  mrose
 * jpo
 * 
 * Revision 7.2  90/01/11  18:36:55  mrose
 * real-sync
 * 
 * Revision 7.1  89/11/27  05:43:28  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:11:12  mrose
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


#include <signal.h>
#include <stdio.h>
#include <varargs.h>
#include "manifest.h"
#include <sys/ioctl.h>
#ifdef BSD42
#include <sys/file.h>
#endif
#ifdef SYS5
#include <fcntl.h>
#endif
#include "tsap.h"
#include "logger.h"
#include "psap.h"
#include "tailor.h"

/*  */

static int debug = 0;
static int nbits = FD_SETSIZE;

static LLog _pgm_log = {
	"tsbridge.log",	NULLCP,	NULLCP,
	LLOG_FATAL | LLOG_EXCEPTIONS | LLOG_NOTICE, LLOG_FATAL,
	-1, LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};

LLog	*pgm_log = &_pgm_log;

static char	*myname = "tsbridge";

typedef struct ContTbl {
	struct TSAPaddr	src;
	struct TSAPaddr dest;
	unsigned int flags;
#define CONN_STRICT	01
#define CONN_TRANS	02
#define CONN_NOMUNGE	04
#define CONN_FORCEMUNGE	010
} ContTbl;
ContTbl con_tbl[FD_SETSIZE];
int con_tbl_cnt = 0;

static struct TSAPaddr *maketa ();
static struct TSAPaddr *getnewta ();
static ContTbl *find_connection ();

static void read_file ();

static void adios (), advise ();

static void ts_adios (), ts_advise ();
static void ts_close (), ts_discon ();
static void tsbridge (), do_the_biz (), copy_tsdu (), arginit (), envinit ();
/*  */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char	**argv,
        **envp;
{
	struct TSAPdisconnect   tds;
	register struct TSAPdisconnect  *td = &tds;
	struct TSAPaddr tas, *ta = &tas;
	int	vecp;
	char	*vec[4];

	arginit (argv);

	envinit ();

	for (vecp = 0; vecp < con_tbl_cnt; vecp++) {
	    advise (LLOG_TRACE, NULLCP, "Listening on %s",
		     taddr2str (&con_tbl[vecp].src));
	    if (TNetListen (&con_tbl[vecp].src, td) == NOTOK) {
		advise (LLOG_FATAL, NULLCP, "Listen failed on \"%s\"",
			taddr2str (&con_tbl[vecp].src));
		ts_adios (td, "listen failed");
	    }
	}

	for (;;) {
		if (TNetAcceptAux (&vecp, vec, NULLIP, ta, 0, NULLFD, NULLFD,
				   NULLFD, NOTOK, td) == NOTOK)
			ts_adios (td, "accept failed");

		if (vecp <= 0)
			continue;

		advise (LLOG_TRACE, NULLCP, "accepted new connection");
		switch (TNetFork (vecp, vec, td)) {
		    case OK:
		        ll_hdinit (pgm_log, myname);
			tsbridge (vecp, vec, ta);
			exit (1);
			/* NOTREACHED */

		    case NOTOK:
			ts_advise (td, LLOG_EXCEPTIONS, "TNetFork failed");
			break;

		    default:
			break;
		}
	}
}

/*  */

static	void tsbridge (vecp, vec, ta)
int	vecp;
char	**vec;
struct TSAPaddr *ta;
{
	struct TSAPstart tss;
	register struct TSAPstart *ts = &tss;
	struct TSAPdisconnect   tds;
	register struct TSAPdisconnect  *td = &tds;
	struct TSAPaddr *tota;
	struct TSAPaddr *fromta;
	struct TSAPconnect tcs;
	struct TSAPconnect *tc = &tcs;
	int	sd;
	ContTbl *ctp;

	if (TInit (vecp, vec, ts, td) == NOTOK)
		ts_adios (td, "T-CONNECT.INDICATION failed");
		
	sd = ts -> ts_sd;
	advise (LLOG_NOTICE, NULLCP,
		"T-CONNECT.INDICATION: <%d, %s, %s, %d, %d>",
		ts -> ts_sd, taddr2str (&ts -> ts_calling),
		taddr2str (&ts -> ts_called),
		ts -> ts_expedited, ts -> ts_tsdusize);

	ctp = find_connection (ta);
	if (ctp == NULL) {
	    ts_close (sd, "Unknown listener address");
	    exit (1);
	}
	advise (LLOG_TRACE, NULLCP, "Accepted from address %s",
		taddr2str (&ctp -> src));

	tota = getnewta (&ts -> ts_called, sd, ctp);

	fromta = maketa (&ts -> ts_calling, tota -> ta_addrs[0].na_stack, ctp);

	if ((ctp -> flags & CONN_TRANS) == 0) {
	    ts -> ts_expedited = 0;
	    if (ts -> ts_cc > 0) {
		advise (LLOG_EXCEPTIONS, NULLCP,
			"%d octets initial user-data",
			ts -> ts_cc);
		ts_close (sd, "initial user-data not allowed");
		exit (1);
	    }
	}
	
	advise (LLOG_NOTICE, NULLCP,
		"T-CONNECT.REQUEST: <%s, %s, %d, 0x%x/%d>",
		taddr2str (fromta), taddr2str (tota), ts -> ts_expedited,
		ts -> ts_data, ts -> ts_cc);

	if (TConnRequest (fromta, tota, ts -> ts_expedited,
			  ts -> ts_data, ts -> ts_cc, &ts -> ts_qos,
			  tc, td) == NOTOK) {
		ts_close (sd, "connection establishment failed");
		ts_adios(td, "T-CONNECT.REQUEST");
	}
	if (TConnResponse (sd, NULLTA,
			   tc -> tc_expedited, tc -> tc_data, tc -> tc_cc,
			   &tc -> tc_qos, td) == NOTOK) {
		ts_close (sd, "connection establishment failed");
		ts_close (tc -> tc_sd, "connection establishment failed");
		ts_adios (td, "T-CONNECT.RESPONSE");
	}
	
	do_the_biz (sd, tc -> tc_sd);
}

/*  */

static void	do_the_biz (sd1, sd2)
int	sd1, sd2;
{
	int nfds = 0;
	fd_set rmask, imask;
	struct TSAPdisconnect   tds;
	register struct TSAPdisconnect  *td = &tds;

	FD_ZERO (&rmask);
	
	if (TSelectMask (sd1, &rmask, &nfds, td) == NOTOK
	        || TSelectMask (sd2, &rmask, &nfds, td) == NOTOK)
	    ts_adios (td, "TSelectMask failed");

	for (;;) {
		imask = rmask;
		if (xselect (nfds, &imask, NULLFD, NULLFD, NOTOK) == NOTOK)
			adios ("select", "failed");

		if (FD_ISSET (sd1, &imask))
			copy_tsdu (sd1, sd2);
		if (FD_ISSET (sd2, &imask))
			copy_tsdu (sd2, sd1);
	}
}

/*  */

static	void copy_tsdu (s1, s2)
int	s1, s2;
{
	struct TSAPdisconnect   tds;
	register struct TSAPdisconnect  *td = &tds;
	struct TSAPdata txs;
	register struct TSAPdata *tx = &txs;
	int	result;
	char	*p;

	SLOG (pgm_log, LLOG_DEBUG, NULLCP, ("copy_tsdu (%d -> %d)", s1, s2));

	if (TReadRequest (s1, tx, OK, td) == NOTOK) {
		switch (td -> td_reason) {
		    case DR_TIMER:
		    case DR_WAITING:
		    case DR_OPERATION:
		    case DR_PARAMETER:
			ts_advise (td, LLOG_TRACE, "TReadRequest");
			return;

		    case DR_NORMAL:
			ts_discon (td, s2);
			break;

		    default:
			ts_adios (td, "TReadRequest");
		}
	}

	if (tx -> tx_expedited) {
		SLOG (pgm_log, LLOG_DEBUG, NULLCP, ("TExpdRequest"));
		p = qb2str (&tx -> tx_qbuf);
		result = TExpdRequest (s2, p, tx -> tx_cc, td);
		free (p);
	}
	else {
		struct qbuf *qb;
		int	uiocnt = 0;
		struct udvec uvec[100];
		int total;

		total = uiocnt = 0;
		for (qb = tx-> tx_qbuf.qb_forw; qb != &tx -> tx_qbuf;
		     qb = qb -> qb_forw) {
			uvec[uiocnt].uv_base = qb -> qb_data;
			uvec[uiocnt++].uv_len = qb -> qb_len;
			total += qb -> qb_len;
			if (uiocnt > 100)
				adios (NULLCP, "Internal buffer overflow");
		}
		uvec[uiocnt].uv_base = NULLCP;
		uvec[uiocnt].uv_len = 0;
		if (tx -> tx_cc != total)
			advise (LLOG_EXCEPTIONS, NULLCP,
				"Mismatch in data %d != %d",
				tx -> tx_cc, total);
		SLOG (pgm_log, LLOG_DEBUG, NULLCP, ("TWriteRequest"));
		result = TWriteRequest (s2, uvec, td);
	}
	TXFREE (tx);

	if (result == NOTOK) {
		if (td -> td_reason == DR_NORMAL)
			ts_discon (td, s1);

		ts_adios (td, tx -> tx_expedited ? "T-EXPEDITED-DATA.REQUEST"
						 : "T-DATA.REQUEST");
	}
}

/*  */

static void ts_discon (td, sd)
struct TSAPdisconnect *td;
int	sd;
{
	ts_close (sd, "Normal Disconnect");
	ts_advise (td, LLOG_NOTICE, "T-DISCONNECT.INDICATION");

	exit (0);
}

/*  */

static void ts_close (sd, event)
int	sd;
char	*event;
{
	struct TSAPdisconnect tds;
	register struct TSAPdisconnect *td = &tds;

	if (strlen (event) >= TD_SIZE)
	    event = NULLCP;
	if (TDiscRequest (sd, event, event ? strlen (event) + 1: 0, td)
	        == NOTOK)
	    ts_advise (td, LLOG_EXCEPTIONS, "T-DISCONNECT.REQUEST");
}

/*  */

static void  ts_adios (td, event)
register struct TSAPdisconnect *td;
char	*event;
{
	ts_advise (td, LLOG_EXCEPTIONS, event);

	exit (1);
}

/*  */

static void  ts_advise (td, code, event)
register struct TSAPdisconnect *td;
int     code;
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
static int isnew = 1;

static struct TSAPaddr *getnewta (ta, sd, ctp)
struct TSAPaddr *ta;
int	sd;
ContTbl *ctp;
{
	static struct TSAPaddr newta;
	struct TSAPaddr *nta = &newta;
	char	buffer[TSSIZE + 1];
	int	n, m;

	isnew = 1;
	if (ctp -> flags & CONN_TRANS) {	/* make transparent address */
		*nta = ctp -> dest;	/* struct copy */
		nta -> ta_selectlen = ta -> ta_selectlen;
		bcopy (ta -> ta_selector, nta -> ta_selector,
		       ta -> ta_selectlen);
		return nta;
	}

	/* do the real TS bridge stuff */
	if ((m = ta -> ta_selectlen) == 0) {
		ts_close (sd, "no transport selector");
		adios (NULLCP, "no transport selector");
	}

	/* does this look like an encoded TSEL? */
	n = ta -> ta_selector[0];
/*
	advise (LLOG_TRACE, NULLCP, "n=%d,m=%d s[0] = %d, s[1] = %d",
		n, m, ta -> ta_selector[0], ta -> ta_selector[1]);
*/
	if (m > 4 &&
	    ta -> ta_selector[0] == ta -> ta_selector[1] &&
	    n > 2 && n <= m - 2) { /* encoded! */
		bzero ((char *)nta, sizeof *nta);
		nta -> ta_selectlen = m - n - 2;
		if (nta -> ta_selectlen > 0)
			bcopy (&ta -> ta_selector[n+2], nta -> ta_selector,
			       nta -> ta_selectlen);
		if (norm2na (&ta -> ta_selector[2], n, nta -> ta_addrs) != OK) {
			ts_close (sd, "undecodable address");
			adios (NULLCP, "Can't decode address");
		}
		nta -> ta_naddr = 1;
		return nta;
	}
	isnew = 0;
	/* try old form... */
	bcopy (ta -> ta_selector, buffer, ta -> ta_selectlen);
	buffer[ta -> ta_selectlen] = NULL;

	if ((nta = str2taddr (buffer)) == NULLTA) {
		ts_close (sd, "unable to translate address");
		adios (NULLCP, "unable to translate \"%s\"", buffer);
	}
	newta = *nta;

	return &newta;
}

/*  */

static struct TSAPaddr *maketa (ta, type, ctp)
struct TSAPaddr *ta;
long	type;
ContTbl *ctp;
{
    static struct TSAPaddr newta;
    register struct TSAPaddr *nta = &newta;
    char	*p;
    int	i;
    struct PSAPaddr pas;
    struct PSAPaddr *pa = &pas;

    if (ctp -> flags & CONN_NOMUNGE) {
	*nta = *ta;	/* struct copy */
    }
    if (!(ctp -> flags & CONN_NOMUNGE) || (ctp -> flags & CONN_FORCEMUNGE)) {
	if (isnew == 0) {
	    bzero ((char *)pa, sizeof *pa);
	    pa -> pa_addr.sa_addr = *ta;
	    if ((p = _paddr2str (pa, NULLNA, -1)) == NULL ||
		(nta -> ta_selectlen = strlen (p)) >= TSSIZE) {
		if (ctp -> flags & CONN_STRICT)
		    adios (NULLCP, "new selector not encodable");

		advise (LLOG_NOTICE, NULLCP,
			"new selector not encodable");
		nta -> ta_selectlen = 0;
	    }
	    else
		bcopy (p, nta -> ta_selector, TSSIZE);
	}
	else {
	    struct NSAPaddr *nna = na2norm (&ta -> ta_addrs[0]);

	    if ((nta -> ta_selectlen = 2 + nna -> na_addrlen +
		 ta -> ta_selectlen) >= TSSIZE)
		nta -> ta_selectlen = 0;
	    else {
		bcopy (nna -> na_address, &nta -> ta_selector[2],
		       nna -> na_addrlen);
		bcopy (ta -> ta_selector,
		       &nta -> ta_selector[2 + nna -> na_addrlen],
		       ta -> ta_selectlen);
		nta -> ta_selector[0] = nta -> ta_selector[1] =
		    nna -> na_addrlen;
	    }
	}
    }
    for (i = 0; i < ctp -> src.ta_naddr; i++) {
	if (ctp -> src.ta_addrs[i].na_stack == type) {
	    /* our address */
	    nta -> ta_addrs[0] = ctp->src.ta_addrs[i]; 
	    nta -> ta_naddr = 1;
	    return nta;
	}
    }
    /*
     * This requires an explanation:
     * If NOMUNGE && FORCEMUNGE  we have a semi-transparent bridge
     * and since [at least on my machine] the recipient of a "transparent"
     * call sees it as coming from the bridge host, ie the effect is that
     * of a strict call, the structure that is now in nta, viz:
     * "calling address"/calling address
     * is going to get clobbered and appear at the final host as originating
     * "calling address"/bridge host
     * anyway.  This is what I want.
     * => return nta
     */
    if ((ctp -> flags & CONN_NOMUNGE) 
	&& (ctp -> flags & CONN_FORCEMUNGE)
	&& !(ctp -> flags & CONN_STRICT)) {
	return nta;
    }
    if (ctp -> flags & CONN_STRICT)
	adios (NULLCP, "not listening on this network (%d)", type);

    advise (LLOG_NOTICE, NULLCP,
	    "not listening on this network (%d)", type);

    return ta;
}

/*  */

static ContTbl *find_connection (ta)
struct TSAPaddr *ta;
{
    ContTbl *ctp;
    struct NSAPaddr *na1, *na2;

    for (ctp = con_tbl; ctp < &con_tbl[con_tbl_cnt]; ctp ++) {
	for (na1 = &ctp -> src.ta_addrs[0];
	     na1 < &ctp -> src.ta_addrs[ctp->src.ta_naddr]; na1++) {
	    for (na2 = &ta -> ta_addrs[0];
		 na2 < &ta -> ta_addrs[ta->ta_naddr]; na2 ++) {
		if (na1 -> na_stack != na2 -> na_stack)
		    continue;

		switch (na1 -> na_stack) {
		case NA_NSAP:
		    if (na1 -> na_addrlen == na2 -> na_addrlen &&
			bcmp (na1 -> na_address, na2 -> na_address,
			      na1 -> na_addrlen) == 0 &&
			ta -> ta_selectlen == ctp -> src.ta_selectlen &&
			bcmp (ta -> ta_selector, ctp -> src.ta_selector,
			      ta -> ta_selectlen) == 0)
			return ctp;
		    break;

		case NA_TCP:
		    if (na1 -> na_port == na2 -> na_port &&
		       strcmp (na1 -> na_domain, na2 -> na_domain) == 0)
			return ctp;
		    break;

		case NA_X25:
		case NA_BRG:
		    if (na1 -> na_dtelen == na2 -> na_dtelen &&
			bcmp (na1 -> na_dte, na2 -> na_dte,
			      na1 -> na_dtelen) == 0 &&
			na1 -> na_pidlen == na2 -> na_pidlen &&
			bcmp (na1 -> na_pid, na2 -> na_pid,
			      na1 -> na_pidlen) == 0)
			return ctp;
		    break;
		}
	    }
	}
    }
    return NULL;
}

/*  */

static void	arginit (vec)
char	**vec;
{
	register char   *ap;
	register struct TSAPaddr *ta;

	if (myname = rindex (*vec, '/'))
		myname++;
	if (myname == NULL || *myname == NULL)
		myname = *vec;

	for (vec++; ap = *vec; vec++) {
	    if (*ap == '-' && ap[1])
		switch (*++ap) {
		    case 'T':
		    	if ((ap = *++vec) == NULL || *ap == '-')
			    adios (NULLCP, "usage: %s -T tailorfile", myname);
		    	(void) isodesetailor (ap);
			isodetailor (myname, 0);
			ll_hdinit (pgm_log, myname);
			continue;

		    case 'a':
		        if ((ap = *++vec) == NULL || *ap == '-')
			    adios (NULLCP, "usage: %s -a address", myname);
			if ((ta = str2taddr (ap)) == NULLTA)
			    adios (NULLCP, "bad address \"%s\"", ap);
			con_tbl[0].src = *ta; /* struct copy */
			con_tbl[0].flags =  0;
			con_tbl_cnt = 1;
		        continue;

		    case 's':
			con_tbl[0].flags |= CONN_STRICT;
			continue;

		    default:
			adios (NULLCP, "unknown switch -%s", ap);
		}
	    else
		break;
	    
	}
	isodetailor (myname, 0);
	ll_hdinit (pgm_log, myname);

	for (; ap = *vec; vec++)
	    read_file (ap);

	if (con_tbl_cnt <= 0) {
	    if ((ta = str2taddr (tsb_default_address)) == NULLTA)
		adios (NULLCP, "bad default address \"%s\"",
		       tsb_default_address);
	    con_tbl[0].src = *ta; /* struct copy */
	    con_tbl_cnt = 1;
	}
}

/*  */

static void read_file (file)
char	*file;
{
    FILE	*fp;
    char	buf[BUFSIZ];
    char	*vec[50];
    char	*ap;
    int		vecp, i;
    ContTbl	*ctp;
    struct TSAPaddr *ta;

    if (strcmp (file, "-") == 0)
	fp = stdin;
    else if ((fp = fopen (file, "r")) == NULL)
	adios (file, "Can't open ");

    while (fgets (buf, sizeof buf, fp) != NULLCP) {
	if (buf[0] == '#' || buf[0] == '\n')
	    continue;

	vecp = sstr2arg (buf, 50, vec, " \t,\n");
	if (vecp <= 0)
	    continue;

	if ((ta = str2taddr (vec[0])) == NULLTA)
	    adios (NULLCP, "Bad address \"%s\" in file %s", vec[0], file);

	ctp = &con_tbl[con_tbl_cnt];
	ctp -> src = *ta; /* struct copy */
	con_tbl_cnt ++;

	for (i = 1; i < vecp; i++) {
	    ap = vec[i];
	    if (*ap == '\0')
		continue;
	    if (*ap == '-') {
		switch (*++ap) {
		case 's':
		    ctp -> flags |= CONN_STRICT;
		    break;
		case 't':
		    ctp -> flags |= CONN_TRANS;
		    break;
		case 'n':
		    ctp -> flags |= CONN_NOMUNGE;
		    break;
		case 'f':
		    ctp -> flags |= CONN_FORCEMUNGE;
		    break;

		default:
		    adios (NULLCP, "Unknown option -%c", *ap);
		}
	    }
	    else {
		if ((ta = str2taddr (ap)) == NULLTA)
		    adios (NULLCP, "Bad address \"%s\" in file %s",
			   ap, file);
		ctp -> dest = *ta; /* struct copy */
		ctp -> flags |= (CONN_TRANS|CONN_NOMUNGE);
	    }
	}

    }

    if (strcmp (file, "-") != 0)
	(void) fclose (fp);
}

/*  */

static	void envinit () {
	int     i,
		sd;

	nbits = getdtablesize ();

	if (!(debug = isatty (2))) {
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
#ifdef  TIOCNOTTY
		if ((sd = open ("/dev/tty", O_RDWR)) != NOTOK) {
			(void) ioctl (sd, TIOCNOTTY, NULLCP);
			(void) close (sd);
		}
#else
#ifdef  SYS5
		(void) setpgrp ();
		(void) signal (SIGINT, SIG_IGN);
		(void) signal (SIGQUIT, SIG_IGN);
#endif
#endif
	}
	else
		ll_dbinit (pgm_log, myname);

#ifndef sun			/* damn YP... */
	for (sd = 3; sd < nbits; sd++)
	    if (pgm_log -> ll_fd != sd)
		(void) close (sd);
#endif

	(void) signal (SIGPIPE, SIG_IGN);

	ll_hdinit (pgm_log, myname);
	advise (LLOG_NOTICE, NULLCP, "starting");
}

/*    ERRORS */

#ifndef lint
static void    adios (va_alist)
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

static void    adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef lint
static void    advise (va_alist)
va_dcl
{
    int     code;
    va_list ap;

    va_start (ap);

    code = va_arg (ap, int);

    _ll_log (pgm_log, code, ap);

    va_end (ap);
}
#else
/* VARARGS */

static void    advise (code, what, fmt)
char   *what,
       *fmt;
int     code;
{
    advise (code, what, fmt);
}
#endif

