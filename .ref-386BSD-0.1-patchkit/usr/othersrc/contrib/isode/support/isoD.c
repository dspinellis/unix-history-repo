/* isod.c - "minimal" ISODE server for testing */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/support/RCS/isod.c,v 7.2 91/02/22 09:46:37 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/support/RCS/isod.c,v 7.2 91/02/22 09:46:37 mrose Interim $
 *
 *
 * $Log:	isod.c,v $
 * Revision 7.2  91/02/22  09:46:37  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:07:50  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  22:27:27  mrose
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
#include "isoservent.h"
#include "tailor.h"
#include "OACS-types.h"

/*    DATA */

static int debug = 0;
static int isacs = 0;
static int isrts = 0;

static LLog _pgm_log = {
    "isod.log", NULLCP, NULLCP, LLOG_FATAL | LLOG_EXCEPTIONS | LLOG_NOTICE,
    LLOG_FATAL, -1, LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *pgm_log = &_pgm_log;

static char *myname = "isod";


static enum mode { echo, sink, XXX } mymode = XXX;

struct dispatch {
    char   *ds_entity;

    enum mode ds_mode;
};


void	adios (), advise ();


void	ts_adios (), ts_advise ();
int	ts_dataindication (), ts_discindication ();

static struct dispatch  ts_dispatches[] = {
    "echo", echo,
    "sink", sink,

    NULLCP, XXX
};


void	ss_adios (), ss_advise ();
int	ss_dataindication (), ss_tokenindication (), ss_syncindication (),
	ss_actindication (), ss_reportindication (), ss_finishindication (),
	ss_abortindication ();

static struct dispatch *ss_dispatches = ts_dispatches;


void	ps_adios (), ps_advise ();
int	ps_dataindication (), ps_tokenindication (), ps_syncindication (),
	ps_actindication (), ps_reportindication (), ps_finishindication (),
	ps_abortindication ();

static struct dispatch *ps_dispatches = ts_dispatches;


void	acs_adios (), acs_advise ();

static struct dispatch  acs_dispatches[] = {
    "isode echo", echo,
    "isode sink", sink,

    NULLCP, XXX
};


void	rts_adios (), rts_advise ();
int	rts_indication ();

static struct dispatch  rts_dispatches[] = {
    "echo", echo,
    "sink", sink,
    "ros_echo", echo,
    "ros_sink", sink,

    NULLCP, XXX
};

static struct dispatch  rtse_dispatches[] = {
    "isode rtse echo", echo,
    "isode rtse sink", sink,
    "isode ros_echo", echo,
    "isode ros_sink", sink,

    NULLCP, XXX
};

static PE  apdupe = NULLPE;


void	ros_adios (), ros_advise ();
int	ros_indication ();

static struct dispatch *ros_dispatches = ts_dispatches;

static PE nullpe = NULLPE;


extern int  errno;

/*    MAIN */

/* ARGSUSED */

old_isod_main (argc, argv, envp)
int     argc;
char  **argv,
      **envp;
{
    register char  *cp;

    if (myname = rindex (argv[0], '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = argv[0];

    isodetailor (myname, 0);
    if (debug = isatty (fileno (stderr)))
	ll_dbinit (pgm_log, myname);
    else
	ll_hdinit (pgm_log, myname);

    advise (LLOG_NOTICE, NULLCP, "starting");

    if (cp = rindex (*argv, '.'))
	*cp++ = NULL;

/* cheat! should do this after calling the init function (sigh!) */
    if (argc > 1 && strcmp (argv[1], "-rtse") == 0)
	isacs++;

    if (cp == NULL || strcmp (cp, "tsap") == 0)
	ts_main (argc, argv);
    else
	if (strcmp (cp, "ssap") == 0)
	    ss_main (argc, argv);
	else
	    if (strcmp (cp, "psap") == 0)
		ps_main (argc, argv);
	    else
		if (strcmp (cp, "acsap") == 0) {
		    isacs++;
		    ps_main (argc, argv);
		}
		else
		    if (strcmp (cp, "rtsap") == 0) {
			isrts++;
			rts_main (argc, argv);
		    }
		    else
			if (strcmp (cp, "rosap") == 0)
			    ros_main (argc, argv);
			else
			    adios (NULLCP, "unknown provider: \"%s\"", cp);

    exit (0);			/* NOTREACHED */
}

/*    TSAP */

static int  ts_main (argc, argv)
int	argc;
char  **argv;
{
    int     async,
	    sd;
    char    buffer[BUFSIZ];
    register struct dispatch *ds;
    register struct isoservent *is;
    struct TSAPstart    tss;
    register struct TSAPstart  *ts = &tss;
    struct TSAPdata txs;
    register struct TSAPdata   *tx = &txs;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect *td = &tds;

    if (TInit (argc, argv, ts, td) == NOTOK)
	ts_adios (td, "(T)initialization fails");
    advise (LLOG_NOTICE, NULLCP,
	    "T-CONNECT.INDICATION: <%d, %s, %s, %d, %d>",
	    ts -> ts_sd,
	    taddr2str (&ts -> ts_calling), taddr2str (&ts -> ts_called),
	    ts -> ts_expedited, ts -> ts_tsdusize);
#ifdef	DEBUG
    if (ts -> ts_cc > 0)
	advise (LLOG_DEBUG, NULLCP, "greetings: %d octets", ts -> ts_cc);
#endif

    sd = ts -> ts_sd;

    if (is = getisoserventbyselector ("tsap", ts -> ts_called.ta_selector,
			ts -> ts_called.ta_selectlen))
	for (ds = ts_dispatches; ds -> ds_entity; ds++)
	    if (strcmp (ds -> ds_entity, is -> is_entity) == 0) {
		mymode = ds -> ds_mode;
		break;
	    }

    async = 0;
    for (argv++; *argv; argv++) {
	if (strcmp (*argv, "-async") == 0) {
	    async++;
	    continue;
	}
	if (strcmp (*argv, "-sync") == 0) {
	    async = 0;
	    continue;
	}

	advise (LLOG_NOTICE, NULLCP, "unknown argument \"%s\"", *argv);
    }

    switch (mymode) {
	case echo: 
	case sink:
	    if (TConnResponse (sd, NULLTA, ts -> ts_expedited,
		    mymode == echo ? ts -> ts_data : NULLCP,
		    mymode == echo ? ts -> ts_cc : 0, NULLQOS, td) == NOTOK)
		ts_adios (td, "T-CONNECT.RESPONSE");
	    break;

	default: 
	    (void) strcpy (buffer, "entity unknown or unavailable");
	    if (TDiscRequest (sd, buffer, strlen (buffer) + 1, td) == NOTOK)
		ts_adios (td, "T-DISCONNECT.REQUEST");
	    advise (LLOG_NOTICE, NULLCP, "rejected");
	    exit (1);
    }

    if (async) {
	if (TSetIndications (sd, ts_dataindication, ts_discindication, td)
		== NOTOK)
	    ts_adios (td, "set ASYNC fails");

	for (;;)
	    pause ();
    }

    for (;;) {
	if (TReadRequest (sd, tx, NOTOK, td) == NOTOK)
	    ts_discindication (sd, td);

	ts_dataindication (sd, tx);
    }
}

/*  */

static int  ts_dataindication (sd, tx)
int	sd;
register struct TSAPdata *tx;
{
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect *td = &tds;

    if (mymode == echo) {
	register char *p = qb2str (&tx -> tx_qbuf);

	if ((tx -> tx_expedited
		    ? TExpdRequest (sd, p, tx -> tx_cc, td)
		    : TDataRequest (sd, p, tx -> tx_cc, td))
		== NOTOK) {
	    if (td -> td_reason == DR_NORMAL)
		ts_discindication (sd, td);

	    ts_adios (td, tx -> tx_expedited ? "T-EXPEDITED-DATA.REQUEST"
		    : "T-DATA.REQUEST");
	}

	free (p);
    }

    TXFREE (tx);
}

/*  */

/* ARGSUSED */

static int  ts_discindication (sd, td)
int	sd;
register struct TSAPdisconnect *td;
{
    if (td -> td_reason != DR_NORMAL)
	ts_adios (td, "T-DISCONNECT.INDICATION");

    if (td -> td_cc > 0)
	ts_advise (td, "T-DISCONNECT.INDICATION");
    else
	advise (LLOG_NOTICE, NULLCP, "T-DISCONNECT.INDICATION");

    exit (0);
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
    char    buffer[BUFSIZ];

    if (td -> td_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s",
		TErrString (td -> td_reason),
		td -> td_cc, td -> td_cc, td -> td_data);
    else
	(void) sprintf (buffer, "[%s]", TErrString (td -> td_reason));

    advise (LLOG_NOTICE, NULLCP, "%s: %s", event, buffer);
}

/*    SSAP */

#define	RMASK \
    "\020\01HALFDUPLEX\02DUPLEX\03EXPEDITED\04MINORSYNC\05MAJORSYNC\06RESYNC\
\07ACTIVITY\010NEGOTIATED\011CAPABILITY\012EXCEPTIONS\013TYPEDATA"

#define	TMASK	"\020\01DATA\03SYNC\05ACTIVITY\07RELEASE"
    

#define dotoken(requires,shift,bit,type) \
{ \
    if (requirements & requires) \
	switch (ss -> ss_settings & (ST_MASK << shift)) { \
	    case ST_CALL_VALUE << shift: \
		advise (LLOG_DEBUG, NULLCP, "%s token: choice", type); \
		ss -> ss_settings &= ~(ST_MASK << shift); \
		ss -> ss_settings |= ST_INIT_VALUE << shift; \
		break; \
 \
	    case ST_INIT_VALUE: \
		advise (LLOG_DEBUG, NULLCP, "%s token: initiator", type); \
		break; \
 \
	    case ST_RESP_VALUE: \
		advise (LLOG_DEBUG, NULLCP, "%s token: responder", type); \
		owned |= bit; \
		break; \
 \
	    default: \
		adios (NULLCP, "%s token: reserved", type); \
		break; \
	} \
}


static int  requirements = 0;
static int  owned = 0;

static struct SSAPdata hxs;
static struct SSAPdata *hx = &hxs;

/*  */

static int ss_main (argc, argv)
int	argc;
char  **argv;
{
    int     async,
	    result,
            sd;
    char    buffer[BUFSIZ];
    register struct dispatch   *ds;
    register struct isoservent *is;
    struct SSAPstart    sss;
    register struct SSAPstart  *ss = &sss;
    struct SSAPdata sxs;
    register struct SSAPdata   *sx = &sxs;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;

    if (SInit (argc, argv, ss, si) == NOTOK)
	ss_adios (sa, "(S)initialization fails");
    advise (LLOG_NOTICE, NULLCP,
	    "S-CONNECT.INDICATION: <%d, %s, %s, %s, %s, %ld, %d>",
	    ss -> ss_sd, sprintref (&ss -> ss_connect),
	    saddr2str (&ss -> ss_calling), saddr2str (&ss -> ss_called),
	    sprintb (ss -> ss_requirements, RMASK), ss -> ss_isn,
	    ss -> ss_ssdusize);
#ifdef	DEBUG
    if (ss -> ss_cc > 0)
	advise (LLOG_DEBUG, NULLCP, "greetings: %d octets", ss -> ss_cc);
#endif

    sd = ss -> ss_sd;
    ss -> ss_requirements &= SR_HALFDUPLEX | SR_DUPLEX | SR_EXPEDITED
	| SR_MINORSYNC | SR_MAJORSYNC | SR_RESYNC | SR_ACTIVITY
	| SR_NEGOTIATED | SR_CAPABILITY | SR_EXCEPTIONS | SR_TYPEDATA;
    if ((ss -> ss_requirements & SR_HALFDUPLEX)
	    && (ss -> ss_requirements & SR_DUPLEX))
	ss -> ss_requirements &= ~SR_DUPLEX;
    requirements = ss -> ss_requirements;
    advise (LLOG_DEBUG, NULLCP, "new requirements: %s",
		sprintb (ss -> ss_requirements, RMASK));
    dotokens ();
    advise (LLOG_DEBUG, NULLCP, "initial tokens: %s", sprintb (owned, TMASK));
    if (!(ss -> ss_requirements & (SR_MINORSYNC | SR_MAJORSYNC | SR_RESYNC)))
	ss -> ss_isn = SERIAL_NONE;

    if (is = getisoserventbyselector ("ssap", ss -> ss_called.sa_selector,
		ss -> ss_called.sa_selectlen))
	for (ds = ss_dispatches; ds -> ds_entity; ds++)
	    if (strcmp (ds -> ds_entity, is -> is_entity) == 0) {
		mymode = ds -> ds_mode;
		break;
	    }

    async = 0;
    for (argv++; *argv; argv++) {
	if (strcmp (*argv, "-async") == 0) {
	    async++;
	    continue;
	}
	if (strcmp (*argv, "-sync") == 0) {
	    async = 0;
	    continue;
	}

	advise (LLOG_NOTICE, NULLCP, "unknown argument \"%s\"", *argv);
    }

    switch (mymode) {
	case echo: 
	case sink:
	    if (SConnResponse (sd, &ss -> ss_connect, NULLSA,
			SC_ACCEPT, ss -> ss_requirements, ss -> ss_settings,
			ss -> ss_isn, mymode == echo ? ss -> ss_data : NULLCP,
			mymode == echo ? ss -> ss_cc : 0, si) == NOTOK)
		ss_adios (sa, "S-CONNECT.RESPONSE (accept)");
	    break;

	default: 
	    (void) strcpy (buffer, "entity unknown or unavailable");
	    if (SConnResponse (sd, &ss -> ss_connect, NULLSA,
			SC_REJECTED, 0, 0, SERIAL_NONE, buffer,
			strlen (buffer + 1), si)
		    == NOTOK)
		ss_adios (sa, "S-CONNECT.RESPONSE (reject)");
	    advise (LLOG_NOTICE, NULLCP, "rejected");
	    exit (1);
    }

    if (async) {
	if (SSetIndications (sd, ss_dataindication, ss_tokenindication,
		    ss_syncindication, ss_actindication, ss_reportindication,
		ss_finishindication, ss_abortindication, si) == NOTOK)
	    ss_adios (sa, "set ASYNC fails");

	for (;;)
	    pause ();
    }

    for (;;)
	switch (result = SReadRequest (sd, sx, NOTOK, si)) {
	    case NOTOK: 
		ss_abortindication (sd, sa);

	    case OK: 
		ss_dataindication (sd, sx);
		break;

	    case DONE: 
		switch (si -> si_type) {
		    case SI_TOKEN: 
			ss_tokenindication (sd, &si -> si_token);
			break;

		    case SI_SYNC: 
			ss_syncindication (sd, &si -> si_sync);
			break;

		    case SI_ACTIVITY:
			ss_actindication (sd, &si -> si_activity);
			break;

		    case SI_REPORT:
			ss_reportindication (sd, &si -> si_report);
			break;

		    case SI_FINISH: 
			ss_finishindication (sd, &si -> si_finish);
			break;

		    default: 
			adios (NULLCP, "unknown indication type=0x%x",
				si -> si_type);
		}
		break;

	    default: 
		adios (NULLCP, "unknown return from SReadRequest=%d", result);
	}
}

#undef	dotoken

/*  */

static int ss_dataindication (sd, sx)
int	sd;
register struct SSAPdata *sx;
{
    char   *p,
	    buffer[BUFSIZ];
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;

#ifdef	DEBUG
    switch (sx -> sx_type) {
	case SX_NORMAL: 
	    advise (LLOG_DEBUG, NULLCP, "normal data, %d bytes", sx -> sx_cc);
	    break;

	case SX_EXPEDITED: 
	    advise (LLOG_DEBUG, NULLCP, "expedited data, %d bytes",
		    sx -> sx_cc);
	    break;

	case SX_TYPED: 
	    advise (LLOG_DEBUG, NULLCP, "typed data, %d bytes", sx -> sx_cc);
	    break;

	case SX_CAPDIND: 
	    advise (LLOG_DEBUG, NULLCP, "capability data, %d bytes",
		    sx -> sx_cc);
	    break;

	case SX_CAPDCNF: 
	    advise (LLOG_DEBUG, NULLCP, "capability data ack, %d bytes",
		    sx -> sx_cc);

	default:
	    advise (LLOG_DEBUG, NULLCP,
		    "unknown data indication type=0x%x, %d bytes",
		    sx -> sx_type, sx -> sx_cc);
    }
#endif

    p = NULL;

    switch (sx -> sx_type) {
	case SX_NORMAL: 
	    if (mymode == sink)
		break;
	    if (requirements & SR_HALFDUPLEX) {
		if (hx -> sx_cc > 0) {
		    (void) strcpy (buffer, "protocol screw-up");
		    if (SUAbortRequest (sd, buffer, strlen (buffer) + 1, si) == NOTOK)
			ss_adios (sa, "S-U-ABORT.REQUEST");
		    else
			adios (NULLCP, "protocol screw-up");
		}
		else {
		    *hx = *sx;	/* struct copy */
		    hx -> sx_qbuf.qb_forw -> qb_back =
			    hx -> sx_qbuf.qb_back -> qb_forw = &hx -> sx_qbuf;
		    bzero ((char *) sx, sizeof *sx);
		    sx -> sx_qbuf.qb_forw =
			    sx -> sx_qbuf.qb_back = &sx -> sx_qbuf;
		    if (!(owned & ST_DAT_TOKEN)
			    && SPTokenRequest (sd, ST_DAT_TOKEN, NULLCP,
				0, si) == NOTOK)
			ss_adios (sa, "S-TOKEN-PLEASE.REQUEST");
		}
	    }
	    else
		if (SDataRequest (sd, p = qb2str (&sx -> sx_qbuf), sx -> sx_cc,
			    si) == NOTOK)
		    ss_adios (sa, "S-DATA.REQUEST");
	    break;

	case SX_EXPEDITED: 
	    if (mymode == sink)
		break;
	    if (SExpdRequest (sd, p = qb2str (&sx -> sx_qbuf), sx -> sx_cc,
			si) == NOTOK)
		ss_adios (sa, "S-EXPEDITED-DATA.REQUEST");
	    break;

	case SX_TYPED: 
	    if (mymode == sink)
		break;
	    if (STypedRequest (sd, p = qb2str (&sx -> sx_qbuf), sx -> sx_cc,
			si) == NOTOK)
		ss_adios (sa, "S-TYPED-DATA.REQUEST");
	    break;

	case SX_CAPDIND: 
	    if (SCapdResponse (sd, p = qb2str (&sx -> sx_qbuf), sx -> sx_cc,
			si) == NOTOK)
		ss_adios (sa, "S-CAPABILITY-DATA.REQUEST");
	    break;

	case SX_CAPDCNF: 
	    adios (NULLCP, "got capability data response");

	default: 
	    adios (NULLCP, "unknown data indication type=0x%x", sx -> sx_type);
    }

    SXFREE (sx);
    if (p)
	free (p);
}

/*  */

static int ss_tokenindication (sd, st)
int	sd;
register struct SSAPtoken *st;
{
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort *sa = &si -> si_abort;

#ifdef	DEBUG
    advise (LLOG_DEBUG, NULLCP, "%s tokens: %s, %d bytes",
	    st -> st_type == ST_PLEASE ? "please"
	    : st -> st_type == ST_GIVE ? "give" : "control",
	    sprintb ((int) st -> st_tokens, TMASK), st -> st_cc);
#endif

    switch (st -> st_type) {
	case ST_GIVE:
	case ST_CONTROL:
	    owned = st -> st_owned;
	    break;

	case ST_PLEASE:
	    break;

	default:
	    adios (NULLCP, "unknown token indication type=0x%x",
		    st -> st_type);
    }
    
    if ((owned & ST_DAT_TOKEN) && hx -> sx_cc > 0) {
	char   *p;
	
	if (SDataRequest (sd, p = qb2str (&hx -> sx_qbuf), hx -> sx_cc, si)
		== NOTOK)
	    ss_adios (sa, "S-DATA.REQUEST");
	SXFREE (hx);
	free (p);
	bzero ((char *) hx, sizeof *hx);
	hx -> sx_qbuf.qb_forw =
		hx -> sx_qbuf.qb_back = &hx -> sx_qbuf;
    }

    switch (st -> st_type) {
	case ST_GIVE:
	    break;

	default:
	    if (SGTokenRequest (sd, (int) st -> st_tokens, si) == NOTOK)
		ss_adios (sa, "S-TOKEN-GIVE.REQUEST");
	    else
		owned &= ~st -> st_tokens;
	    break;
    }

    STFREE (st);
}

/*  */

static int ss_syncindication (sd, sn)
int	sd;
register struct SSAPsync *sn;
{
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;

#ifdef	DEBUG
    switch (sn -> sn_type) {
	case SN_MAJORIND: 
	    advise (LLOG_DEBUG, NULLCP, "majorsync indication %d, %d bytes",
		    sn -> sn_ssn, sn -> sn_cc);
	    break;

	case SN_MAJORCNF: 
	    advise (LLOG_DEBUG, NULLCP, "majorsync confirmation %d, %d bytes",
		    sn -> sn_ssn, sn -> sn_cc);
	    break;

	case SN_MINORIND: 
	    advise (LLOG_DEBUG, NULLCP, "minorsync indication %d%s, %d bytes",
		    sn -> sn_ssn, sn -> sn_options == SYNC_CONFIRM
		    ? " (wants confirmation)" : NULLCP, sn -> sn_cc);
	    break;

	case SN_MINORCNF: 
	    advise (LLOG_DEBUG, NULLCP, "minorsync confirmation %d, %d bytes",
		    sn -> sn_ssn, sn -> sn_cc);
	    break;

	case SN_RESETIND: 
	    advise (LLOG_DEBUG, NULLCP,
		    "resync indication type=%d %d, %d bytes",
		    sn -> sn_options, sn -> sn_ssn, sn -> sn_cc);
	    break;

	case SN_RESETCNF: 
	    advise (LLOG_DEBUG, NULLCP, "resync confirmation %d, %d bytes",
		    sn -> sn_ssn, sn -> sn_cc);
	    break;

	default: 
	    advise (LLOG_DEBUG, NULLCP,
		    "unknown sync indication=0x%x, ssn=%d, %d bytes",
		    sn -> sn_type, sn -> sn_ssn, sn -> sn_cc);
	    break;
    }
#endif

    switch (sn -> sn_type) {
	case SN_MAJORIND: 
	    if (SMajSyncResponse (sd,
			mymode == echo ? sn -> sn_data : NULL,
			mymode == echo ? sn -> sn_cc : 0, si) == NOTOK)
		ss_adios (sa, "S-MAJOR-SYNC.RESPONSE");
	    break;

	case SN_MAJORCNF: 
	    adios (NULLCP, "got majorsync confirmation");

	case SN_MINORIND: 
	    if (sn -> sn_options == SYNC_CONFIRM)
		if (SMinSyncResponse (sd, sn -> sn_ssn,
			    mymode == echo ? sn -> sn_data : NULL,
			    mymode == echo ? sn -> sn_cc : 0, si) == NOTOK)
		    ss_adios (sa, "S-MINOR-SYNC.RESPONSE");
	    break;

	case SN_MINORCNF: 
	    adios (NULLCP, "got minorsync confirmation");

	case SN_RESETIND: 
#define	dotoken(requires,shift,bit,type) \
{ \
	    if (requirements & requires) \
		switch (sn -> sn_settings & (ST_MASK << shift)) { \
		    case ST_CALL_VALUE << shift: \
			sn -> sn_settings &= ~(ST_MASK << shift); \
			sn -> sn_settings |= ST_RESP_VALUE << shift; \
		    case ST_RESP_VALUE << shift: \
			owned |= bit; \
			break; \
 \
		    case ST_INIT_VALUE << shift: \
			owned &= ~bit; \
			break; \
 \
		    default: \
			adios (NULLCP, "%s token: reserved", type); \
			break; \
		} \
}
	    dotokens ();
#undef	dotoken
	    if (SReSyncRequest (sd, SYNC_ABANDON, SERIAL_NONE,
			sn -> sn_settings,
			mymode == echo ? sn -> sn_data : NULL,
			mymode == echo ? sn -> sn_cc : 0, si) == NOTOK)
		ss_adios (sa, "S-RESYNCHRONIZE.REQUEST");
	    break;
	
	case SN_RESETCNF: 
	    break;

	default: 
	    adios (NULLCP, "unknown sync indication type=0x%x", sn -> sn_type);
    }

    SNFREE (sn);
}

/*  */

static int ss_actindication (sd, sv)
int	sd;
register struct SSAPactivity *sv;
{
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;

#ifdef	DEBUG
    switch (sv -> sv_type) {
	case SV_START: 
	    advise (LLOG_DEBUG, NULLCP,
		    "activity start indication: %*.*s, %d bytes",
		    sv -> sv_id.sd_len, sv -> sv_id.sd_len,
		    sv -> sv_id.sd_data, sv -> sv_cc);
	    break;

	case SV_RESUME: 
	    advise (LLOG_DEBUG, NULLCP,
		    "activity resume indication: id=%*.*s oid=%*.*s connect=%s ssn=%d, %d bytes",
		    sv -> sv_id.sd_len, sv -> sv_id.sd_len,
		    sv -> sv_id.sd_data, sv -> sv_oid.sd_len,
		    sv -> sv_oid.sd_len, sv -> sv_oid.sd_data,
		    sprintref (&sv -> sv_connect), sv -> sv_ssn, sv -> sv_cc);
	    break;

	case SV_INTRIND: 
	    advise (LLOG_DEBUG, NULLCP,
		    "activity interrupt indication %d, %d bytes",
		    sv -> sv_reason, sv -> sv_cc);
	    break;

	case SV_INTRCNF: 
	    advise (LLOG_DEBUG, NULLCP,
		    "activity interrupt confirmation, %d bytes", sv -> sv_cc);
	    break;

	case SV_DISCIND: 
	    advise (LLOG_DEBUG, NULLCP,
		    "activity discard indication %d, %d bytes",
		    sv -> sv_reason, sv -> sv_cc);
	    break;

	case SV_DISCCNF: 
	    advise (LLOG_DEBUG, NULLCP,
		    "activity discard confirmation, %d bytes", sv -> sv_cc);
	    break;

	case SV_ENDIND: 
	    advise (LLOG_DEBUG, NULLCP, "activity end indication %d, %d bytes",
		    sv -> sv_ssn, sv -> sv_cc);
	    break;

	case SV_ENDCNF: 
	    advise (LLOG_DEBUG, NULLCP, "activity end confirmation, %d bytes",
		    sv -> sv_cc);
	    break;

	default: 
	    advise (LLOG_DEBUG, NULLCP,
		    "unknown activity indication=0x%x, %d bytes",
		    sv -> sv_type, sv -> sv_cc);
	    break;
    }
#endif

    switch (sv -> sv_type) {
	case SV_START: 
	case SV_RESUME: 
	    break;

	case SV_INTRIND: 
	    if (SActIntrResponse (sd, si) == NOTOK)
		ss_adios (sa, "S-ACTIVITY-INTERRUPT.RESPONSE");
	    owned = 0;
	    break;

	case SV_INTRCNF: 
	    adios (NULLCP, "got activity interrupt confirmation");

	case SV_DISCIND: 
	    if (SActDiscResponse (sd, si) == NOTOK)
		ss_adios (sa, "S-ACTIVITY-DISCARD.RESPONSE");
	    owned = 0;
	    break;

	case SV_DISCCNF: 
	    adios (NULLCP, "got activity discard confirmation");

	case SV_ENDIND: 
	    if (SActEndResponse (sd, mymode == echo ? sv -> sv_data : NULLCP,
			mymode == echo ? sv -> sv_cc : 0, si) == NOTOK)
		ss_adios (sa, "S-ACTIVITY-END.RESPONSE");
	    break;

	case SV_ENDCNF: 
	    adios (NULLCP, "got activity end confirmation");

	default: 
	    adios (NULLCP, "unknown activity indication=0x%x", sv -> sv_type);
    }

    SVFREE (sv);
}

/*  */

static int ss_reportindication (sd, sp)
int	sd;
struct SSAPreport *sp;
{
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;

#ifdef	DEBUG
    advise (LLOG_DEBUG, NULLCP, "%s report %d, %d bytes",
	    sp -> sp_peer ? "user" : "provider", sp -> sp_reason, sp -> sp_cc);
#endif

    if (requirements & SR_DAT_EXISTS) {
	if (SGTokenRequest (sd, ST_DAT_TOKEN, si) == NOTOK)
	    ss_adios (sa, "S-TOKEN-GIVE.REQUEST");
	else
	    owned &= ~ST_DAT_TOKEN;
#ifdef	DEBUG
	advise (LLOG_DEBUG, NULLCP, "cleared");
#endif
    }
    else
	if (SUAbortRequest (sd, NULLCP, 0, si) == NOTOK)
	    ss_adios (sa, "S-U-ABORT.REQUEST");
	else
	    adios (NULLCP, "aborted");

    SPFREE (sp);
}

/*  */

static int ss_finishindication (sd, sf)
int	sd;
register struct SSAPfinish *sf;
{
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort *sa = &si -> si_abort;

    if (sf -> sf_cc > 0)
	advise (LLOG_NOTICE, NULLCP, "S-RELEASE.INDICATION: %d bytes",
		sf -> sf_cc);
    else
	advise (LLOG_NOTICE, NULLCP, "S-RELEASE.INDICATION");

    if (SRelResponse (sd, SC_ACCEPT, mymode == echo ? sf -> sf_data : NULL,
		mymode == echo ? sf -> sf_cc : 0, si) == NOTOK)
	ss_adios (sa, "S-RELEASE.RESPONSE");

    SFFREE (sf);

    exit (0);
}


/* ARGSUSED */

static int ss_abortindication (sd, sa)
int	sd;
register struct SSAPabort *sa;
{
    if (!sa -> sa_peer)
	ss_adios (sa, "S-P-ABORT.INDICATION");

    if (sa -> sa_cc > 0)
	ss_advise (sa, "S-U-ABORT.INDICATION");
    else
	advise (LLOG_NOTICE, NULLCP, "S-U-ABORT.INDICATION");

    exit (1);
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

    advise (LLOG_NOTICE, NULLCP, "%s: %s", event, buffer);

    SAFREE (sa);
}

/*    PSAP */

#define	PMASK \
	"\020\01MANAGEMENT\02RESTORATION"

#define dotoken(requires,shift,bit,type) \
{ \
    if (srequirements & requires) \
	switch (ps -> ps_settings & (ST_MASK << shift)) { \
	    case ST_CALL_VALUE << shift: \
		advise (LLOG_DEBUG, NULLCP, "%s token: choice", type); \
		ps -> ps_settings &= ~(ST_MASK << shift); \
		ps -> ps_settings |= ST_INIT_VALUE << shift; \
		break; \
 \
	    case ST_INIT_VALUE: \
		advise (LLOG_DEBUG, NULLCP, "%s token: initiator", type); \
		break; \
 \
	    case ST_RESP_VALUE: \
		advise (LLOG_DEBUG, NULLCP, "%s token: responder", type); \
		owned |= bit; \
		break; \
 \
	    default: \
		adios (NULLCP, "%s token: reserved", type); \
		break; \
	} \
}


static int  prequirements = 0;
#define	srequirements	requirements

static struct PSAPdata ixs;
static struct PSAPdata *ix = &ixs;

/*  */

static int  ps_main (argc, argv)
int	argc;
char  **argv;
{
    int     async,
	    result,
            sd;
#ifdef	DEBUG
    int	    i;
#endif
    register struct dispatch   *ds;
    register struct isoservent *is;
    struct PSAPdata pxs;
    register struct PSAPdata   *px = &pxs;
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort  *pa = &pi -> pi_abort;
    struct AcSAPstart   acss;
    register struct AcSAPstart   *acs = &acss;
    register struct PSAPstart  *ps = &acs -> acs_start;
    register struct PSAPctxlist *pl = &ps -> ps_ctxlist;
    struct AcSAPindication  acis;
    register struct AcSAPindication  *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;

    if (isacs) {
	if (AcInit (argc, argv, acs, aci) == NOTOK)
	    acs_adios (aca, "(Ac)initialization fails");

	advise (LLOG_NOTICE, NULLCP,
		"A-ASSOCIATE.INDICATION: <%d, %s, %s, %s, %d>",
		acs -> acs_sd, oid2ode (acs -> acs_context),
		sprintaei (&acs -> acs_callingtitle),
		sprintaei (&acs -> acs_calledtitle), acs -> acs_ninfo);

	advise (LLOG_NOTICE, NULLCP,
		"PSAP: <%d, %s, %s, %d, %s,",
		ps -> ps_sd, 
		paddr2str (&ps -> ps_calling, NULLNA),
		paddr2str (&ps -> ps_called, NULLNA),
		pl -> pc_nctx, sprintb (ps -> ps_prequirements, PMASK));
	advise (LLOG_NOTICE, NULLCP,
		"  %s, %d, %d>",
		sprintb (ps -> ps_srequirements, RMASK), ps -> ps_isn,
		ps -> ps_ssdusize);

	sd = acs -> acs_sd;
    }
    else {
	if (PInit (argc, argv, ps, pi) == NOTOK)
	    ps_adios (pa, "(P)initialization fails");
	advise (LLOG_NOTICE, NULLCP,
		"P-CONNECT.INDICATION: <%d, %s, %s, %d, %s,",
		ps -> ps_sd,
		paddr2str (&ps -> ps_calling, NULLNA),
		paddr2str (&ps -> ps_called, NULLNA),
		pl -> pc_nctx,
		sprintb (ps -> ps_prequirements, PMASK));
	advise (LLOG_NOTICE, NULLCP,
		"  %s, %d, %d>",
		sprintb (ps -> ps_srequirements, RMASK), ps -> ps_isn,
		ps -> ps_ssdusize);

	sd = ps -> ps_sd;
    }
#ifdef	DEBUG
    if (ps -> ps_ninfo > 0)
	advise (LLOG_DEBUG, NULLCP, "greetings: %d elements", ps -> ps_ninfo);

    for (i = 0; i < pl -> pc_nctx; i++)
	advise (LLOG_DEBUG, NULLCP, " ctx %d:  %d %s 0x%x %d",
		i, pl -> pc_ctx[i].pc_id, sprintoid (pl -> pc_ctx[i].pc_asn),
		pl -> pc_ctx[i].pc_atn, pl -> pc_ctx[i].pc_result);
    if (ps -> ps_defctx)
	advise (LLOG_DEBUG, NULLCP, " default: %s %d",
		sprintoid (ps -> ps_defctx), ps -> ps_defctxresult);
#endif

    ps -> ps_prequirements &= PR_MANAGEMENT | PR_RESTORATION;
    prequirements = ps -> ps_prequirements;
    advise (LLOG_DEBUG, NULLCP, "new presentation requirements: %s",
		sprintb (ps -> ps_prequirements, PMASK));
    ps -> ps_srequirements &= SR_HALFDUPLEX | SR_DUPLEX | SR_EXPEDITED
	| SR_MINORSYNC | SR_MAJORSYNC | SR_RESYNC | SR_ACTIVITY
	| SR_NEGOTIATED | SR_CAPABILITY | SR_EXCEPTIONS | SR_TYPEDATA;
    if ((ps -> ps_srequirements & SR_HALFDUPLEX)
	    && (ps -> ps_srequirements & SR_DUPLEX))
	ps -> ps_srequirements &= ~SR_DUPLEX;
    srequirements = ps -> ps_srequirements;
    advise (LLOG_DEBUG, NULLCP, "new session requirements: %s",
		sprintb (ps -> ps_srequirements, RMASK));
    dotokens ();
    advise (LLOG_DEBUG, NULLCP, "initial tokens: %s", sprintb (owned, TMASK));
    if (!(ps -> ps_srequirements & (SR_MINORSYNC | SR_MAJORSYNC | SR_RESYNC)))
	ps -> ps_isn = SERIAL_NONE;

    if (isacs) {
	struct TSAPaddr *ta = &ps -> ps_called.pa_addr.sa_addr;

	if (is = getisoserventbyselector ("tsap", ta -> ta_selector,
			ta -> ta_selectlen))
	    for (ds = acs_dispatches; ds -> ds_entity; ds++)
		if (strcmp (ds -> ds_entity, is -> is_entity) == 0) {
		    mymode = ds -> ds_mode;
		    break;
		}
    }
    else
	if (is = getisoserventbyselector ("psap", ps -> ps_called.pa_selector,
			ps -> ps_called.pa_selectlen))
	    for (ds = ps_dispatches; ds -> ds_entity; ds++)
		if (strcmp (ds -> ds_entity, is -> is_entity) == 0) {
		    mymode = ds -> ds_mode;
		    break;
		}

    async = 0;
    for (argv++; *argv; argv++) {
	if (strcmp (*argv, "-async") == 0) {
	    async++;
	    continue;
	}
	if (strcmp (*argv, "-sync") == 0) {
	    async = 0;
	    continue;
	}

	advise (LLOG_NOTICE, NULLCP, "unknown argument \"%s\"", *argv);
    }

    if (isacs) {
	switch (mymode) {
	    case echo:
		if (AcAssocResponse (sd, ACS_ACCEPT, ACS_USER_NULL,
			    NULLOID, NULLAEI, NULLPA, pl,
			    ps -> ps_defctxresult, ps -> ps_prequirements,
			    ps -> ps_srequirements, ps -> ps_isn,
			    ps -> ps_settings, &ps -> ps_connect,
			    acs -> acs_info, acs -> acs_ninfo, aci) == NOTOK)
		    acs_adios (aca, "A-ASSOCIATE.RESPONSE (accept)");
		break;

	    case sink:
		if (AcAssocResponse (sd, ACS_ACCEPT, ACS_USER_NULL,
			    NULLOID, NULLAEI, NULLPA, pl,
			    ps -> ps_defctxresult, ps -> ps_prequirements,
			    ps -> ps_srequirements, ps -> ps_isn,
			    ps -> ps_settings, &ps -> ps_connect,
			    NULLPEP, 0, aci) == NOTOK)
		    acs_adios (aca, "A-ASSOCIATE.RESPONSE (accept)");
		break;

	    default:
		if (AcAssocResponse (sd, ACS_PERMANENT, ACS_CONTEXT,
			    NULLOID, NULLAEI, NULLPA, pl,
			    ps -> ps_defctxresult, 0, 0, SERIAL_NONE, 0,
			    &ps -> ps_connect, NULLPEP, 0, aci) == NOTOK)
		    acs_adios (aca, "A-ASSOCIATE.RESPONSE (reject)");
		advise (LLOG_NOTICE, NULLCP, "rejected");
		exit (1);
	}
	
	ACSFREE (acs);

	{
	    struct RoSAPindication rois;
	    register struct RoSAPpreject *rop = &rois.roi_preject;

	    if (RoSetService (sd, RoPService, &rois) == NOTOK)
	        ros_adios (rop, "set RO/PS fails");
	}

	do_ros (sd, async);
	return;
    }
    else {
	switch (mymode) {
	    case echo: 
		if (PConnResponse (sd, PC_ACCEPT, NULLPA,
			    pl, ps -> ps_defctxresult,
			    ps -> ps_prequirements, ps -> ps_srequirements,
			    ps -> ps_isn, ps -> ps_settings, &ps -> ps_connect,
			    ps -> ps_info, ps -> ps_ninfo, pi) == NOTOK)
		    ps_adios (pa, "P-CONNECT.RESPONSE (accept)");
		break;

	    case sink: 
		if (PConnResponse (sd, PC_ACCEPT, NULLPA,
			    pl, ps -> ps_defctxresult,
			    ps -> ps_prequirements, ps -> ps_srequirements,
			    ps -> ps_isn, ps -> ps_settings, &ps -> ps_connect,
			    NULLPEP, 0, pi) == NOTOK)
		    ps_adios (pa, "P-CONNECT.RESPONSE (accept)");
		break;

	    default: 
		if (PConnResponse (sd, PC_REJECTED, NULLPA, 
			    pl, ps -> ps_defctxresult, 0, 0, SERIAL_NONE, 0,
			    &ps -> ps_connect, NULLPEP, 0, pi) == NOTOK)
		    ps_adios (pa, "P-CONNECT.RESPONSE (reject)");
		advise (LLOG_NOTICE, NULLCP, "rejected");
		exit (1);
	}

	PSFREE (ps);
    }

    if (async) {
	if (PSetIndications (sd, ps_dataindication, ps_tokenindication,
		    ps_syncindication, ps_actindication, ps_reportindication,
		ps_finishindication, ps_abortindication, pi) == NOTOK)
	    ps_adios (pa, "set ASYNC fails");

	for (;;)
	    pause ();
    }

    for (;;)
	switch (result = PReadRequest (sd, px, NOTOK, pi)) {
	    case NOTOK: 
		ps_abortindication (sd, pa);

	    case OK: 
		ps_dataindication (sd, px);
		break;

	    case DONE: 
		switch (pi -> pi_type) {
		    case PI_TOKEN: 
			ps_tokenindication (sd, &pi -> pi_token);
			break;

		    case PI_SYNC: 
			ps_syncindication (sd, &pi -> pi_sync);
			break;

		    case PI_ACTIVITY:
			ps_actindication (sd, &pi -> pi_activity);
			break;

		    case PI_REPORT:
			ps_reportindication (sd, &pi -> pi_report);
			break;

		    case PI_FINISH: 
			ps_finishindication (sd, &pi -> pi_finish);
			break;

		    default: 
			adios (NULLCP, "unknown indication type=0x%x",
				pi -> pi_type);
		}
		break;

	    default: 
		adios (NULLCP, "unknown return from PReadRequest=%d", result);
	}
}

#undef	dotoken

/*  */

static int ps_dataindication (sd, px)
int	sd;
register struct PSAPdata *px;
{
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort  *pa = &pi -> pi_abort;

#ifdef	DEBUG
    switch (px -> px_type) {
	case SX_NORMAL: 
	    advise (LLOG_DEBUG, NULLCP, "normal data, %d elements",
		    px -> px_ninfo);
	    break;

	case SX_EXPEDITED: 
	    advise (LLOG_DEBUG, NULLCP, "expedited data, %d elements",
		    px -> px_ninfo);
	    break;

	case SX_TYPED: 
	    advise (LLOG_DEBUG, NULLCP, "typed data, %d elements",
		    px -> px_ninfo);
	    break;

	case SX_CAPDIND: 
	    advise (LLOG_DEBUG, NULLCP, "capability data, %d elements",
		    px -> px_ninfo);
	    break;

	case SX_CAPDCNF: 
	    advise (LLOG_DEBUG, NULLCP, "capability data ack, %d elements",
		    px -> px_ninfo);

	default:
	    advise (LLOG_DEBUG, NULLCP,
		    "unknown data indication type=0x%x, %d elements",
		    px -> px_type, px -> px_ninfo);
    }
#endif

    switch (px -> px_type) {
	case SX_NORMAL: 
	    if (mymode == sink)
		break;
	    if (srequirements & SR_HALFDUPLEX) {
		if (ix -> px_ninfo) {
		    if (PUAbortRequest (sd, NULLPEP, 0, pi) == NOTOK)
			ps_adios (pa, "P-U-ABORT.REQUEST");
		    else
			adios (NULLCP, "protocol screw-up");
		}
		else {
		    *ix = *px;	/* struct copy */
		    bzero ((char *) px, sizeof *px);
		    if (!(owned & ST_DAT_TOKEN)
			    && PPTokenRequest (sd, ST_DAT_TOKEN, NULLPEP,
				0, pi) == NOTOK)
			ps_adios (pa, "P-TOKEN-PLEASE.REQUEST");
		}
	    }
	    else
		if (PDataRequest (sd, px -> px_info, px -> px_ninfo, pi)
			== NOTOK)
		    ps_adios (pa, "P-DATA.REQUEST");
	    break;

	case SX_EXPEDITED: 
	    if (mymode == sink)
		break;
	    if (PExpdRequest (sd, px -> px_info, px -> px_ninfo, pi) == NOTOK)
		ps_adios (pa, "P-EXPEDITED-DATA.REQUEST");
	    break;

	case SX_TYPED: 
	    if (mymode == sink)
		break;
	    if (PTypedRequest (sd, px -> px_info, px -> px_ninfo, pi) == NOTOK)
		ps_adios (pa, "P-TYPED-DATA.REQUEST");
	    break;

	case SX_CAPDIND: 
	    if (PCapdResponse (sd, px -> px_info, px -> px_ninfo, pi) == NOTOK)
		ps_adios (pa, "P-CAPABILITY-DATA.REQUEST");
	    break;

	case SX_CAPDCNF: 
	    adios (NULLCP, "got capability data response");

	default: 
	    adios (NULLCP, "unknown data indication type=0x%x", px -> px_type);
    }

    PXFREE (px);
}

/*  */

static int ps_tokenindication (sd, pt)
int	sd;
register struct PSAPtoken *pt;
{
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort *pa = &pi -> pi_abort;

#ifdef	DEBUG
    advise (LLOG_DEBUG, NULLCP, "%s tokens: %s, %d elements",
	    pt -> pt_type == ST_PLEASE ? "please"
	    : pt -> pt_type == ST_GIVE ? "give" : "control",
	    sprintb ((int) pt -> pt_tokens, TMASK),
	    pt -> pt_ninfo);
#endif

    switch (pt -> pt_type) {
	case ST_GIVE:
	case ST_CONTROL:
	    owned = pt -> pt_owned;
	    break;

	case ST_PLEASE:
	    break;

	default:
	    adios (NULLCP, "unknown token indication type=0x%x",
		    pt -> pt_type);
    }
    
    if ((owned & ST_DAT_TOKEN) && ix -> px_ninfo)
	if (PDataRequest (sd, ix -> px_info, ix -> px_ninfo, pi) == NOTOK)
	    ps_adios (pa, "P-DATA.REQUEST");
	else {
	    PXFREE (ix);
	    bzero ((char *) ix, sizeof *ix);
	}

    switch (pt -> pt_type) {
	case ST_GIVE:
	    break;

	default:
	    if (PGTokenRequest (sd, (int) pt -> pt_tokens, pi) == NOTOK)
		ps_adios (pa, "P-TOKEN-GIVE.REQUEST");
	    else
		owned &= ~pt -> pt_tokens;
	    break;
    }

    PTFREE (pt);
}

/*  */

static int ps_syncindication (sd, pn)
int	sd;
register struct PSAPsync *pn;
{
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort  *pa = &pi -> pi_abort;

#ifdef	DEBUG
    switch (pn -> pn_type) {
	case SN_MAJORIND: 
	    advise (LLOG_DEBUG, NULLCP, "majorsync indication %d",
		    pn -> pn_ssn);
	    break;

	case SN_MAJORCNF: 
	    advise (LLOG_DEBUG, NULLCP, "majorsync confirmation %d",
		    pn -> pn_ssn);
	    break;

	case SN_MINORIND: 
	    advise (LLOG_DEBUG, NULLCP, "minorsync indication %d%s",
		    pn -> pn_ssn, pn -> pn_options == SYNC_CONFIRM
		    ? " (wants confirmation)" : NULLCP);
	    break;

	case SN_MINORCNF: 
	    advise (LLOG_DEBUG, NULLCP, "minorsync confirmation %d",
		    pn -> pn_ssn);
	    break;

	case SN_RESETIND: 
	    advise (LLOG_DEBUG, NULLCP, "resync indication type=%d %d",
		    pn -> pn_options, pn -> pn_ssn);
	    break;

	case SN_RESETCNF: 
	    advise (LLOG_DEBUG, NULLCP, "resync confirmation %d",
		    pn -> pn_ssn);
	    break;

	default: 
	    advise (LLOG_DEBUG, NULLCP, "unknown sync indication=0x%x, ssn=%d",
		    pn -> pn_type, pn -> pn_ssn);
	    break;
    }
    advise (LLOG_DEBUG, NULLCP, "%d elements", pn -> pn_ninfo);
#endif

    switch (pn -> pn_type) {
	case SN_MAJORIND: 
	    if (PMajSyncResponse (sd,
			mymode == echo ? pn -> pn_info : NULLPEP,
			mymode == echo ? pn -> pn_ninfo : 0, pi) == NOTOK)
		ps_adios (pa, "P-MAJOR-SYNC.RESPONSE");
	    break;

	case SN_MAJORCNF: 
	    adios (NULLCP, "got majorsync confirmation");

	case SN_MINORIND: 
	    if (pn -> pn_options == SYNC_CONFIRM)
		if (PMinSyncResponse (sd, pn -> pn_ssn,
			    mymode == echo ? pn -> pn_info : NULLPEP,
			    mymode == echo ? pn -> pn_ninfo : 0, pi) == NOTOK)
		    ps_adios (pa, "P-MINOR-SYNC.RESPONSE");
	    break;

	case SN_MINORCNF: 
	    adios (NULLCP, "got minorsync confirmation");

	case SN_RESETIND: 
#define	dotoken(requires,shift,bit,type) \
{ \
	    if (srequirements & requires) \
		switch (pn -> pn_settings & (ST_MASK << shift)) { \
		    case ST_CALL_VALUE << shift: \
			pn -> pn_settings &= ~(ST_MASK << shift); \
			pn -> pn_settings |= ST_RESP_VALUE << shift; \
		    case ST_RESP_VALUE << shift: \
			owned |= bit; \
			break; \
 \
		    case ST_INIT_VALUE << shift: \
			owned &= ~bit; \
			break; \
 \
		    default: \
			adios (NULLCP, "%s token: reserved", type); \
			break; \
		} \
}
	    dotokens ();
#undef	dotoken
	    if (PReSyncRequest (sd, SYNC_ABANDON, SERIAL_NONE,
			pn -> pn_settings,
			mymode == echo ? pn -> pn_info : NULLPEP,
			mymode == echo ? pn -> pn_ninfo : 0, pi) == NOTOK)
		ps_adios (pa, "P-RESYNCHRONIZE.REQUEST");
	    break;
	
	case SN_RESETCNF: 
	    break;

	default: 
	    adios (NULLCP, "unknown sync indication type=0x%x", pn -> pn_type);
    }

    PNFREE (pn);
}

/*  */

static int ps_actindication (sd, pv)
int	sd;
register struct PSAPactivity *pv;
{
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort  *pa = &pi -> pi_abort;

#ifdef	DEBUG
    switch (pv -> pv_type) {
	case SV_START: 
	    advise (LLOG_DEBUG, NULLCP, "activity start indication: %*.*s",
		    pv -> pv_id.sd_len, pv -> pv_id.sd_len,
		    pv -> pv_id.sd_data);
	    break;

	case SV_RESUME: 
	    advise (LLOG_DEBUG, NULLCP,
		    "activity resume indication: id=%*.*s oid=%*.*s connect=%s ssn=%d",
		    pv -> pv_id.sd_len, pv -> pv_id.sd_len,
		    pv -> pv_id.sd_data, pv -> pv_oid.sd_len,
		    pv -> pv_oid.sd_len, pv -> pv_oid.sd_data,
		    sprintref (&pv -> pv_connect), pv -> pv_ssn);
	    break;

	case SV_INTRIND: 
	    advise (LLOG_DEBUG, NULLCP, "activity interrupt indication %d",
		    pv -> pv_reason);
	    break;

	case SV_INTRCNF: 
	    advise (LLOG_DEBUG, NULLCP, "activity interrupt confirmation");
	    break;

	case SV_DISCIND: 
	    advise (LLOG_DEBUG, NULLCP, "activity discard indication %d",
		    pv -> pv_reason);
	    break;

	case SV_DISCCNF: 
	    advise (LLOG_DEBUG, NULLCP, "activity discard confirmation");
	    break;

	case SV_ENDIND: 
	    advise (LLOG_DEBUG, NULLCP, "activity end indication %d",
		    pv -> pv_ssn);
	    break;

	case SV_ENDCNF: 
	    advise (LLOG_DEBUG, NULLCP, "activity end confirmation");
	    break;

	default: 
	    advise (LLOG_DEBUG, NULLCP, "unknown activity indication=0x%x",
		    pv -> pv_type);
	    break;
    }
    advise (LLOG_DEBUG, NULLCP, "%d elements", pv -> pv_ninfo);
#endif

    switch (pv -> pv_type) {
	case SV_START: 
	case SV_RESUME: 
	    break;

	case SV_INTRIND: 
	    if (PActIntrResponse (sd, pi) == NOTOK)
		ps_adios (pa, "P-ACTIVITY-INTERRUPT.RESPONSE");
	    owned = 0;
	    break;

	case SV_INTRCNF: 
	    adios (NULLCP, "got activity interrupt confirmation");

	case SV_DISCIND: 
	    if (PActDiscResponse (sd, pi) == NOTOK)
		ps_adios (pa, "P-ACTIVITY-DISCARD.RESPONSE");
	    owned = 0;
	    break;

	case SV_DISCCNF: 
	    adios (NULLCP, "got activity discard confirmation");

	case SV_ENDIND: 
	    if (PActEndResponse (sd, mymode == echo ? pv -> pv_info : NULLPEP,
			mymode == echo ? pv -> pv_ninfo : 0, pi) == NOTOK)
		ps_adios (pa, "P-ACTIVITY-END.RESPONSE");
	    break;

	case SV_ENDCNF: 
	    adios (NULLCP, "got activity end confirmation");

	default: 
	    adios (NULLCP, "unknown activity indication=0x%x", pv -> pv_type);
    }

    PVFREE (pv);
}

/*  */

static int ps_reportindication (sd, pp)
int	sd;
register struct PSAPreport *pp;
{
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort  *pa = &pi -> pi_abort;

#ifdef	DEBUG
    advise (LLOG_NOTICE, NULLCP, "%s report %d, %d elements",
	    pp -> pp_peer ? "user" : "provider", pp -> pp_reason,
	    pp -> pp_ninfo);
#endif

    if (srequirements & SR_DAT_EXISTS) {
	if (PGTokenRequest (sd, ST_DAT_TOKEN, pi) == NOTOK)
	    ps_adios (pa, "P-TOKEN-GIVE.REQUEST");
	else
	    owned &= ~ST_DAT_TOKEN;
#ifdef	DEBUG
	advise (LLOG_DEBUG, NULLCP, "cleared");
#endif
    }
    else
	if (PUAbortRequest (sd, NULLPEP, 0, pi) == NOTOK)
	    ps_adios (pa, "P-U-ABORT.REQUEST");
	else
	    adios (NULLCP, "aborted");

    PPFREE (pp);
}

/*  */

static int ps_finishindication (sd, pf)
int	sd;
register struct PSAPfinish *pf;
{
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;
    register struct PSAPabort *pa = &pi -> pi_abort;
    struct AcSAPindication  acis;
    register struct AcSAPabort *aca = &acis.aci_abort;
    register struct AcSAPfinish *acf = &acis.aci_finish;

    if (isacs) {
	if (AcFINISHser (sd, pf, &acis) == NOTOK)
	    acs_adios (aca, "AcFINISHser");
	ros_finish (sd, acf);
	return;
    }

    advise (LLOG_NOTICE, NULLCP, "P-RELEASE.INDICATION: %d elements",
	    pf -> pf_ninfo);

    if (PRelResponse (sd, SC_ACCEPT, mymode == echo ? pf -> pf_info : NULLPEP,
		mymode == echo ? pf -> pf_ninfo : 0, pi) == NOTOK)
	ps_adios (pa, "P-RELEASE.RESPONSE");

    PFFREE (pf);
    
    exit (0);
}


/* ARGSUSED */

static int ps_abortindication (sd, pa)
int	sd;
register struct PSAPabort *pa;
{
    struct AcSAPindication  acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;

    if (isacs) {
	if (AcABORTser (sd, pa, aci) == NOTOK)
	    acs_adios (aca, "AcABORTser");
	advise (LLOG_NOTICE, NULLCP, "A-%sABORT.INDICATION: [%s] %d elements",
		aca -> aca_source != ACA_USER ? "P-" : "",
		AcErrString (aca -> aca_reason), aca -> aca_ninfo);

	ACAFREE (aca);
	
	exit (1);	
    }

    if (!pa -> pa_peer)
	ps_adios (pa, "P-P-ABORT.INDICATION");

    advise (LLOG_NOTICE, NULLCP, "P-U-ABORT.INDICATION: %d elements",
	pa -> pa_ninfo);
    PAFREE (pa);

    exit (1);
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

    advise (LLOG_NOTICE, NULLCP, "%s: %s", event, buffer);
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

    advise (LLOG_NOTICE, NULLCP, "%s: %s (source %d)", event, buffer,
		aca -> aca_source);
}

/*    RtSAP */

static int  rts_main (argc, argv)
int	argc;
char  **argv;
{
    int     async,
            result,
	    ros,
            sd;
#ifdef	DEBUG
    int	    i;
#endif
    struct dispatch *ds;
    struct isoservent  *is;
    struct RtSAPstart   rtss;
    register struct RtSAPstart *rts = &rtss;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;
    register struct RtSAPabort   *rta = &rti -> rti_abort;
    register struct AcSAPstart *acs = &rts -> rts_start;
    register struct PSAPstart *ps = &acs -> acs_start;
    register struct PSAPctxlist *pl = &ps -> ps_ctxlist;

    if (isacs) {
	if (RtInit (argc, argv, rts, rti) == NOTOK)
	    rts_adios (rta, "(Rt)initialization fails");
	advise (LLOG_NOTICE, NULLCP, "RT-OPEN.INDICATION: <%d, %s, %s, 0x%x>",
		rts -> rts_sd,
		rts -> rts_mode == RTS_TWA ? "twa" : "mono",
		rts -> rts_turn == RTS_RESPONDER ? "responder" : "initiator",
		rts -> rts_data);

	advise (LLOG_NOTICE, NULLCP, "ACSE: <%d, %s, %s, %s, %d>",
		acs -> acs_sd, oid2ode (acs -> acs_context),
		sprintaei (&acs -> acs_callingtitle),
		sprintaei (&acs -> acs_calledtitle), acs -> acs_ninfo);

	advise (LLOG_NOTICE, NULLCP,
		"PSAP: <%d, %s, %s, %d, %s,",
		ps -> ps_sd, 
		paddr2str (&ps -> ps_calling, NULLNA),
		paddr2str (&ps -> ps_called, NULLNA),
		pl -> pc_nctx, sprintb (ps -> ps_prequirements, PMASK));
	advise (LLOG_NOTICE, NULLCP,
		"  %s, %d, %d>",
		sprintb (ps -> ps_srequirements, RMASK), ps -> ps_isn,
		ps -> ps_ssdusize);

#ifdef	DEBUG
	{
	    if (ps -> ps_ninfo > 0)
		advise (LLOG_DEBUG, NULLCP, "greetings: %d elements",
			ps -> ps_ninfo);

	    for (i = 0; i < pl -> pc_nctx; i++)
		advise (LLOG_DEBUG, NULLCP, " ctx %d:  %d %s 0x%x %d",
			i, pl -> pc_ctx[i].pc_id,
			sprintoid (pl -> pc_ctx[i].pc_asn),
			pl -> pc_ctx[i].pc_atn, pl -> pc_ctx[i].pc_result);
	    if (ps -> ps_defctx)
		advise (LLOG_DEBUG, NULLCP, " default: %s %d",
			sprintoid (ps -> ps_defctx), ps -> ps_defctxresult);
	}
#endif

    }
    else {
	if (RtBInit (argc, argv, rts, rti) == NOTOK)
	    rts_adios (rta, "(RtB)initialization fails");
	advise (LLOG_NOTICE, NULLCP,
		"RT-BEGIN.INDICATION: <%d, %s, %s, <%d, %s>, 0x%x>",
		rts -> rts_sd, rts -> rts_mode == RTS_TWA ? "twa" :"monologue",
		rts -> rts_turn == RTS_RESPONDER ? "responder" : "initiator",
		ntohs (rts -> rts_port),
		saddr2str (&rts -> rts_initiator.rta_addr),
		rts -> rts_data);
    }

    if (rts -> rts_data) {
	if ((result = prim2num (rts -> rts_data)) == NOTOK
		&& rts -> rts_data -> pe_errno != PE_ERR_NONE)
	    adios (NULLCP, "error decoding hello: %s",
		    pe_error (rts -> rts_data -> pe_errno));

	advise (LLOG_DEBUG, NULLCP, "received greetings of %d", result);

	pe_free (rts -> rts_data);
	if ((rts -> rts_data = int2prim (result = getpid ())) == NULLPE)
	    adios (NULLCP, "unable to allocate hello");
    }

    sd = rts -> rts_sd;

    if (isacs) {
	struct TSAPaddr *ta = &ps -> ps_called.pa_addr.sa_addr;

	if (is = getisoserventbyselector ("tsap", ta -> ta_selector,
			ta -> ta_selectlen))
	    for (ds = rtse_dispatches; ds -> ds_entity; ds++)
		if (strcmp (ds -> ds_entity, is -> is_entity) == 0) {
		    mymode = ds -> ds_mode;
		    break;
		}
    }
    else
	if (is = getisoserventbyport ("rtsap", rts -> rts_port))
	    for (ds = rts_dispatches; ds -> ds_entity; ds++)
		if (strcmp (ds -> ds_entity, is -> is_entity) == 0) {
		    mymode = ds -> ds_mode;
		    break;
		}

    async = 0;
    ros = !isacs && strncmp (is -> is_entity, "ros_", strlen ("ros_")) == 0;
    for (argv++; *argv; argv++) {
	if (strcmp (*argv, "-async") == 0) {
	    async++;
	    continue;
	}
	if (strcmp (*argv, "-sync") == 0) {
	    async = 0;
	    continue;
	}
	if (strcmp (*argv, "-rtse") == 0)
	    continue;
	if (strcmp (*argv, "-rose") == 0) {
	    ros++;
	    continue;
	}

	advise (LLOG_NOTICE, NULLCP, "unknown argument \"%s\"", *argv);
    }

    if (isacs) {
	switch (mymode) {
	    case echo: 
		if (rts -> rts_mode == RTS_TWA)
		    goto rtse_accept;
	rtse_reject: ;
		if (RtOpenResponse (sd, ACS_USER_NOREASON, NULLOID, NULLAEI, 
			NULLPA, NULLPC, ps -> ps_defctxresult, NULLPE, rti)
			== NOTOK)
		    rts_adios (rta, "RT-OPEN.RESPONSE (reject)");
		advise (LLOG_NOTICE, NULLCP, "rejected");
		exit (1);

	    case sink: 
		if (rts -> rts_mode != RTS_TWA
			&& rts -> rts_turn != RTS_INITIATOR)
		    goto rtse_reject;
	rtse_accept: 
		if (RtOpenResponse (sd, ACS_ACCEPT, NULLOID, NULLAEI,
			NULLPA, pl, ps -> ps_defctxresult,
			rts -> rts_data, rti) == NOTOK)
		    rts_adios (rta, "RT-OPEN.RESPONSE (accept)");
		advise (LLOG_DEBUG, NULLCP, "sent greetings of %d", result);
		break;

	    default: 
		goto rtse_reject;
	}
    }
    else {
	switch (mymode) {
	    case echo: 
		if (rts -> rts_mode == RTS_TWA)
		    goto accept;
	reject: ;
		if (RtBeginResponse (sd, RTS_MODE, NULLPE, rti) == NOTOK)
		    rts_adios (rta, "RT-BEGIN.RESPONSE (reject)");
		advise (LLOG_NOTICE, NULLCP, "rejected");
		exit (1);

	    case sink: 
		if (rts -> rts_mode != RTS_TWA
			&& rts -> rts_turn != RTS_INITIATOR)
		    goto reject;
	accept: ;
		if (RtBeginResponse (sd, RTS_ACCEPT, rts -> rts_data, rti)
			== NOTOK)
		    rts_adios (rta, "RT-BEGIN.RESPONSE (accept)");
		advise (LLOG_DEBUG, NULLCP, "sent greetings of %d", result);
		break;

	    default: 
		if (RtBeginResponse (sd, RTS_VALIDATE, NULLPE, rti) == NOTOK)
		    rts_adios (rta, "RT-BEGIN.RESPONSE (reject)");
		advise (LLOG_NOTICE, NULLCP, "rejected");
		exit (1);
	}
    }

    RTSFREE (rts);

    if (ros) {
	struct RoSAPindication rois;
	register struct RoSAPpreject *rop = &rois.roi_preject;

	if (RoSetService (sd, RoRtService, &rois) == NOTOK)
	    ros_adios (rop, "set RO/RT fails");

	do_ros (sd, async);
	return;
    }

    if (async) {
	if (RtSetIndications (sd, rts_indication, rti) == NOTOK)
	    rts_adios (rta, "set ASYNC fails");

	for (;;)
	    pause ();
    }

    for (;;)
	switch (result = RtWaitRequest (sd, NOTOK, rti)) {
	    case NOTOK: 
	    case OK: 
	    case DONE: 
		rts_indication (sd, rti);
		break;

	    default: 
		adios (NULLCP, "unknown return from RtWaitRequest=%d", result);
	}
}

/*  */

static int  rts_indication (sd, rti)
int	sd;
register struct RtSAPindication *rti;
{
    switch (rti -> rti_type) {
	case RTI_TURN: 
	    rts_turn (sd, &rti -> rti_turn);
	    break;

	case RTI_TRANSFER: 
	    rts_transfer (sd, &rti -> rti_transfer);
	    break;

	case RTI_ABORT: 
	    rts_abort (sd, &rti -> rti_abort);
	    break;

	case RTI_CLOSE: 
	    rts_close (sd, &rti -> rti_close);
	    break;

	case RTI_FINISH:
	    rts_finish (sd, &rti -> rti_finish);
	    break;

	default: 
	    adios (NULLCP, "unknown indication type=%d", rti -> rti_type);
    }
}

/*  */

static int  rts_turn (sd, rtu)
int	sd;
register struct RtSAPturn *rtu;
{
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;
    register struct RtSAPabort *rta = &rti -> rti_abort;

    if (rtu -> rtu_please) {
	if (RtGTurnRequest (sd, rti) == NOTOK)
	    rts_adios (rta, "RT-TURN-GIVE.REQUEST");
    }
    else
	if (apdupe) {
	    if (RtTransferRequest (sd, apdupe, NOTOK, rti) == NOTOK)
		rts_adios (rta, "RT-TRANSFER.REQUEST");
	    pe_free (apdupe);
	    apdupe = NULLPE;
	}
}

/*  */

static int  rts_transfer (sd, rtt)
int	sd;
register struct RtSAPtransfer *rtt;
{
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;
    register struct RtSAPabort *rta = &rti -> rti_abort;
    static int  priority = 1;

    if (mymode == echo) {
	if (apdupe)
	    adios (NULLCP, "protocol screw-up");
	if (RtPTurnRequest (sd, priority++, rti) == NOTOK)
	    rts_adios (rta, "RT-TURN-PLEASE.REQUEST");
	apdupe = rtt -> rtt_data;
    }
    else
	RTTFREE (rtt);
}

/*  */

/* ARGSUSED */

static int  rts_abort (sd, rta)
int	sd;
register struct RtSAPabort *rta;
{
    if (rta -> rta_peer)
	rts_adios (rta, "RT-U-ABORT.INDICATION");

    if (RTS_FATAL (rta -> rta_reason))
	rts_adios (rta, "RT-P-ABORT.INDICATION");
    rts_advise (rta, "RT-P-ABORT.INDICATION");
}

/*  */

/* ARGSUSED */

static int  rts_close (sd, rtc)
int	sd;
struct RtSAPclose *rtc;
{
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;
    register struct RtSAPabort *rta = &rti -> rti_abort;

    advise (LLOG_NOTICE, NULLCP, "RT-END.INDICATION");

    if (RtEndResponse (sd, rti) == NOTOK)
	rts_adios (rta, "RT-END.RESPONSE");

    exit (0);
}

/*  */

static int  rts_finish (sd, acf)
int	sd;
register struct AcSAPfinish *acf;
{
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;
    register struct RtSAPabort *rta = &rti -> rti_abort;

    advise (LLOG_NOTICE, NULLCP, "RT-CLOSE.INDICATION: %d, %d elements",
	    acf -> acf_reason, acf -> acf_ninfo);

    if (RtCloseResponse (sd, ACR_NORMAL, mymode == echo
		? acf -> acf_info[0] : NULLPE, rti) == NOTOK)
	rts_adios (rta, "RT-CLOSE.RESPONSE");

    ACFFREE (acf);

    exit (0);
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

    advise (LLOG_NOTICE, NULLCP, "%s: %s", event, buffer);
}

/*    RoSAP */

static int  ros_main (argc, argv)
int	argc;
char  **argv;
{
    int     async,
            result,
            sd;
    struct dispatch *ds;
    struct isoservent  *is;
    struct RoSAPstart   ross;
    register struct RoSAPstart *ros = &ross;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject   *rop = &roi -> roi_preject;

    if (RoInit (argc, argv, ros, roi) == NOTOK)
	ros_adios (rop, "(Ro)initialization fails");
    advise (LLOG_NOTICE, NULLCP, "RO-BEGIN.INDICATION: <%d, <%d, %s>, 0x%x>",
	    ros -> ros_sd,
	    ntohs (ros -> ros_port),
	    saddr2str (&ros -> ros_initiator.roa_addr),
	    ros -> ros_data);
    if (ros -> ros_data) {
	if ((result = prim2num (ros -> ros_data)) == NOTOK
		&& ros -> ros_data -> pe_errno != PE_ERR_NONE)
	    adios (NULLCP, "error decoding hello: %s",
			pe_error (ros -> ros_data -> pe_errno));

	advise (LLOG_DEBUG, NULLCP, "received greetings of %d", result);

	pe_free (ros -> ros_data);
	if ((ros -> ros_data = int2prim (result = getpid ())) == NULLPE)
	    adios (NULLCP, "unable to allocate hello");
    }

    sd = ros -> ros_sd;

    if (is = getisoserventbyport ("rosap", ros -> ros_port))
	for (ds = ros_dispatches; ds -> ds_entity; ds++)
	    if (strcmp (ds -> ds_entity, is -> is_entity) == 0) {
		mymode = ds -> ds_mode;
		break;
	    }

    async = 0;
    for (argv++; *argv; argv++) {
	if (strcmp (*argv, "-async") == 0) {
	    async++;
	    continue;
	}
	if (strcmp (*argv, "-sync") == 0) {
	    async = 0;
	    continue;
	}

	advise (LLOG_NOTICE, NULLCP, "unknown argument \"%s\"", *argv);
    }

    switch (mymode) {
	case echo: 
	case sink: 
	    if (RoBeginResponse (sd, ROS_ACCEPT, ros -> ros_data, roi)
		    == NOTOK)
		ros_adios (rop, "RO-BEGIN.RESPONSE (accept)");
	    advise (LLOG_DEBUG, NULLCP, "sent greetings of %d", result);
	    break;

	default: 
	    if (RoBeginResponse (sd, ROS_VALIDATE, NULLPE, roi) == NOTOK)
		ros_adios (rop, "RO-BEGIN.RESPONSE (reject)");
	    advise (LLOG_NOTICE, NULLCP, "rejected");
	    exit (1);
    }

    ROSFREE (ros);

    do_ros (sd, async);
}

/*  */

static int  do_ros (sd, async)
int	sd,
	async;
{
    int     result;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject   *rop = &roi -> roi_preject;

    if ((nullpe = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_NULL))
	    == NULLPE)
	adios (NULLCP, "unable to allocate NULL PE");

    if (async) {
	if (RoSetIndications (sd, ros_indication, roi) == NOTOK)
	    ros_adios (rop, "set ASYNC fails");

	for (;;)
	    pause ();
    }

    for (;;)
	switch (result = RoWaitRequest (sd, NOTOK, roi)) {
	    case NOTOK: 
	    case OK: 
	    case DONE: 
		ros_indication (sd, roi);
		break;

	    default: 
		adios (NULLCP, "unknown return from RoWaitRequest=%d", result);
	}
}

/*  */

static int ros_indication (sd, roi)
int	sd;
register struct RoSAPindication *roi;
{
    switch (roi -> roi_type) {
	case ROI_INVOKE: 
	    ros_invoke (sd, &roi -> roi_invoke);
	    break;

	case ROI_RESULT: 
	    ros_result (sd, &roi -> roi_result);
	    break;

	case ROI_ERROR: 
	    ros_error (sd, &roi -> roi_error);
	    break;

	case ROI_UREJECT: 
	    ros_ureject (sd, &roi -> roi_ureject);
	    break;

	case ROI_PREJECT: 
	    ros_preject (sd, &roi -> roi_preject);
	    break;

	case ROI_END: 
	    ros_end (sd, &roi -> roi_end);
	    break;

	case ROI_FINISH:
	    ros_finish (sd, &roi -> roi_finish);
	    break;

	default: 
	    adios (NULLCP, "unknown indication type=%d", roi -> roi_type);
    }
}

/*  */

static int  ros_invoke (sd, rox)
int	sd;
register struct RoSAPinvoke *rox;
{
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject   *rop = &roi -> roi_preject;
    static int  ff = 0;

    if (ff++ & 0x01) {
	if (RoErrorRequest (sd, rox -> rox_id, ff,
		    mymode == echo ? rox -> rox_args : nullpe, ROS_NOPRIO,
		    roi) == NOTOK)
	    ros_adios (rop, "RO-ERROR.REQUEST");
    }
    else {
	if (RoResultRequest (sd, rox -> rox_id, rox -> rox_op,
		    mymode == echo ? rox -> rox_args : nullpe, ROS_NOPRIO,
		    roi) == NOTOK)
	    ros_adios (rop, "RO-RESULT.REQUEST");
    }

    ROXFREE (rox);
}

/*  */

static int  ros_result (sd, ror)
int	sd;
register struct RoSAPresult *ror;
{
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject   *rop = &roi -> roi_preject;

    if (RoURejectRequest (sd, &ror -> ror_id, ROS_RRP_UNRECOG, ROS_NOPRIO, roi)
	    == NOTOK)
	ros_adios (rop, "RO-REJECT-U.REQUEST");

    RORFREE (ror);
}

/*  */

static int  ros_error (sd, roe)
int	sd;
register struct RoSAPerror *roe;
{
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject   *rop = &roi -> roi_preject;

    if (RoURejectRequest (sd, &roe -> roe_id, ROS_REP_UNRECOG, ROS_NOPRIO, roi)
	    == NOTOK)
	ros_adios (rop, "RO-REJECT-U.REQUEST");

    ROEFREE (roe);
}

/*  */

/* ARGSUSED */

static int  ros_ureject (sd, rou)
int	sd;
register struct RoSAPureject *rou;
{
    if (rou -> rou_noid)
	advise (LLOG_NOTICE, NULLCP, "RO-REJECT-U.INDICATION: %s",
		RoErrString (rou -> rou_reason));
    else
	advise (LLOG_NOTICE, NULLCP, "RO-REJECT-U.INDICATION: %s (id=%d)",
		RoErrString (rou -> rou_reason), rou -> rou_id);
}

/*  */

/* ARGSUSED */

static int  ros_preject (sd, rop)
int	sd;
register struct RoSAPpreject *rop;
{
    if (ROS_FATAL (rop -> rop_reason))
	ros_adios (rop, "RO-REJECT-P.INDICATION");
    ros_advise (rop, "RO-REJECT-P.INDICATION");
}

/*  */

/* ARGSUSED */

static int  ros_end (sd, roe)
int	sd;
struct RoSAPend *roe;
{
    if (isrts) {
	struct RtSAPindication  rtis;
	register struct RtSAPindication *rti = &rtis;
	register struct RtSAPabort *rta = &rti -> rti_abort;

	advise (LLOG_NOTICE, NULLCP, "RT-END.INDICATION");
	if (RtEndResponse (sd, rti) == NOTOK)
	    rts_adios (rta, "RT-END.RESPONSE");
    }
    else {
	struct RoSAPindication  rois;
	register struct RoSAPindication *roi = &rois;
	register struct RoSAPpreject   *rop = &roi -> roi_preject;

	advise (LLOG_NOTICE, NULLCP, "RO-END.INDICATION");
	if (RoEndResponse (sd, roi) == NOTOK)
	    ros_adios (rop, "RO-END.RESPONSE");
    }

    exit (0);
}

/*  */

static int  ros_finish (sd, acf)
int	sd;
register struct AcSAPfinish *acf;
{
    if (isrts) {
	struct RtSAPindication  rtis;
	register struct RtSAPabort *rta = &rtis.rti_abort;

	advise (LLOG_NOTICE, NULLCP, "RT-CLOSE.INDICATION: %d, %d elements",
		acf -> acf_reason, acf -> acf_ninfo);

	if (RtCloseResponse (sd, ACR_NORMAL, mymode == echo
		    ? acf -> acf_info[0] : NULLPE, &rtis) == NOTOK)
	    rts_adios (rta, "RT-CLOSE.RESPONSE");
    }
    else {
	struct AcSAPindication  acis;
	register struct AcSAPabort *aca = &acis.aci_abort;

	advise (LLOG_NOTICE, NULLCP, "A-RELEASE.INDICATION: %d, %d elements",
		acf -> acf_reason, acf -> acf_ninfo);

	if (AcRelResponse (sd, ACS_ACCEPT, ACR_NORMAL, mymode == echo
		    ? acf -> acf_info : NULLPEP, mymode == echo
		    ? acf -> acf_ninfo : 0, &acis) == NOTOK)
	    acs_adios (aca, "A-RELEASE.RESPONSE");
    }

    ACFFREE (acf);

    exit (0);
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

    advise (LLOG_NOTICE, NULLCP, "%s: %s", event, buffer);
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
