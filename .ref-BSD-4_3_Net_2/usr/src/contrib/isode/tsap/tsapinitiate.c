/* tsapinitiate.c - TPM: initiator */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/tsap/RCS/tsapinitiate.c,v 7.4 91/02/22 09:47:27 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/tsap/RCS/tsapinitiate.c,v 7.4 91/02/22 09:47:27 mrose Interim $
 *
 *
 * $Log:	tsapinitiate.c,v $
 * Revision 7.4  91/02/22  09:47:27  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/08/08  14:03:13  mrose
 * stuff
 * 
 * Revision 7.2  90/07/09  14:51:26  mrose
 * sync
 * 
 * Revision 7.1  90/03/23  17:31:34  mrose
 * 8
 * 
 * Revision 7.0  89/11/23  22:30:44  mrose
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


/* LINTLIBRARY */

#include <stdio.h>
#include <signal.h>
#include "tpkt.h"
#include "mpkt.h"
#include "isoservent.h"
#include "tailor.h"


static struct nsapent {
    int	    ns_type;
    int	    ns_stack;

    IFP	    ns_open;
}     nsaps[] = {
#ifdef	TCP
    NA_TCP, TS_TCP, tcpopen,
#endif
#ifdef	X25
    NA_X25, TS_X25, x25open,
#endif
#ifdef BRIDGE_X25
    NA_BRG, TS_BRG, bridgeopen,
#endif
#ifdef	TP4
    NA_NSAP, TS_TP4, tp4open,
#endif

    NOTOK, TS_NONE, NULL
};


struct TSAPaddr *newtaddr (), *ta2norm (), *maketsbaddr ();

/*    T-(ASYN-)CONNECT.REQUEST */

int     TAsynConnRequest (calling, called, expedited, data, cc, qos,
			  tc, td, async)
struct TSAPaddr *calling,
		*called;
int	expedited,
	cc,
	async;
char   *data;
struct QOStype *qos;
struct TSAPconnect *tc;
struct TSAPdisconnect *td;
{
    register int  n;
    SBV     smask;
    int     result;

    isodetailor (NULLCP, 0);

#ifdef	notdef
    missingP (calling);
#endif
    missingP (called);
    if ((n = called -> ta_naddr) <= 0)
	return tsaplose (td, DR_PARAMETER, NULLCP,
			 "no NSAP addresses in called parameter");
    if (n > NTADDR)
	return tsaplose (td, DR_PARAMETER, NULLCP,
			 "too many NSAP addresses in called parameter");

    if ((called = ta2norm (called)) == NULLTA)
	return tsaplose (td, DR_PARAMETER, "invalid called parameter");
    toomuchP (data, cc, TS_SIZE, "initial");
#ifdef	notdef
    missingP (qos);
#endif
    missingP (td);

    smask = sigioblock ();

    result = TConnRequestAux (calling, called, expedited, data, cc, qos,
			      tc, td, async);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  TConnRequestAux (calling, called, expedited, data, cc, qos,
			     tc, td, async)
struct TSAPaddr *calling,
		*called;
char    *data;
int	expedited,
	cc,
	async;
struct QOStype *qos;
register struct TSAPconnect *tc;
register struct TSAPdisconnect *td;
{
    int	    result;
    register struct tsapblk *tb;

    if ((tb = newtblk ()) == NULL)
	return tsaplose (td, DR_CONGEST, NULLCP, "out of memory");

    if (calling == NULLTA) {
	static struct TSAPaddr tas;

	calling = &tas;
	bzero ((char *) calling, sizeof *calling);
    }
#ifdef	notdef
    if (called -> ta_selectlen > 0 && calling -> ta_selectlen == 0) {
	calling -> ta_port = htons ((u_short) (0x8000 | (getpid () & 0x7fff)));
	calling -> ta_selectlen = sizeof calling -> ta_port;
    }
#endif

    if (qos)
	tb -> tb_qos = *qos;		/* struct copy */

    if ((tb -> tb_calling = (struct TSAPaddr *)
			calloc (1, sizeof *tb -> tb_calling)) == NULL)
	goto no_mem;
    *tb -> tb_calling = *calling;	/* struct copy */
    bcopy (calling -> ta_selector, tb -> tb_initiating.ta_selector,
	   tb -> tb_initiating.ta_selectlen = calling -> ta_selectlen);

    if ((tb -> tb_called = (struct TSAPaddr *)
			calloc (1, sizeof *tb -> tb_called)) == NULL)
	goto no_mem;
    *tb -> tb_called = *called;		/* struct copy */

    if ((tb -> tb_cc = cc) > 0) {
	if ((tb -> tb_data = malloc ((unsigned) cc)) == NULLCP) {
no_mem: ;
	    (void) tsaplose (td, DR_CONGEST, NULLCP, "out of memory");
	    goto out;
	}
	bcopy (data, tb -> tb_data, cc);
    }
    tb -> tb_expedited = expedited;

    if ((result = TConnAttempt (tb, td, async)) == NOTOK) {
#ifdef  MGMT
	if (tb -> tb_manfnx)
	    (*tb -> tb_manfnx) (OPREQOUTBAD, tb);
#endif
	goto out;
    }

    if (async) {
	tc -> tc_sd = tb -> tb_fd;
	switch (result) {
	    case CONNECTING_1:
	    case CONNECTING_2:
		return result;
	}
    }

    if ((result = (*tb -> tb_retryPfnx) (tb, async, tc, td)) == DONE && !async)
	result = OK;
    return result;

out: ;
    freetblk (tb);

    return NOTOK;
}

/*  */

static int  TConnAttempt (tb, td, async)
struct tsapblk *tb;
struct TSAPdisconnect *td;
int	async;
{
    register int   n;
    int	    didone,
	    l,
	    result;
    register struct TSAPaddr *called, *calling;
    struct TSAPaddr *realcalled;
    register struct NSAPaddr *na, *la;
    struct NSAPaddr *realna;
    register struct TSAPdisconnect *te = td;
    struct TSAPdisconnect tds;

    calling = tb -> tb_calling;
    called = tb -> tb_called;

    didone = 0;
    for (na = called -> ta_addrs, n = called -> ta_naddr - 1;
	     n >= 0;
	     na++, n--) {
	register int   *ip;
	register char **ap;
        register struct nsapent *ns;

        realcalled = called;
        realna = na;
	for (ip = ts_communities; *ip; ip++)
	    if (*ip == na -> na_community)
		break;
	if (!*ip)
	    continue;
	for (ip = tsb_communities, ap = tsb_addresses; *ip; ip++, ap++)
	    if (*ip == na -> na_community) {
		if ((realcalled = maketsbaddr (*ap, na, called)) == NULLTA)
		    continue;
		realna = realcalled -> ta_addrs;
		break;
	    }

	for (la = calling -> ta_addrs, l = calling -> ta_naddr - 1;
	         l >= 0;
	         la++, l--)
	    if (la -> na_community == na -> na_community)
		break;
	if (l < 0)
	    la = NULLNA;

	for (ns = nsaps; ns -> ns_open; ns++)
	    if (ns -> ns_type == realna -> na_stack
		    && (ns -> ns_stack & ts_stacks))
		break;
	if (!ns -> ns_open)
	    continue;

	didone = 1;
	switch (ns -> ns_type) {
	    case NA_NSAP:
	        if ((result = (*ns -> ns_open) (tb, calling, la,
						realcalled, realna,
						te, async)) == NOTOK) {
		    te = &tds;
		    continue;
		}
		break;

	    default:
		if ((result = (*ns -> ns_open) (tb, la, realna, te, async))
		        == NOTOK) {
		    te = &tds;
		    continue;
		}
		break;
	}
	break;
    }

    if (tb -> tb_fd == NOTOK) {
	if (!didone)
	    return tsaplose (td, DR_PARAMETER, NULLCP,
			     "no supported NSAP addresses in, nor known TSBridges for, called parameter");

	return NOTOK;
    }

    if (la) {
	tb -> tb_initiating.ta_present = 1;
	tb -> tb_initiating.ta_addr = *la;	/* struct copy */
    }
    if (la && la != calling -> ta_addrs) {
	struct NSAPaddr ns;

	ns = calling -> ta_addrs[0];	/* struct copy */
	calling -> ta_addrs[0] = *la;	/*   .. */
	*la = ns;			/*   .. */
	la = calling -> ta_addrs;	/*   .. */
    }

    bcopy (realcalled -> ta_selector, tb -> tb_responding.ta_selector,
	   tb -> tb_responding.ta_selectlen = realcalled -> ta_selectlen);
    tb -> tb_responding.ta_present = 1;
    tb -> tb_responding.ta_addr = *realna;	/* struct copy */

    if ((result = (*tb -> tb_connPfnx) (tb, tb -> tb_expedited, tb -> tb_data,
			      tb -> tb_cc, td)) == NOTOK)
	return NOTOK;

    if (result == OK)
	result = CONNECTING_1;

    return result;
}

/*    T-ASYN-RETRY.REQUEST (pseudo) */

int	TAsynRetryRequest (sd, tc, td)
int	sd;
struct TSAPconnect *tc;
struct TSAPdisconnect *td;
{
    SBV     smask;
    int     result;
    register struct tsapblk *tb;
    struct TSAPaddr *ta;

    missingP (tc);
    missingP (td);

    smask = sigioblock ();

    if ((tb = findtblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return tsaplose (td, DR_PARAMETER, NULLCP,
		"invalid transport descriptor");
    }
    if (tb -> tb_flags & TB_CONN) {
	(void) sigiomask (smask);
	return tsaplose (td, DR_OPERATION, NULLCP,
		"transport descriptor connected");
    }

    ta = tb -> tb_called;

    switch (result = (*tb -> tb_retryPfnx) (tb, 1, tc, td)) {
        case NOTOK:	/* try next nsap in list */
	    if (ta -> ta_naddr <= 1) {
		freetblk (tb);
		break;
	    }
	    *tb -> tb_called = *newtaddr (ta, &ta -> ta_addrs[1],
					ta -> ta_naddr - 1); /* struct copy */

	    switch (result = TConnAttempt (tb, td, 1)) {
	        case DONE:
	            result = OK;
		    /* and fall... */
		case CONNECTING_1:
		case CONNECTING_2:
		    if (tb -> tb_fd != sd) {
			(void) dup2 (tb -> tb_fd, sd);
			(void) close (tb -> tb_fd);
			tb -> tb_fd = sd;
		    }
		    break;

		case NOTOK:
		    freetblk (tb);
		    break;
	    }
	    break;

	case DONE:
	    if (tb -> tb_data) {
		free (tb -> tb_data);
		tb -> tb_data = NULLCP;
	    }
	    tb -> tb_cc = 0;
	    tb -> tb_expedited = 0;
	    break;

	case CONNECTING_1:
	case CONNECTING_2:
	default:
	    break;
    }

    (void) sigiomask (smask);

    return result;
}

/*    T-ASYN-NEXT.REQUEST (pseudo) */

int	TAsynNextRequest (sd, tc, td)
int	sd;
struct TSAPconnect *tc;
struct TSAPdisconnect *td;
{
    SBV     smask;
    int     result;
    register struct tsapblk *tb;
    struct TSAPaddr *ta;

    missingP (tc);
    missingP (td);

    smask = sigioblock ();

    if ((tb = findtblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return tsaplose (td, DR_PARAMETER, NULLCP,
		"invalid transport descriptor");
    }
    if (tb -> tb_flags & TB_CONN) {
	(void) sigiomask (smask);
	return tsaplose (td, DR_OPERATION, NULLCP,
		"transport descriptor connected");
    }

    ta = tb -> tb_called;

    /* close previous connection attempt */
    if (tb -> tb_fd != NOTOK)
	(void) (*tb -> tb_closefnx) (tb -> tb_fd);
    tb -> tb_fd = NOTOK;

    if (ta -> ta_naddr <= 1) {
	freetblk (tb);
	(void) sigiomask (smask);
	return tsaplose (td, DR_PARAMETER, NULLCP, "no more NSAPs to try");
    }
    *tb -> tb_called = *newtaddr (ta, &ta -> ta_addrs[1],
				ta -> ta_naddr - 1); /* struct copy */

    switch (result = TConnAttempt (tb, td, 1)) {
        case DONE:
            result = OK;
	    /* and fall... */
	case CONNECTING_1:
	case CONNECTING_2:
	    if (tb -> tb_fd != sd) {
		(void) dup2 (tb -> tb_fd, sd);
		(void) close (tb -> tb_fd);
		tb -> tb_fd = sd;
	    }
	    break;

	case NOTOK:
	    freetblk (tb);
	    break;
    }

    (void) sigiomask (smask);

    return result;
}

/*  */

static struct TSAPaddr *newtaddr (ta, na, n)
register struct TSAPaddr *ta;
register struct NSAPaddr *na;
int	n;
{
    static struct TSAPaddr tzs;
    register struct TSAPaddr *tz = &tzs;
    register struct NSAPaddr *nz = tz -> ta_addrs;

    bzero ((char *) tz, sizeof *tz);

    if (tz -> ta_selectlen = ta -> ta_selectlen)
	bcopy (ta -> ta_selector, tz -> ta_selector, ta -> ta_selectlen);
    if (na)
	for (tz -> ta_naddr = n; n > 0; n--)
	    *nz++ = *na++;	/* struct copy */

    return tz;
}

/*  */

struct TSAPaddr *ta2norm (ta)
register struct TSAPaddr *ta;
{
    register int    n,
		   *ip;
    static struct TSAPaddr tzs;
    register struct TSAPaddr *tz = &tzs;
    register struct NSAPaddr *na,
			     *ca;

    SLOG (addr_log, LLOG_TRACE, NULLCP,
	  ("ta2norm %s", taddr2str (ta)));

    for (na = ta -> ta_addrs, n = ta -> ta_naddr - 1; n >= 0; na++, n--)
	if (na -> na_community == 0) {
	    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
		  ("ta2norm: empty subnet in NSAP address at offset %d",
		   na -  ta -> ta_addrs));
	    return NULLTA;
	}

    bzero ((char *) tz, sizeof *tz);
    bcopy (ta -> ta_selector, tz -> ta_selector,
	   tz -> ta_selectlen = ta -> ta_selectlen);
    ca = tz -> ta_addrs;

    for (ip = ts_communities; *ip; ip++)
	for (na = ta -> ta_addrs, n = ta -> ta_naddr - 1;
	         n >= 0;
	         na++, n--)
	    if (*ip == na -> na_community) {
		*ca++ = *na;		/* struct copy */
		tz -> ta_naddr++;
	    }

    for (na = ta -> ta_addrs, n = ta -> ta_naddr - 1; n >= 0; na++, n--) {
	for (ip = ts_communities; *ip; ip++)
	    if (*ip == na -> na_community)
		break;
	if (!*ip) {
	    *ca++ = *na;		/* struct copy */
	    tz -> ta_naddr++;
	}
    }

    SLOG (addr_log, LLOG_TRACE, NULLCP,
	  ("ta2norm returns %s", taddr2str (tz)));

    return tz;
}

/*  */

static struct TSAPaddr *maketsbaddr (cp, na, ta)
char *cp;
struct NSAPaddr *na;
struct TSAPaddr *ta;
{
    static struct TSAPaddr newta;
    register struct TSAPaddr *nta = &newta;
    struct TSAPaddr *taz;
    struct NSAPaddr *nna;

    if ((taz = str2taddr (cp)) == NULLTA)
	return taz;

    *nta = *taz;	/* struct copy */
    if ((nna = na2norm (na)) == NULLNA)
	return NULLTA;
    if ((nta -> ta_selectlen = 2 + nna -> na_addrlen + ta -> ta_selectlen)
	>= TSSIZE)
	return NULLTA;
    bcopy (nna -> na_address, &nta -> ta_selector[2], nna -> na_addrlen);
    bcopy (ta -> ta_selector, &nta -> ta_selector[2 + nna -> na_addrlen],
	   ta -> ta_selectlen);
    nta -> ta_selector[0] = nta -> ta_selector[1] = nna -> na_addrlen;
    return nta;
}
