/* tsaprespond.c - TPM: responder */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/tsap/RCS/tsaprespond.c,v 7.2 91/02/22 09:47:42 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/tsap/RCS/tsaprespond.c,v 7.2 91/02/22 09:47:42 mrose Interim $
 *
 *
 * $Log:	tsaprespond.c,v $
 * Revision 7.2  91/02/22  09:47:42  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/03/23  17:31:49  mrose
 * 8
 * 
 * Revision 7.0  89/11/23  22:30:55  mrose
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
#include "tpkt.h"
#include "tailor.h"

/*    T-CONNECT.INDICATION */

int     TInit (vecp, vec, ts, td)
register int     vecp;
register char  **vec;
register struct TSAPstart *ts;
register struct TSAPdisconnect *td;
{
    register struct tsapblk *tb;

    isodetailor (NULLCP, 0);

    if (vecp < 3)
	return tsaplose (td, DR_PARAMETER, NULLCP,
		    "bad initialization vector");
    missingP (vec);
    missingP (ts);
    missingP (td);

    if ((tb = newtblk ()) == NULL)
	return tsaplose (td, DR_CONGEST, NULLCP, "out of memory");

    vec += vecp - 2;
    switch (*vec[0]) {
	case NT_TCP:
#ifdef	TCP
	    if (tcprestore (tb, vec[0] + 1, td) == NOTOK)
		goto out;
	    break;
#else
	    goto not_supported;
#endif

	case NT_X25:
#ifdef	X25
	    if (x25restore (tb, vec[0] + 1, td) == NOTOK)
		goto out;
	    break;
#else
	    goto not_supported;
#endif

	case NT_BRG:
#ifdef	BRIDGE_X25
	    if (bridgerestore (tb, vec[0] + 1, td) == NOTOK)
		goto out;
	    break;
#else
	    goto not_supported;
#endif

	case NT_BSD:
#ifdef	BSD_TP4
	    if (tp4restore (tb, vec[0] + 1, td) == NOTOK)
		goto out;
	    break;
#else
	    goto not_supported;
#endif

	case NT_SUN:
#ifdef	SUN_TP4
	    if (tp4restore (tb, vec[0] + 1, td) == NOTOK)
		goto out;
	    break;
#else
	    goto not_supported;
#endif

	default:
	    (void) tsaplose (td, DR_PARAMETER, NULLCP,
			"unknown network type: 0x%x (%c)", *vec[0], *vec[0]);
	    goto out;
    }
    bzero (vec[0], strlen (vec[0]));

    if ((*tb -> tb_startPfnx) (tb, vec[1], ts, td) == NOTOK)
	goto out;
    bzero (vec[1], strlen (vec[1]));

    *vec = NULL;

    return OK;

not_supported: ;
    (void) tsaplose (td, DR_PARAMETER, NULLCP,
		"not configured for network type: 0x%x (%c)",
		*vec[0], *vec[0]);

out: ;
    freetblk (tb);

    return NOTOK;
}

/*    T-CONNECT.RESPONSE */

int     TConnResponse (sd, responding, expedited, data, cc, qos, td)
int	sd;
register struct TSAPaddr *responding;
int	expedited,
	cc;
char   *data;
struct QOStype *qos;
register struct TSAPdisconnect *td;
{
    int	    result;
    register struct tsapblk *tb;
    struct tsapADDR tas;

    if ((tb = findtblk (sd)) == NULL || (tb -> tb_flags & TB_CONN)) 
	return tsaplose (td, DR_PARAMETER, NULLCP, "invalid transport descriptor");
#ifdef	notdef
    missingP (responding);
#endif
    if (responding) {
	copyTSAPaddrY (responding, &tas);
	if (bcmp ((char *) &tb -> tb_responding, (char *) &tas, sizeof tas))
	    tb -> tb_responding = tas;	/* struct copy */
	else
	    responding = NULLTA;
    }
    if (expedited && !(tb -> tb_flags & TB_EXPD))
	return tsaplose (td, DR_PARAMETER, NULLCP,
		"expedited service not available");
    toomuchP (data, cc, TC_SIZE, "initial");
#ifdef	notdef
    missingP (qos);
#endif
    missingP (td);

    if (!expedited)
	tb -> tb_flags &= ~TB_EXPD;

    if ((result = (*tb -> tb_acceptPfnx) (tb, responding ? 1 : 0, data, cc,
			    qos, td)) == NOTOK)
	freetblk (tb);
#ifdef	X25
    else
	if (tb -> tb_flags & TB_X25)
	    LLOG (x25_log, LLOG_NOTICE,
		  ("connection %d from %s",
		   sd, na2str (&tb -> tb_initiating.ta_addr)));
#endif

    return result;
}
