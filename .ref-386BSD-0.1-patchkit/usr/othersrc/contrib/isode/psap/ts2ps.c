/* ts2ps.c - TSDU-backed abstraction for PStreams
 		(really just a refinement of datagram-backed PStreams) */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/ts2ps.c,v 7.1 91/02/22 09:37:16 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/ts2ps.c,v 7.1 91/02/22 09:37:16 mrose Interim $
 *
 *
 * $Log:	ts2ps.c,v $
 * Revision 7.1  91/02/22  09:37:16  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:55  mrose
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
#include "psap.h"
#include "tsap.h"
#include "tailor.h"

/*  */

int	ts_read (fd, q)
int	fd;
struct qbuf **q;
{
    register struct qbuf *qb;
    struct TSAPdata  txs;
    register struct TSAPdata *tx = &txs;
    struct TSAPdisconnect  tds;
    register struct TSAPdisconnect *td = &tds;

    if (TReadRequest (fd, tx, NOTOK, td) == NOTOK) {
	if (td -> td_reason == DR_NORMAL) {
	    *q = NULL;
	    return OK;
	}

	SLOG (psap_log, LLOG_EXCEPTIONS, NULLCP,
	      (td -> td_cc > 0 ? "ts_read: [%s] %*.*s" : "ts_read: [%s]",
	       TErrString (td -> td_reason), td -> td_cc, td -> td_cc,
	        td -> td_data));

	return NOTOK;
    }

    qb = &tx -> tx_qbuf;
    if (qb -> qb_forw -> qb_forw != qb && qb_pullup (qb) == NOTOK) {
	SLOG (psap_log, LLOG_EXCEPTIONS, NULLCP,
	      ("ts_read: qb_pullup fails"));
	TXFREE (tx);

	return NOTOK;
    }

    remque (qb = tx -> tx_qbuf.qb_forw);
    qb -> qb_forw = qb -> qb_back = qb;

    *q = qb;

    TXFREE (tx);

    return qb -> qb_len;
}


int	ts_write (fd, qb)
int	fd;
register struct qbuf *qb;
{
    struct TSAPdisconnect  tds;
    register struct TSAPdisconnect *td = &tds;
    
    if (TDataRequest (fd, qb -> qb_data, qb -> qb_len, td) == NOTOK) {
	SLOG (psap_log, LLOG_EXCEPTIONS, NULLCP,
	      (td -> td_cc > 0 ? "ts_write: [%s] %*.*s" : "ts_write: [%s]",
	       TErrString (td -> td_reason), td -> td_cc, td -> td_cc,
	        td -> td_data));

	return NOTOK;
    }

    return qb -> qb_len;
}
