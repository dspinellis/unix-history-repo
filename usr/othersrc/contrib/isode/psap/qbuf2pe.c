/* qbuf2pe.c - read a PE from a SSDU */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/qbuf2pe.c,v 7.1 91/02/22 09:36:47 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/qbuf2pe.c,v 7.1 91/02/22 09:36:47 mrose Interim $
 *
 *
 * $Log:	qbuf2pe.c,v $
 * Revision 7.1  91/02/22  09:36:47  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:32  mrose
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
#undef	qbuf2pe
#include "tailor.h"

/*  */

#ifndef	DEBUG
/* ARGSUSED */
#endif

PE	qbuf2pe (qb, len, result)
register struct qbuf *qb;
int	len;
int    *result;
{
#ifdef	notdef
    register struct qbuf *qp;
#endif
    register PE	    pe;
    register PS	    ps;

#ifdef	notdef		/* "inline" nonsense too difficult to handle */
    if ((qp = qb -> qb_forw) != qb && qp -> qb_forw == qb) {
	remque (qp);

	return ssdu2pe (qp -> qb_data, qp -> qb_len, (char *) qp, result);
    }
#endif

    if ((ps = ps_alloc (qbuf_open)) == NULLPS) {
	*result = PS_ERR_NMEM;
	return NULLPE;
    }
    if (qbuf_setup (ps, qb) == NOTOK || (pe = ps2pe (ps)) == NULLPE) {
	if (ps -> ps_errno == PS_ERR_NONE)
	    ps -> ps_errno = PS_ERR_EOF;
	*result = ps -> ps_errno;
	ps_free (ps);
	return NULLPE;
    }

    *result = PS_ERR_NONE;
    ps_free (ps);

#ifdef	DEBUG
    if (psap_log -> ll_events & LLOG_PDUS)
	pe2text (psap_log, pe, 1, len);
#endif

    return pe;
}
