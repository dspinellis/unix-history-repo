/* pe2uvec.c - write a PE to a udvec */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/pe2uvec.c,v 7.1 91/02/22 09:36:08 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/pe2uvec.c,v 7.1 91/02/22 09:36:08 mrose Interim $
 *
 *
 * $Log:	pe2uvec.c,v $
 * Revision 7.1  91/02/22  09:36:08  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:12:59  mrose
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
#include "tailor.h"

/*  */

int	pe2uvec (pe, uv)
register PE pe;
struct udvec **uv;
{
    int	    cc;
#ifdef	DEBUG
    int	    len;
#endif
    register PS	ps;

    *uv = NULL;

    if ((ps = ps_alloc (uvec_open)) == NULLPS)
	return NOTOK;
    cc = ps_get_abs (pe) - ps_get_plen (pe);
#ifdef	DEBUG
    len = ps -> ps_byteno;
#endif
    if (uvec_setup (ps, cc) == NOTOK || pe2ps_aux (ps, pe, 0) == NOTOK) {
	ps_free (ps);
	return NOTOK;
    }

    *uv = ps -> ps_head;
#ifdef	DEBUG
    len = ps -> ps_byteno - len;
#endif

    ps -> ps_head = NULL;
    ps -> ps_extra = NULL;
    ps_free (ps);

#ifdef	DEBUG
    if (psap_log -> ll_events & LLOG_PDUS) {
	register int	i,
			j,
			k;
	register struct udvec *vv;

	i = j = k = 0;
	for (vv = *uv; vv -> uv_base; vv++, i++)
	    if (vv -> uv_inline)
		j++, k += vv -> uv_len;

	LLOG (psap_log, LLOG_PDUS,
	      ("PE written in %d elements, %d inline (%d octet%s)",
	       i, j, k, k != 1 ? "s" : ""));
	pe2text (psap_log, pe, 0, len);
    }
#endif

    return OK;
}
