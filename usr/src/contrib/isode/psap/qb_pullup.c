/* qb_pullup.c - "pullup" a list of qbufs */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/qb_pullup.c,v 7.1 91/02/22 09:36:46 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/qb_pullup.c,v 7.1 91/02/22 09:36:46 mrose Interim $
 *
 *
 * $Log:	qb_pullup.c,v $
 * Revision 7.1  91/02/22  09:36:46  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:31  mrose
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

/*  */

int	qb_pullup (qb)
register struct qbuf *qb;
{
    register int    len;
    register char  *d;
    register struct qbuf  *p,
			  *qp,
    			  *qpp;

    len = 0;
#ifdef	notdef		/* want null-termination... */
    if ((p = qb -> qb_forw) -> qb_forw == qb)
	return OK;
#endif
    for (p = qb -> qb_forw; p != qb; p = p -> qb_forw)
	len += p -> qb_len;

    if ((p = (struct qbuf *) malloc ((unsigned) (sizeof *p + len + 1)))
	    == NULL)
	return NOTOK;
    d = p -> qb_data = p -> qb_base;
    p -> qb_len = len;

    for (qp = qb -> qb_forw; qp != qb; qp = qpp) {
	qpp = qp -> qb_forw;

	remque (qp);

	bcopy (qp -> qb_data, d, qp -> qb_len);
	d += qp -> qb_len;

	free ((char *) qp);
    }
    *d = NULL;

    insque (p, qb);

    return OK;
}
