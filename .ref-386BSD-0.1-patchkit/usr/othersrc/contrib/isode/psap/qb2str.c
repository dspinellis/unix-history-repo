/* qb2str.c - qbuf to string */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/qb2str.c,v 7.2 91/02/22 09:36:44 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/qb2str.c,v 7.2 91/02/22 09:36:44 mrose Interim $
 *
 *
 * $Log:	qb2str.c,v $
 * Revision 7.2  91/02/22  09:36:44  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/08/08  14:02:34  mrose
 * stuff
 * 
 * Revision 7.0  89/11/23  22:13:30  mrose
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

char   *qb2str (q)
register struct qbuf *q;
{
    register int    len;
    register char  *b,
                   *d;
    register struct qbuf   *p;

    p = q -> qb_forw, len = 0;
    do {
	len += p -> qb_len;

	p = p -> qb_forw;
    }
    while (p != q);
    q -> qb_len = len;

    if ((b = d = malloc ((unsigned) (len + 1))) == NULL)
	return NULLCP;

    p = q -> qb_forw;
    do {
	bcopy (p -> qb_data, d, p -> qb_len);
	d += p -> qb_len;

	p = p -> qb_forw;
    }
    while (p != q);
    *d = NULL;

    return b;
}
