/* str2spkt.c - read/write a SPDU thru a string */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ssap/RCS/str2spkt.c,v 7.1 91/02/22 09:46:12 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ssap/RCS/str2spkt.c,v 7.1 91/02/22 09:46:12 mrose Interim $
 *
 *
 * $Log:	str2spkt.c,v $
 * Revision 7.1  91/02/22  09:46:12  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:25:53  mrose
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
#include "spkt.h"
#include "tailor.h"

/*  */

char   *spkt2str (s)
struct ssapkt *s;
{
    int     i,
	    len;
    char   *base,
	   *dp;
    static char buffer[(CONNECT_MAX + BUFSIZ) * 2 + 1];

    if (spkt2tsdu (s, &base, &len) == NOTOK)
	return NULLCP;
    if (s -> s_udata)
	switch (s -> s_code) {
	    case SPDU_DT:
		if (s -> s_mask & SMASK_SPDU_GT)
		    break;	/* else fall */
	    case SPDU_EX:
	    case SPDU_TD:
		if ((dp = realloc (base, (unsigned) (i = len + s -> s_ulen)))
			== NULL) {
		    free (base);
		    return NULLCP;
		}
		bcopy (s -> s_udata, (base = dp) + len, s -> s_ulen);
		len = i;
		break;

	    default:
		break;
	}

    buffer[explode (buffer, (u_char *) base, len)] = NULL;
    if (len > 0)
	free (base);

#ifdef	DEBUG
    if (ssap_log -> ll_events & LLOG_PDUS) {
	LLOG (ssap_log, LLOG_PDUS,
	      ("write %d bytes, \"%s\"", strlen (buffer), buffer));
	spkt2text (ssap_log, s, 0);
    }
#endif

    return buffer;
}

/*  */

struct ssapkt *str2spkt (buffer)
char  *buffer;
{
    int	    cc;
    char    packet[CONNECT_MAX + BUFSIZ];
    register struct ssapkt *s;
    struct qbuf qbs;
    register struct qbuf *qb = &qbs,
			 *qp;

    bzero ((char *) qb, sizeof *qb);
    qb -> qb_forw = qb -> qb_back = qb;

    cc = implode ((u_char *) packet, buffer, strlen (buffer));
    if ((qp = (struct qbuf *) malloc (sizeof *qp + (unsigned) cc)) == NULL)
	s = NULLSPKT;
    else {
	bcopy (packet, qp -> qb_data = qp -> qb_base, qp -> qb_len = cc);
	insque (qp, qb -> qb_back);
	s = tsdu2spkt (qb, cc, NULLIP);
	QBFREE (qb);
    }

#ifdef	DEBUG
    if (ssap_log -> ll_events & LLOG_PDUS) {
	LLOG (ssap_log, LLOG_PDUS,
	      ("read %d bytes, \"%s\"", strlen (buffer), buffer));
	spkt2text (ssap_log, s, 1);
    }
#endif

    return s;
}
