/* str2tpkt.c - read/write a TPDU thru a string */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/tsap/RCS/str2tpkt.c,v 7.2 91/02/22 09:47:11 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/tsap/RCS/str2tpkt.c,v 7.2 91/02/22 09:47:11 mrose Interim $
 *
 *
 * $Log:	str2tpkt.c,v $
 * Revision 7.2  91/02/22  09:47:11  mrose
 * Interim 6.8
 * 
 * Revision 7.1  89/12/07  01:07:28  mrose
 * queued writes
 * 
 * Revision 7.0  89/11/23  22:30:30  mrose
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


int	readfnx (), getfnx (), writefnx (), putfnx ();

/*  */

char   *tpkt2str (t)
struct tsapkt *t;
{
    int	    cc;
    char    packet[BUFSIZ];
    static char buffer[2 * sizeof packet + 1];

    (void) writefnx ((struct tsapblk *) NOTOK, packet, 0);
    if (tpkt2fd ((struct tsapblk *) 0, t, putfnx) == NOTOK)
	return NULLCP;

    cc = writefnx ((struct tsapblk *) NOTOK, NULLCP, 0);
    if (t -> t_qbuf) {
	bcopy (t -> t_qbuf -> qb_data, packet + cc, t -> t_qbuf -> qb_len);
	cc += t -> t_qbuf -> qb_len;
    }
    buffer[explode (buffer, (u_char *) packet, cc)] = NULL;

    DLOG (tsap_log, LLOG_PDUS,
	  ("write %d bytes, \"%s\"", strlen (buffer), buffer));

    return buffer;
}

/*  */

struct tsapkt *str2tpkt (buffer)
char  *buffer;
{
    char    packet[BUFSIZ];
    register struct tsapkt *t;

    DLOG (tsap_log, LLOG_PDUS,
	  ("read %d bytes, \"%s\"", strlen (buffer), buffer));

    (void) getfnx (NOTOK, NULLPKT, packet,
		implode ((u_char *) packet, buffer, strlen (buffer)));
    t = fd2tpkt (0, getfnx, readfnx);

    return t;
}

/*  */

static int  getfnx (fd, t, buffer, n)
int	fd;
register struct tsapkt *t;
char   *buffer;
int	n;
{
    static int  cc;

    if (fd == NOTOK) {
	(void) readfnx (NOTOK, buffer, cc = n);
	return OK;
    }

    t -> t_length = cc + sizeof t -> t_pkthdr;
    t -> t_vrsn = TPKT_VRSN;

    if (readfnx (fd, (char *) &t -> t_li, sizeof t -> t_li)
	    != sizeof t -> t_li)
        return DR_LENGTH;
	
    if (readfnx (fd, (char *) &t -> t_code, sizeof t -> t_code)
	    != sizeof t -> t_code)
        return DR_LENGTH;
	
    return OK;
}


static int  readfnx (fd, buffer, n)
int	fd,
	n;
char   *buffer;
{
    register int    i;
    static int  cc;
    static char *bp;

    if (fd == NOTOK) {
	bp = buffer, cc = n;

	return OK;
    }

    if ((i = min (cc, n)) > 0) {
	bcopy (bp, buffer, n);
	bp += i, cc -= i;
    }

    return i;
}

/*  */

static int  putfnx (tb, t, cp, n)
struct tsapblk *tb;
register struct tsapkt *t;
char   *cp;
int	n;
{
    register int    cc;
    register struct udvec  *uv;

    cc = sizeof t -> t_li;
    if (writefnx (tb, (char *) &t -> t_li, cc) != cc)
	return NOTOK;

    if (writefnx (tb, (char *) &t -> t_code, sizeof t -> t_code)
	    != sizeof t -> t_code)
	return NOTOK;
    cc += sizeof t -> t_code;

    if (writefnx (tb, cp, n) != n)
	return NOTOK;
    cc += n;

    if (t -> t_vdata
	    && writefnx (tb, t -> t_vdata, t -> t_vlen) != t -> t_vlen)
	return NOTOK;
    cc += t -> t_vlen;

    for (uv = t -> t_udvec; uv -> uv_base; uv++) {
	if (writefnx (tb, uv -> uv_base, uv -> uv_len) != uv -> uv_len)
	    return NOTOK;
	cc += uv -> uv_len;
    }

    return cc;
}

/*  */

static int  writefnx (tb, buffer, n)
struct tsapblk *tb;
int	n;
char   *buffer;
{
    static int  cc;
    static char *bp;

    if (tb) {
	if (buffer == NULLCP)
	    return cc;
	bp = buffer, cc = 0;

	return OK;
    }

    bcopy (buffer, bp, n);
    bp += n, cc += n;

    return n;
}
