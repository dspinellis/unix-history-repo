/* ps2udp.c - PPM: UDP backing */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap2-lpp/RCS/ps2udp.c,v 7.6 91/02/22 09:38:04 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap2-lpp/RCS/ps2udp.c,v 7.6 91/02/22 09:38:04 mrose Interim $
 *
 * Contributed by The Wollongong Group, Inc.
 *
 *
 * $Log:	ps2udp.c,v $
 * Revision 7.6  91/02/22  09:38:04  mrose
 * Interim 6.8
 * 
 * Revision 7.5  91/01/07  12:40:39  mrose
 * update
 * 
 * Revision 7.4  90/10/16  16:24:32  mrose
 * foo
 * 
 * Revision 7.3  90/07/09  14:45:02  mrose
 * sync
 * 
 * Revision 7.2  90/07/01  21:05:21  mrose
 * pepsy
 * 
 * Revision 7.1  89/12/19  16:17:48  mrose
 * dgram
 * 
 * Revision 7.0  89/11/23  22:15:51  mrose
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

#include <errno.h>
#include <stdio.h>
#define	LPP
#include "PS-types.h"
#include "ppkt.h"
#include "tsap.h"
#include "tailor.h"

#include "dgram.h"
#include "internet.h"


extern int  errno;

/*  */

#define	MAXTRIES	 3		/* should be tailorable... */
#define	WAITRIES	30		/*   .. */


int	udpopen (pb, calling, called, pi, async)
register struct psapblk *pb;
struct NSAPaddr *calling,
		*called;
struct PSAPindication *pi;
int	async;
{
    int	    fd;
    struct sockaddr_in  lo_socket,
			in_socket;
    register struct sockaddr_in *lsock = &lo_socket,
			       *isock = &in_socket;
    register struct hostent *hp;

    bzero ((char *) isock, sizeof *isock);

    if (called -> na_port == 0)
	return psaplose (pi, PC_ADDRESS, NULLCP,
			 "UDP port of called address not specified");
    else
	isock -> sin_port = called -> na_port;

    if ((hp = gethostbystring (called -> na_domain)) == NULL)
	return psaplose (pi, PC_ADDRESS, NULLCP, "%s: unknown host",
			 called -> na_domain);
#ifdef	notanymore
    (void) strncpy (called -> na_domain, hp -> h_name,
		    sizeof called -> na_domain);
#endif

    isock -> sin_family = hp -> h_addrtype;
    inaddr_copy (hp, isock);

#ifndef	notanymore
    (void) strcpy (called -> na_domain, inet_ntoa (isock -> sin_addr));
#endif

    bzero ((char *) lsock, sizeof *lsock);
    if (calling && calling -> na_domain[0]) {
	if ((hp = gethostbystring (calling -> na_domain)) == NULL)
	    return psaplose (pi, PC_ADDRESS, NULLCP, "%s: unknown host",
		    calling -> na_domain);

	if ((lsock -> sin_family = hp -> h_addrtype) != isock -> sin_family)
	    return psaplose (pi, PC_ADDRESS, NULLCP,
		    "address family mismatch");

	inaddr_copy (hp, lsock);
    }
    else
	lsock -> sin_family = isock -> sin_family;

    if ((fd = start_udp_client (lsock, 0, 0, 0)) == NOTOK)
	return psaplose (pi, PC_CONGEST, "socket", "unable to start");

    if (join_udp_server (fd, isock) == NOTOK) {
	(void) psaplose (pi, PC_REFUSED, "connection", "unable to establish");
	(void) close_udp_socket (fd);
	return NOTOK;
    }

    if ((pb -> pb_stream = ps_alloc (dg_open)) == NULLPS
	    || dg_setup (pb -> pb_stream, fd, MAXDGRAM, read_udp_socket,
			 write_udp_socket, check_udp_socket) == NOTOK) {
	(void) psaplose (pi, PC_CONGEST, NULLCP, NULLCP);
	(void) close_udp_socket (fd);
	return NOTOK;
    }

    PUservice (pb, fd);

    pb -> pb_tries = pb -> pb_maxtries;

    for (;;)
	switch (udpretry (pb, PC_REFUSED, pi)) {
	    case NOTOK:
		return NOTOK;

	    case OK:
		if (async)
		    return OK;
		continue;

	    case DONE:
	    default:
		return DONE;
	}
}

/*  */

/* ARGSUSED */

char   *udpsave (fd, cp1, cp2, td)
int	fd;
char   *cp1,
       *cp2;
struct TSAPdisconnect *td;
{
    static char	buffer[BUFSIZ];

    (void) sprintf (buffer, "%c%d %s %s", PT_UDP, fd, cp1, cp2);

    return buffer;
}


int	udprestore (pb, buffer, pi)
register struct psapblk *pb;
char   *buffer;
struct PSAPindication *pi;
{
    int	    fd;
    register char *cp;
    char    domain1[NSAP_DOMAINLEN + 1 + 5 + 1],
	    domain2[NSAP_DOMAINLEN + 1 + 5 + 1];
    register struct NSAPaddr *na;

    na = &pb -> pb_initiating;
    na -> na_stack = NA_TCP;
    na -> na_community = ts_comm_tcp_default;
    na -> na_tset = NA_TSET_UDP;

    if (sscanf (buffer, "%d %s %s", &fd, domain1, domain2) != 3 || fd < 0)
	return psaplose (pi, PC_PARAMETER, NULLCP,
			 "bad initialization vector");

    if (cp = index (domain1, '+')) {
	*cp++ = NULL;
	na -> na_port = htons ((u_short) atoi (cp));
    }
    (void) strncpy (na -> na_domain, domain1, sizeof na -> na_domain);

    PUservice (pb, fd);

    na = pb -> pb_responding.pa_addr.sa_addr.ta_addrs;
    na -> na_stack = NA_TCP;
    na -> na_community = ts_comm_tcp_default;
    na -> na_tset = NA_TSET_UDP;

    if (cp = index (domain2, '+')) {
	*cp++ = NULL;
	na -> na_port = htons ((u_short) atoi (cp));
    }
    (void) strncpy (na -> na_domain, domain2, sizeof na -> na_domain);

    if ((pb -> pb_stream = ps_alloc (dg_open)) == NULLPS
	    || dg_setup (pb -> pb_stream, pb -> pb_fd, MAXDGRAM,
			 read_udp_socket, write_udp_socket,
			 check_udp_socket) == NOTOK)
	return psaplose (pi, PC_CONGEST, NULLCP, NULLCP);

    return OK;
}

/*  */

static int  udpretry (pb, reason, pi)
register struct psapblk *pb;
int	reason;
struct PSAPindication *pi;
{
    int	    nfds;
    fd_set  ifds;
    PS	    ps;

    PLOGP (psap2_log,PS_PDUs, pb -> pb_retry,
	  reason == PC_REFUSED ? "ConnectRequest-PDU" : "ReleaseRequest-PDU",
	  0);

    if (pe2ps (ps = pb -> pb_stream, pb -> pb_retry) == NOTOK) {
	(void) pslose (pi, ps -> ps_errno);
	(void) close_udp_socket (pb -> pb_fd);
	return (pb -> pb_fd = NOTOK);
    }

    FD_ZERO (&ifds);

    nfds = pb -> pb_fd + 1;
    FD_SET (pb -> pb_fd, &ifds);
    if (select_udp_socket (nfds, &ifds, NULLFD, NULLFD, WAITRIES) < 1) {
	if (--pb -> pb_tries > 0)
	    return OK;

	if (reason == PC_REFUSED) {
	    errno = ETIMEDOUT;
	    (void) ppktlose (pb, pi, PC_REFUSED, pb -> pb_reference,
			     "connection", "unable to establish");
	}
	else
	    (void) ppktlose (pb, pi, reason, pb -> pb_reference, NULLCP,
			     NULLCP);

	(void) close_udp_socket (pb -> pb_fd);
	return (pb -> pb_fd = NOTOK);
    }

    if (pb -> pb_response)
	pe_free (pb -> pb_response);
    if ((pb -> pb_response = ps2pe (ps = pb -> pb_stream)) == NULLPE) {
	(void) pslose (pi, ps -> ps_errno);
	(void) close_udp_socket (pb -> pb_fd);
	return (pb -> pb_fd = NOTOK);
    }

    return DONE;
}

/*  */

static int  udpcheck (pb, pi)
register struct psapblk *pb;
struct PSAPindication *pi;
{
    int	    nfds;
    fd_set  ifds;

    FD_ZERO (&ifds);

    nfds = pb -> pb_fd + 1;
    FD_SET (pb -> pb_fd, &ifds);
    if (select_udp_socket (nfds, &ifds, NULLFD, NULLFD, OK) < 1)
	return psaplose (pi, PC_TIMER, NULLCP, NULLCP);

    return psaplose (pi, PC_WAITING, NULLCP, NULLCP);
}

/*  */

#define	udpclose	close_udp_socket
#define	udpselect	select_udp_socket


static int  PUservice (pb, fd)
register struct psapblk *pb;
int	fd;
{
    pb -> pb_fd = fd;

    pb -> pb_reliability = LOW_QUALITY;
    pb -> pb_maxtries = MAXTRIES;

    pb -> pb_retryfnx = udpretry;
    pb -> pb_closefnx = udpclose;
    pb -> pb_selectfnx = udpselect;
    pb -> pb_checkfnx = udpcheck;
}
