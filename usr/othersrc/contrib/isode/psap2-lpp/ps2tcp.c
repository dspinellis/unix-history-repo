/* psaptcp.c - PPM: TCP backing */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap2-lpp/RCS/ps2tcp.c,v 7.5 91/02/22 09:38:02 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap2-lpp/RCS/ps2tcp.c,v 7.5 91/02/22 09:38:02 mrose Interim $
 *
 * Contributed by The Wollongong Group, Inc.
 *
 *
 * $Log:	ps2tcp.c,v $
 * Revision 7.5  91/02/22  09:38:02  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/12/11  10:53:05  mrose
 * lock-and-load
 * 
 * Revision 7.3  90/10/16  16:24:30  mrose
 * foo
 * 
 * Revision 7.2  90/07/09  14:45:01  mrose
 * sync
 * 
 * Revision 7.1  90/07/01  21:05:19  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  22:15:50  mrose
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
#define	LPP
#include "PS-types.h"
#include "ppkt.h"
#include "tsap.h"
#include "tailor.h"

#include "internet.h"
#include <errno.h>
#ifdef	BSD42
#include <sys/ioctl.h>
#endif

/*    DATA */

#ifdef	FIONBIO
static  fd_set  inprogress;
static struct sockaddr_in *peers = NULL;
#endif

extern int  errno;

/*  */

int	tcpopen (pb, calling, called, pi, async)
register struct psapblk *pb;
struct NSAPaddr *calling,
		*called;
struct PSAPindication *pi;
int	async;
{
    int     fd;
#ifdef	FIONBIO
    int	    onoff;
#endif
    struct sockaddr_in  lo_socket,
                        in_socket;
    register struct sockaddr_in *lsock = &lo_socket,
                               *isock = &in_socket;
    register struct hostent *hp;

#ifndef	FIONBIO
    if (async)
	return psaplose (pi, PC_PARAMETER, NULLCP,
			 "asynchronous not supported");
#endif

    bzero ((char *) isock, sizeof *isock);

    if (called -> na_port == 0)
	return psaplose (pi, PC_ADDRESS, NULLCP,
			 "TCP port of called address not specified");
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

    if (calling && calling -> na_domain[0]) {
	bzero ((char *) lsock, sizeof *lsock);

	if ((hp = gethostbystring (calling -> na_domain)) == NULL)
	    return psaplose (pi, PC_ADDRESS, NULLCP, "%s: unknown host",
		    calling -> na_domain);

	if ((lsock -> sin_family = hp -> h_addrtype) != isock -> sin_family)
	    return psaplose (pi, PC_ADDRESS, NULLCP,
		    "address family mismatch");

	inaddr_copy (hp, lsock);
    }
    else
	lsock = NULL;

    if ((fd = start_tcp_client (lsock, 0)) == NOTOK)
	return psaplose (pi, PC_CONGEST, "socket", "unable to start");

#ifdef	FIONBIO
    if (async)
	(void) ioctl (fd, FIONBIO, (onoff = 1, (char *) &onoff));
#endif
    PTservice (pb, fd);

    if (join_tcp_server (fd, isock) == NOTOK) {
#ifdef	FIONBIO
	if (async)
	    switch (errno) {
		case EINPROGRESS: 
		    if (peers == NULL) {
			peers = (struct sockaddr_in *)
					calloc ((unsigned) getdtablesize (),
						sizeof *peers);
			if (peers == NULL) {
			    (void) psaplose (pi, PC_CONGEST, NULLCP,
					     "out of memory");
			    (void) close_tcp_socket (fd);
			    return (pb -> pb_fd = NOTOK);
			}

			FD_ZERO (&inprogress);
		    }
		    FD_SET (fd, &inprogress);
		    peers[fd] = *isock;/* struct copy */
		    return OK;

		case EISCONN: 
		    goto done;

		default: 
		    break;
	    }
#endif

	(void) psaplose (pi, PC_REFUSED, "connection", "unable to establish");
	(void) close_tcp_socket (fd);
	return (pb -> pb_fd = NOTOK);
    }
#ifdef	FIONBIO
done: ;

    if (async)
	(void) ioctl (fd, FIONBIO, (onoff = 0, (char *) &onoff));
#endif

    if (tcpready (pb, pi) == NOTOK) {
	(void) close_tcp_socket (fd);
	pb -> pb_fd = NOTOK;
    }

    return DONE;
}

/*  */

/* ARGSUSED */

char   *tcpsave (fd, cp1, cp2, td)
int	fd;
char   *cp1,
       *cp2;
struct TSAPdisconnect *td;
{
    static char	buffer[BUFSIZ];

    (void) sprintf (buffer, "%c%d %s %s", PT_TCP, fd, cp1, cp2);

    return buffer;
}


int	tcprestore (pb, buffer, pi)
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

    if (sscanf (buffer, "%d %s %s", &fd, domain1, domain2) != 3 || fd < 0)
	return psaplose (pi, PC_PARAMETER, NULLCP,
			 "bad initialization vector");

    if (cp = index (domain1, '+')) {
	*cp++ = NULL;
	na -> na_port = htons ((u_short) atoi (cp));
    }
    (void) strncpy (na -> na_domain, domain1, sizeof na -> na_domain);

    PTservice (pb, fd);

    na = pb -> pb_responding.pa_addr.sa_addr.ta_addrs;
    na -> na_stack = NA_TCP;
    na -> na_community = ts_comm_tcp_default;

    if (cp = index (domain2, '+')) {
	*cp++ = NULL;
	na -> na_port = htons ((u_short) atoi (cp));
    }
    (void) strncpy (na -> na_domain, domain2, sizeof na -> na_domain);

    if ((pb -> pb_stream = ps_alloc (fdx_open)) == NULLPS
	    || fdx_setup (pb -> pb_stream, pb -> pb_fd) == NOTOK)
	return psaplose (pi, PC_CONGEST, NULLCP, NULLCP);

    return OK;
}

/*  */

/* ARGSUSED */

static int  tcpretry (pb, reason, pi)
register struct psapblk *pb;
int	reason;
struct PSAPindication *pi;
{
#ifdef	FIONBIO
    int	    onoff;
    int	    fd = pb -> pb_fd;
    fd_set  mask;
    struct sockaddr_in *isock = &peers[fd];

    if (!FD_SET (fd, &inprogress))
	return psaplose (pi, PC_PARAMETER, NULLCP,
			 "connection not in progress");
    FD_ZERO (&mask);
    FD_SET (fd, &mask);

    if (xselect (fd + 1, NULLFD, &mask, NULLFD, 0) < 1)
	return OK;

    isock = &peers[fd];
    if (join_tcp_server (fd, isock) == NOTOK) {
	switch (errno) {
	    case EINPROGRESS:
		return OK;

	    case EISCONN:
		goto done;

	    case EINVAL:	/* UNIX bug: could be any socket errno, e.g.,
				   ETIMEDOUT */
		errno = ECONNREFUSED;
		/* and fall */
	    default:
		break;
	}

	(void) psaplose (pi, PC_REFUSED, "connection", "unable to establish");
	FD_CLR (fd, &inprogress);
	(void) close_tcp_socket (fd);
	return (pb -> pb_fd = NOTOK);
    }
done: ;

    (void) ioctl (fd, FIONBIO, (onoff = 0, (char *) &onoff));
    FD_CLR (fd, &inprogress);

    if (tcpready (pb, pi) == NOTOK) {
	(void) close_tcp_socket (fd);
	return (pb -> pb_fd = NOTOK);
    }

    return DONE;
#else
    return psaplose (pi, PC_OPERATION, NULLCP, "connection not in progress");
#endif
}

/*  */

static int  tcpready (pb, pi)
register struct psapblk *pb;
struct PSAPindication *pi;
{
    PS	    ps;

    if ((pb -> pb_stream = ps_alloc (fdx_open)) == NULLPS
	    || fdx_setup (pb -> pb_stream, pb -> pb_fd) == NOTOK)
	return psaplose (pi, PC_CONGEST, NULLCP, NULLCP);

    PLOGP (psap2_log,PS_PDUs, pb -> pb_retry, "ConnectRequest-PDU", 0);

    if (pe2ps (ps = pb -> pb_stream, pb -> pb_retry) == NOTOK)
	return pslose (pi, ps -> ps_errno);

    pe_free (pb -> pb_retry);
    pb -> pb_retry = NULLPE;

    if ((pb -> pb_response = ps2pe (pb -> pb_stream)) == NULLPE)
	return pslose (pi, ps -> ps_errno);

    return OK;
}

/*  */

#define	tcpclose	close_tcp_socket
#define	tcpselect	select_tcp_socket


static int  PTservice (pb, fd)
register struct psapblk *pb;
int	fd;
{
    pb -> pb_fd = fd;

    pb -> pb_reliability = HIGH_QUALITY;

    pb -> pb_retryfnx = tcpretry;
    pb -> pb_closefnx = tcpclose;
    pb -> pb_selectfnx = tcpselect;
}
