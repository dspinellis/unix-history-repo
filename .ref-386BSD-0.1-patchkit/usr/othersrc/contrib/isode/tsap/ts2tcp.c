/* ts2tcp.c - TPM: TCP interface */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/tsap/RCS/ts2tcp.c,v 7.8 91/02/22 09:47:23 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/tsap/RCS/ts2tcp.c,v 7.8 91/02/22 09:47:23 mrose Interim $
 *
 *
 * $Log:	ts2tcp.c,v $
 * Revision 7.8  91/02/22  09:47:23  mrose
 * Interim 6.8
 * 
 * Revision 7.7  90/12/11  10:51:46  mrose
 * lock-and-load
 * 
 * Revision 7.6  90/10/16  16:24:17  mrose
 * foo
 * 
 * Revision 7.5  90/07/09  14:51:21  mrose
 * sync
 * 
 * Revision 7.4  90/03/23  17:31:28  mrose
 * 8
 * 
 * Revision 7.3  90/02/19  13:07:26  mrose
 * update
 * 
 * Revision 7.2  89/12/08  09:41:39  mrose
 * touch-up
 * 
 * Revision 7.1  89/12/07  01:07:36  mrose
 * queued writes
 * 
 * Revision 7.0  89/11/23  22:30:40  mrose
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

#ifdef	TCP
#include "internet.h"
#include <errno.h>
#ifdef	BSD42
#include <sys/ioctl.h>
#endif
#ifdef	SYS5
#include <fcntl.h>
#endif


#define	MAX1006		2048		/* could be as high as TPKT_MAXLEN */

/*    DATA */

#if	defined(FIONBIO) || defined(O_NDELAY)
#define	NODELAY
#endif

#ifdef	NODELAY
static  fd_set  inprogress;
static struct sockaddr_in *peers = NULL;
#endif


extern int  errno;

/*    N-CONNECT.REQUEST */

int	tcpopen (tb, local, remote, td, async)
register struct tsapblk *tb;
struct NSAPaddr *local,
		*remote;
struct TSAPdisconnect *td;
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
    register struct servent *sp;

#ifndef	NODELAY
    if (async)
	return tsaplose (td, DR_PARAMETER, NULLCP,
			 "asynchronous not supported");
#endif

    bzero ((char *) isock, sizeof *isock);

    if (remote -> na_port == 0) {
	if ((sp = getservbyname ("tsap", "tcp")) == NULL)
	    sp = getservbyname ("iso-tsap", "tcp");
	isock -> sin_port = sp ? sp -> s_port : htons ((u_short) 102);
    }
    else
	isock -> sin_port = remote -> na_port;

    if ((hp = gethostbystring (remote -> na_domain)) == NULL)
	return tsaplose (td, DR_ADDRESS, NULLCP, "%s: unknown host",
		    remote -> na_domain);
#ifdef	notanymore
    (void) strncpy (remote -> na_domain, hp -> h_name,
		    sizeof remote -> na_domain);
#endif

    isock -> sin_family = hp -> h_addrtype;
    inaddr_copy (hp, isock);

#ifndef	notanymore
    (void) strcpy (remote -> na_domain, inet_ntoa (isock -> sin_addr));
#endif

    if (local && local -> na_domain[0]) {
	bzero ((char *) lsock, sizeof *lsock);

	if ((hp = gethostbystring (local -> na_domain)) == NULL)
	    return tsaplose (td, DR_ADDRESS, NULLCP, "%s: unknown host",
		    local -> na_domain);

	if ((lsock -> sin_family = hp -> h_addrtype) != isock -> sin_family)
	    return tsaplose (td, DR_ADDRESS, NULLCP,
		    "address family mismatch");

	inaddr_copy (hp, lsock);
    }
    else
	lsock = NULL;

    if ((fd = start_tcp_client (lsock, 0)) == NOTOK)
	return tsaplose (td, DR_CONGEST, "socket", "unable to start");

#ifdef	FIONBIO
    if (async)
	(void) ioctl (fd, FIONBIO, (onoff = 1, (char *) &onoff));
#else
#ifdef	O_NDELAY
    if (async)
	(void) fcntl (fd, F_SETFL, O_NDELAY);
#endif
#endif
    tb -> tb_fd = fd;
    (void) TTService (tb);

    if (join_tcp_server (fd, isock) == NOTOK) {
#ifdef	NODELAY
	if (async)
	    switch (errno) {
		case EINPROGRESS: 
		    if (peers == NULL) {
			peers = (struct sockaddr_in *)
					calloc ((unsigned) getdtablesize (),
						sizeof *peers);
			if (peers == NULL) {
			    (void) tsaplose (td, DR_CONGEST, NULLCP,
					     "out of memory");
			    (void) close_tcp_socket (fd);
			    return (tb -> tb_fd = NOTOK);
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

	(void) tsaplose (td, DR_REFUSED, "connection", "unable to establish");
	(void) close_tcp_socket (fd);
	return (tb -> tb_fd = NOTOK);
    }
#ifdef	NODELAY
done: ;
#endif

#ifdef	FIONBIO
    if (async)
	(void) ioctl (fd, FIONBIO, (onoff = 0, (char *) &onoff));
#else
#ifdef	O_NDELAY
    if (async)
	(void) fcntl (fd, F_SETFL, 0x00);
#endif
#endif

    return DONE;
}

/*  */

#ifndef	NODELAY
/* ARGSUSED */
#endif

static int  tcpretry (tb, td)
struct tsapblk *tb;
struct TSAPdisconnect *td;
{
#ifdef	NODELAY
#ifdef	FIONBIO
    int	    onoff;
#endif
    int	    fd = tb -> tb_fd;
    fd_set  mask;
    struct sockaddr_in *isock = &peers[fd];

    FD_ZERO (&mask);
    FD_SET (fd, &mask);
    if (xselect (fd + 1, NULLFD, &mask, NULLFD, 0) < 1)
	return OK;

    if (!FD_ISSET (fd, &inprogress))
	return DONE;

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

	(void) tsaplose (td, DR_REFUSED, "connection", "unable to establish");
	FD_CLR (fd, &inprogress);
	(void) close_tcp_socket (fd);
	return (tb -> tb_fd = NOTOK);
    }
done: ;

#ifdef	FIONBIO
    (void) ioctl (fd, FIONBIO, (onoff = 0, (char *) &onoff));
#else
#ifdef	O_NDELAY
    (void) fcntl (fd, F_SETFL, 0x00);
#endif
#endif

    FD_CLR (fd, &inprogress);

    return DONE;
#else
    return tsaplose (td, DR_OPERATION, NULLCP, "connection not in progress");
#endif
}

/*    init for read from network */

#ifndef	BRIDGE_X25
static
#endif
int	tcpinit (fd, t)
int	fd;
register struct tsapkt *t;
{
    register int    cc,
                    i;
    register char  *bp;

    for (bp = (char *) &t -> t_pkthdr, i = TPKT_HDRLEN (t);
	    i > 0;
	    bp += cc, i -= cc)
	switch (cc = read_tcp_socket (fd, bp, i)) {
	    case NOTOK: 
	    case OK: 
		return DR_NETWORK;

	    default: 
		break;
	}

    if (t -> t_vrsn != TPKT_VRSN)
	return DR_PROTOCOL;

    if ((t -> t_length = ntohs (t -> t_length)) < TPKT_HDRLEN (t))
	return DR_LENGTH;

    return OK;
}

/*  */

/* ARGSUSED */

char   *tcpsave (fd, cp1, cp2, td)
int	fd;
char   *cp1,
       *cp2;
struct TSAPdisconnect *td;
{
    static char buffer[BUFSIZ];

    (void) sprintf (buffer, "%c%d %s %s", NT_TCP, fd, cp1, cp2);

    return buffer;
}

/*  */

int	tcprestore (tb, buffer, td)
register struct tsapblk *tb;
char   *buffer;
struct TSAPdisconnect *td;
{
    int     fd;
    register char *cp;
    char    domain1[NSAP_DOMAINLEN + 1 + 5 + 1],
	    domain2[NSAP_DOMAINLEN + 1 + 5 + 1];
    register struct NSAPaddr *na;
    register struct tsapADDR *ta;
    
    ta = &tb -> tb_initiating;
    ta -> ta_present = 1;
    na = &ta -> ta_addr;
    na -> na_stack = NA_TCP;
    na -> na_community = ts_comm_tcp_default;

    if (sscanf (buffer, "%d %s %s", &fd, domain1, domain2) != 3 || fd < 0)
	return tsaplose (td, DR_PARAMETER, NULLCP,
			 "bad initialization vector \"%s\"", buffer);

    if (cp = index (domain1, '+')) {
	*cp++ = NULL;
	na -> na_port = htons ((u_short) atoi (cp));
    }
    (void) strncpy (na -> na_domain, domain1, sizeof na -> na_domain);

    tb -> tb_fd = fd;
    (void) TTService (tb);
    
    ta = &tb -> tb_responding;
    ta -> ta_present = 1;
    na = &ta -> ta_addr;
    na -> na_stack = NA_TCP;
    na -> na_community = ts_comm_tcp_default;
    
    if (cp = index (domain2, '+')) {
	*cp++ = NULL;
	na -> na_port = htons ((u_short) atoi (cp));
    }
    (void) strncpy (na -> na_domain, domain2, sizeof na -> na_domain);

    return OK;
}

/*  */

int	TTService (tb)
register struct tsapblk *tb;
{
    struct tsapkt *t;

    tb -> tb_flags |= TB_TCP;

    tb -> tb_tsdusize = MAX1006
		    - (tb -> tb_tpduslop = sizeof t -> t_pkthdr + DT_MAGIC);

    tb -> tb_retryfnx = tcpretry;

    tb -> tb_initfnx = tcpinit;
    tb -> tb_readfnx = read_tcp_socket;
    tb -> tb_writefnx = tp0write;
    tb -> tb_closefnx = close_tcp_socket;
    tb -> tb_selectfnx = select_tcp_socket;

    tp0init (tb);
}
#endif
