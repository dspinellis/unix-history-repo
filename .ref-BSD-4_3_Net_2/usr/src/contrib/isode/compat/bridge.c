/* bridge.c - X.25 abstractions for TCP bridge to X25 */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/bridge.c,v 7.2 91/02/22 09:14:58 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/bridge.c,v 7.2 91/02/22 09:14:58 mrose Interim $
 *
 * Contributed by Julian Onions, Nottingham University in the UK
 *
 *
 * $Log:	bridge.c,v $
 * Revision 7.2  91/02/22  09:14:58  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/09  14:31:32  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:22:55  mrose
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
#include "general.h"
#include "manifest.h"
#include "tailor.h"
#include "internet.h"
#include "tpkt.h"

/*     TCP/X.25 BRIDGE */

#ifdef	BRIDGE_X25


static	int	assfd[FD_SETSIZE];
static char	bridge_inited = 0;

/*  */

/* ARGSUSED */

int	start_bridge_client (local)
struct NSAPaddr	*local;
{
    int     sd;
    u_short port;
    register struct servent *sp;

    if ((sp = getservbyname ("x25bridge", "tcp")) == NULL)
	port = x25_bridge_port;
    else
	port = sp -> s_port;

    if ((sd = in_connect (x25_bridge_host, port)) == NOTOK)
	return NOTOK;

    if (write_tcp_socket (sd, "\01", 1) != 1) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("initial write"));

	(void) close_tcp_socket (sd);
	return NOTOK;
    }

    return sd;
}

/*  */

static int  in_connect (addr, port)
char	*addr;
u_short port;
{
    int     sd;
    struct sockaddr_in in_socket;
    register struct sockaddr_in *isock = &in_socket;
    register struct hostent *hp;

    if ((hp = gethostbystring (addr)) == NULL) {
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP, ("%s: unknown host", addr));

	return NOTOK;
    }

    bzero ((char *) isock, sizeof *isock);
    isock -> sin_family = hp -> h_addrtype;
    isock -> sin_port = port;
    inaddr_copy (hp, isock);

    if ((sd = start_tcp_client ((struct sockaddr_in *) NULL, 0)) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("start_tcp_client"));

	return NOTOK;
    }

    if (join_tcp_server (sd, isock) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("join_tcp_server"));

	(void) close_tcp_socket (sd);
	return NOTOK;
    }

    return sd;
}

/*  */

int	join_bridge_server (fd, remote)
int	fd;
register struct NSAPaddr *remote;
{
    if (remote != NULLNA)
        remote -> na_stack = NA_BRG, remote -> na_community = ts_comm_x25_default;
    if (bridge_write_nsap_addr (fd, remote, write_tcp_socket) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, NULLCP, ("write of NSAP failed"));

	return NOTOK;
    }

    return fd;
}

/*  */

int	start_bridge_server (local, backlog, opt1, opt2)
struct NSAPaddr *local;
int	backlog,
	opt1,
	opt2;
{
    int     len,
            new,
            sd;
    u_short port;
    struct servent *sp;
    struct sockaddr_in in_socket;
    register struct sockaddr_in *isock = &in_socket;

    if (bridge_inited == 0) {
	for (sd = 0; sd < FD_SETSIZE; sd++)
	    assfd[sd] = NOTOK;
	bridge_inited = 1;
    }
    if ((sp = getservbyname ("x25bridge", "tcp")) == NULL)
	port = x25_bridge_port;
    else
	port = sp -> s_port;

    if ((sd = in_connect (x25_bridge_host, port)) == NOTOK)
	return NOTOK;

    if (write_tcp_socket (sd, "\02", 1) != 1) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("initial write"));

	(void) close_tcp_socket (sd);
	return NOTOK;
    }

    if (local != NULLNA)
	local -> na_stack = NA_BRG, local -> na_community = ts_comm_x25_default;
    if (local != NULLNA && local -> na_dtelen == 0)
    {
	(void) strcpy (local -> na_dte, x25_bridge_addr);
	local -> na_dtelen = strlen(x25_bridge_addr);
    }
    if (local != NULLNA) {
	DLOG (compat_log, LLOG_DEBUG,
	      ("addr", "type=%d '%s' len=%d",
	       local -> na_stack, local -> na_dte,local -> na_dtelen));
	DLOG (compat_log, LLOG_DEBUG,
	      ("addr", "pid='%s'(%d) fac='%s'(%d) cudf='%s'(%d)",
	       local -> na_pid, local -> na_pidlen,
	       local -> na_fac, local -> na_faclen,
	       local -> na_pid, local -> na_pidlen,
	       local -> na_cudf, local -> na_cudflen));
    }

    if (bridge_write_nsap_addr (sd, local, write_tcp_socket) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, NULLCP, ("write of NSAP failed"));

	(void) close_tcp_socket (sd);
	return NOTOK;
    }

    if ((new = in_listen (backlog, opt1, opt2)) == NOTOK) {
	(void) close_tcp_socket (sd);
	return NOTOK;
    }

    len = sizeof *isock;
    if (getsockname (new, (struct sockaddr *) isock, &len) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("getsockname"));

out: ;
	(void) close_tcp_socket (sd);
	(void) close_tcp_socket (new);
	return NOTOK;
    }

    isock -> sin_family = htons (isock -> sin_family);
    if (write_tcp_socket (sd, (char *)isock, sizeof *isock) != sizeof *isock) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("write of sockaddr_in"));

	goto out;
    }
    assfd[new] = sd;

    return new;
}

int	get_bridge_assfd(fd)
int	fd;
{
    if (!bridge_inited)
	return NOTOK;
    return assfd[fd];
}

/*  */

static int  in_listen (backlog, opt1, opt2)
int	backlog,
	opt1,
	opt2;
{
    int     sd;
    char   *cp;
    struct sockaddr_in  lo_socket;
    register struct sockaddr_in *lsock = &lo_socket;
    register struct hostent *hp;

    if ((hp = gethostbystring (cp = getlocalhost ())) == NULL) {
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP, ("%s: unknown host", cp));

	return NOTOK;
    }

    bzero ((char *) lsock, sizeof *lsock);
    lsock -> sin_family = hp -> h_addrtype;
    inaddr_copy (hp, lsock);

    if ((sd = start_tcp_server (lsock, backlog, opt1, opt2)) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("start_tcp_server"));

	return NOTOK;
    }

    return sd;
}

/*  */

int	join_bridge_client (fd, remote)
int	fd;
struct NSAPaddr	*remote;
{
    int     new;
    struct sockaddr_in  in_socket;
    struct NSAPaddr	sock;

    if ((new = join_tcp_client (fd, &in_socket)) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("join_tcp_client"));

	return NOTOK;
    }

    if (bridge_read_nsap_addr (new, &sock, read_tcp_socket) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("read of NSAP"));

	(void) close_tcp_socket (new);
	return NOTOK;
    }
    DLOG (compat_log, LLOG_DEBUG,
	  ("addr", "type=%d '%s' len=%d", sock.na_stack, sock.na_dte,
	   sock.na_dtelen));
    DLOG (compat_log, LLOG_DEBUG,
	  ("addr", "pid='%s'(%d) fac='%s'(%d) cudf='%s'(%d)",
	   sock.na_pid, sock.na_pidlen,
	   sock.na_fac, sock.na_faclen,
	   sock.na_pid, sock.na_pidlen,
	   sock.na_cudf, sock.na_cudflen));
    sock.na_stack = ntohl (sock.na_stack);
    *remote = sock;
    DLOG (compat_log, LLOG_DEBUG,
	  ("addr", "type=%d '%s' len=%d",
	   remote -> na_stack, remote -> na_dte,remote -> na_dtelen));
    DLOG (compat_log, LLOG_DEBUG,
	  ("addr", "pid='%s'(%d) fac='%s'(%d) cudf='%s'(%d)",
	   remote -> na_pid, remote -> na_pidlen,
	   remote -> na_fac, remote -> na_faclen,
	   remote -> na_pid, remote -> na_pidlen,
	   remote -> na_cudf, remote -> na_cudflen));
    return new;
}

int	close_bridge_socket (sd)
int	sd;
{
	if (bridge_inited && assfd[sd] != NOTOK)
		(void) close_tcp_socket (assfd[sd]);
	assfd[sd] = NOTOK;
	return close_tcp_socket (sd);
}

/* ARGSUSED */

int	bridgediscrim (na)
struct NSAPaddr *na;
{
#ifndef	X25
    return 1;	/* must be bridge */
#else
    int len = strlen (x25_bridge_discrim);

    if (len == 1 && *x25_bridge_discrim == '-')
	return 0;

    return (len == 0 ? 1
		     : strncmp (na -> na_dte, x25_bridge_discrim, len) == 0);
#endif
}
#endif

/*
 * Structure is as follows :-
 * 0-1		type
 * 2-17 	dte
 * 18		dte len
 * 19-22	pid
 * 23		pid len
 * 24-39	user data
 * 40		user data len
 * 41-46	facilities
 * 47		facility length
 */
 
int bridge_write_nsap_addr (fd, nsap, writefnx)
int	fd;
struct NSAPaddr *nsap;
IFP	writefnx;
{
    u_short	na_stack;
    char	buffer[50];

    if (nsap == NULLNA || (na_stack = nsap -> na_stack) != NA_BRG)
	return NOTOK;
    na_stack = htons(na_stack);
    bcopy ((char *)&na_stack, buffer, sizeof(na_stack));
    bcopy (nsap -> na_dte, &buffer[2], 16);
    buffer[18] = nsap -> na_dtelen;
    bcopy (nsap -> na_pid, &buffer[19], 4);
    buffer[23] = nsap -> na_pidlen;
    bcopy (nsap -> na_cudf, &buffer[24], 16);
    buffer[40] = nsap -> na_cudflen;
    bcopy (nsap -> na_fac, &buffer[41], 6);
    buffer[47] = nsap -> na_faclen;
    if ((*writefnx) (fd, buffer, 48) != 48)
	return NOTOK;
    return OK;
}

int	bridge_read_nsap_addr (fd, nsap, readfnx)
int	fd;
struct	NSAPaddr *nsap;
IFP	readfnx;
{
    u_short	na_stack;
    char	buffer[50];

    if (readx (fd, buffer, 48, readfnx) != 48)
	return NOTOK;
    bcopy (buffer, (char *)&na_stack, sizeof(na_stack));
    na_stack = ntohs(na_stack);
    if (na_stack != NA_BRG)
	return NOTOK;
    nsap -> na_stack = na_stack;
    bcopy (&buffer[2], nsap -> na_dte, 16);
    nsap -> na_dtelen = buffer[18];
    bcopy (&buffer[19], nsap -> na_pid, 4);
    nsap -> na_pidlen = buffer[23];
    bcopy (&buffer[24], nsap -> na_cudf, 16);
    nsap -> na_cudflen = buffer[40];
    bcopy (&buffer[41], nsap -> na_fac, 6);
    nsap -> na_faclen = buffer[47];
    return OK;
}

static int  readx (fd, buffer, n, readfnx)
int     fd;
char    *buffer;
int     n;
IFP     readfnx;
{
    register int    i,
    cc;
    register char   *bp;

    for (bp = buffer, i = n; i > 0; bp += cc, i -= cc) {
	switch (cc = (*readfnx) (fd, bp, i)) {
	    case NOTOK:
	        return (i = bp - buffer) ? i : NOTOK;

	    case OK:
		break;

	    default:
		continue;
	}
	break;
    }

    return (bp - buffer);
}
