/* ubcx25.c - X.25 abstractions for UBC X25 */

#ifndef lint
static char *rcsid = "$Header: /f/osi/compat/RCS/ubcx25.c,v 7.4 91/02/22 09:16:13 mrose Interim $";
#endif

/*
 * $Header: /f/osi/compat/RCS/ubcx25.c,v 7.4 91/02/22 09:16:13 mrose Interim $
 *
 * Contributed by Julian Onions, Nottingham University in the UK
 *
 *
 * $Log:	ubcx25.c,v $
 * Revision 7.4  91/02/22  09:16:13  mrose
 * Interim 6.8
 * 
 * Revision 7.3  91/01/14  13:33:53  mrose
 * loader
 * 
 * Revision 7.2  90/07/09  14:32:28  mrose
 * sync
 * 
 * Revision 7.1  89/12/07  01:08:02  mrose
 * queued writes
 * 
 * Revision 7.0  89/11/23  21:23:47  mrose
 * Release 6.0
 * 
 */

/*
 *                                NOTICE
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
#include "tpkt.h"

/*    4.[23] UNIX: UBC X25 */

#ifdef  X25
#ifdef  UBC_X25

#include "x25.h"
#include <sys/uio.h>

#define         X25_MBIT        0x40
#define         X25_QBIT        0x80

/*  */

int     start_x25_client (local)
struct NSAPaddr *local;
{
    int     sd, pgrp;

    if (local != NULLNA)
	local -> na_stack = NA_X25, local -> na_community = ts_comm_x25_default;
    if ((sd = socket (AF_CCITT, SOCK_STREAM, 0)) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("socket"));
	return NOTOK; /* Error can be found in errno */
    }

    pgrp = getpid();
    if (ioctl(sd, SIOCSPGRP, &pgrp)) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("SIOCSPGRP"));
	return NOTOK; /* Error can be found in errno */
    }

    return sd;
}

/*  */

int     start_x25_server (local, backlog, opt1, opt2)
struct NSAPaddr *local;
int     backlog,
	opt1,
	opt2;
{
    int     sd, pgrp;
#ifdef	notyet
#ifdef	BSD43
    int	    onoff;
#endif
#endif
    CONN_DB     zsck;
    CONN_DB     *sck = &zsck;

    if ((sd = socket (AF_CCITT, SOCK_STREAM, 0)) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("socket"));
	return NOTOK; /* Can't get an X.25 socket */
    }

    pgrp = getpid();
    if (ioctl(sd, SIOCSPGRP, &pgrp)) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("SIOCSPGRP"));
	return NOTOK; /* Error can be found in errno */
    }

    if (local != NULLNA) {
	local -> na_stack = NA_X25, local -> na_community = ts_comm_x25_default;
	if (local -> na_dtelen == 0) {
	    (void) strcpy (local -> na_dte, x25_local_dte);
	    local -> na_dtelen = strlen(x25_local_dte);
	    if (local -> na_pidlen == 0 && *x25_local_pid)
		local -> na_pidlen =
		    str2sel (x25_local_pid, -1, local -> na_pid, NPSIZE);
	}
    }

    (void) gen2if (local, sck, ADDR_LISTEN);
    if (bind (sd, sck, sizeof(CONN_DB)) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("bind"));
	(void) close_x25_socket (sd);
	return NOTOK;
    }


#ifdef	notyet		/* not sure if these are supported... */
#ifndef	BSD43
    if (opt1)
	(void) setsockopt (sd, SOL_SOCKET, opt1, NULLCP, 0);
    if (opt2)
	(void) setsockopt (sd, SOL_SOCKET, opt2, NULLCP, 0);
#else
    onoff = 1;
    if (opt1)
	(void) setsockopt (sd, SOL_SOCKET, opt1, (char *)&onoff, sizeof onoff);
    if (opt2)
	(void) setsockopt (sd, SOL_SOCKET, opt2, (char *)&onoff, sizeof onoff);
#endif
#endif

    (void) listen (sd, backlog);

    return sd;
}

/*  */

int     join_x25_client (fd, remote)
int     fd;
struct NSAPaddr *remote;
{
    CONN_DB     sck;
    int     len = sizeof sck;
    int         nfd;

    if((nfd = accept (fd, (struct sockaddr *) &sck, &len)) == NOTOK)
	return NOTOK;
    (void) if2gen (remote, &sck, ADDR_REMOTE);
    return nfd;
}

int     join_x25_server (fd, remote)
int     fd;
struct NSAPaddr *remote;
{
    CONN_DB zsck;
    CONN_DB *sck = &zsck;

    if (remote == NULLNA || remote -> na_stack != NA_X25)
    {
	SLOG (compat_log, LLOG_EXCEPTIONS, NULLCP,
	      ("Invalid type na%d", remote->na_stack));
	return NOTOK;
    }
    (void) gen2if (remote, sck, ADDR_REMOTE);
    return connect (fd, sck, sizeof (CONN_DB));
}

int     read_x25_socket (fd, buffer, len)
int     fd, len;
char    *buffer;
{
    static u_char mode;
    static struct iovec iov[2] = {
	(char *)&mode, 1,
	"", 0
    };
    char        *p = buffer;
    int         cc, count = 0, total = len;

    do {
	iov[1].iov_base = p;
	iov[1].iov_len = total > X25_PACKETSIZE ? X25_PACKETSIZE : total;

	switch (cc = readv (fd, iov, 2)) {
		/*
		 * for the -1,0 & 1 cases these returns should be ok
		 * if it's the first time through, then they are valid anyway
		 * later stages means the M bit is set so there must
		 * be more data else someone is violating the
		 * protocol badly.
		 */

	    case NOTOK:
	    case 0:
		return cc;

	    case 1:
		SLOG (compat_log, LLOG_EXCEPTIONS, NULLCP,
		      ("strange return from read_x25_socket"));
		return NOTOK;

	    default:
		cc --;          /* discount the info byte */
		count += cc;
		p += cc;
		total -= cc;
	}
    } while (len > 0 && (mode & X25_MBIT));
    DLOG (compat_log, LLOG_DEBUG, ("X25 read, total %d/%d", count, len));

    return count;
}

#ifdef UBC_X25_WRITEV
/* God this all very bizarre - iovecs work on read but not write!! */

/*
 * OK, this is due to a bug in UBC implementation. It may or may not
 * be fixed in later versions. If writev allows you to write single
 * bytes in the first vector then use this version. It's much more
 * efficient.
 */

int     write_x25_socket (fd, buffer, len)
int     fd, len;
char    *buffer;
{
    static u_char mode;
    static struct iovec iov[2] = {
	(char *)&mode, 1,
	"", 0
	};
    int cc;
    char        *p = buffer;
    int         count, total = 0;

    do {
	count = len > X25_PACKETSIZE ? X25_PACKETSIZE : len;
	mode = len > X25_PACKETSIZE ? X25_MBIT : 0;
	iov[1].iov_base = p;
	iov[1].iov_len = count;
	switch (cc = writev (fd, iov, 2))
	{
	    case NOTOK:
	    case 0:
		return cc;

	    case 1:
		SLOG (compat_log, LLOG_EXCEPTIONS, NULLCP,
		      ("strange return from write_x25_socket"));
		return NOTOK;

	    default:
		cc --;
		len -= cc;
		p += cc;
		total += cc;
	}
    } while (len > 0);
    DLOG (compat_log, LLOG_DEBUG, ("X25 write, total %d/%d", total, len));
    return total;
}
#else
int     write_x25_socket (fd, buffer, len)
int     fd, len;
char    *buffer;
{
    char        mybuffer[X25_PACKETSIZE+1];
    char        *p = buffer;
    int         count, total = 0;
    int         cc;

    do {
	count = len > X25_PACKETSIZE ? X25_PACKETSIZE : len;
	mybuffer[0] = len > X25_PACKETSIZE ? X25_MBIT : 0;
	bcopy (p, &mybuffer[1], count);
	switch (cc = write (fd, mybuffer, count + 1))
	{
	    case NOTOK:
	    case 0:
		return cc;

	    case 1:
		SLOG (compat_log, LLOG_EXCEPTIONS, NULLCP,
		      ("strange return from write_x25_socket"));
		return NOTOK;

	    default:
		cc --;
		len -= cc;
		p += cc;
		total += cc;
	}
    } while (len > 0);
    DLOG (compat_log, LLOG_DEBUG, ("X25 write, total %d/%d", total, len));
    return total;
}
#endif

#else   /* UBC_X25 */
int     _ubcx25_stub2 () {};
#endif  /* UBC_X25 */
#else	/* X25 */
int	_ubcx25_stub () {};
#endif  /* X25 */
