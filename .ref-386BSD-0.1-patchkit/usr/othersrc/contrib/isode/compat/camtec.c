/* camtec.c - X.25, CONS abstractions for CAMTEC CCL  */

#ifndef lint
static char *rcsid = "$Header: /f/osi/compat/RCS/camtec.c,v 7.3 91/02/22 09:15:02 mrose Interim $";
#endif

/*
 * $Header: /f/osi/compat/RCS/camtec.c,v 7.3 91/02/22 09:15:02 mrose Interim $
 *
 * Contributed by Keith Ruttle, CAMTEC Electronics Ltd
 *
 *
 * $Log:	camtec.c,v $
 * Revision 7.3  91/02/22  09:15:02  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/14  13:33:46  mrose
 * loader
 * 
 * Revision 7.1  90/07/09  14:31:37  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:22:56  mrose
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

#ifdef  X25

#include "x25.h"
#include "isoaddrs.h"


#ifdef  CAMTEC_CCL

#include "tailor.h"
#include "tpkt.h"

/*    4.[23] UNIX: CCL X25 */


static char calling_dte[NSAP_DTELEN + 1];

/*  */

int     start_x25_client (local)
struct NSAPaddr *local;
{
    int     sd, pgrp;
    CONN_DB l_iov;

    if (local != NULLNA)
	local -> na_stack = NA_X25, local -> na_community = ts_comm_x25_default;
    if ((sd = socket (AF_CCL, SOCK_STREAM, CCLPROTO_X25)) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("socket"));
	return NOTOK; /* Error can be found in errno */
    }

    pgrp = getpid();
    if (ioctl(sd, SIOCSPGRP, &pgrp)) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("SIOCSPGRP"));
	return NOTOK; /* Error can be found in errno */
    }

    l_iov.ccl_iovec[0].iov_base = calling_dte;
    gen2if(local, &l_iov, ADDR_LOCAL);

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
    CONN_DB b_iov;
    char param1[128];

    b_iov.ccl_iovec[0].iov_base = param1;

    if ((sd = socket (AF_CCL, SOCK_STREAM, CCLPROTO_X25)) == NOTOK) {
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

    (void) gen2if (local, &b_iov, ADDR_LISTEN);
    if (bind (sd, &b_iov, sizeof(b_iov)) != NOTOK) {
	if (ioctl(sd, CCL_AUTO_ACCEPT, 1) < 0) {
		SLOG (compat_log, LLOG_EXCEPTIONS, "failed",
		      ("CCL_AUTO_ACCEPT"));
		close (sd);
		return NOTOK;
	}
	(void) listen (sd, backlog);
	return sd;
    }

    (void) close (sd);
    return NOTOK;
}

/*  */

int     join_x25_client (fd, remote)
int     fd;
struct NSAPaddr *remote;
{
    CONN_DB     sck;
    struct iovec *iov;
    int         i, len = 0;
    int         nfd;
    char        param1[128];
    char        param2[128];
    char        param3[128];
    char        param4[256];

    iov = &(sck.ccl_iovec[0]);
    if((nfd = accept (fd, (char *) 0, &len)) == NOTOK)
	return NOTOK;
    iov[0].iov_base = param1;
    iov[1].iov_base = param2;
    iov[2].iov_base = param3;
    iov[3].iov_base = param4;
    iov[0].iov_len = iov[1].iov_len = iov[2].iov_len = 128;
    iov[3].iov_len = 256;
    iov[4].iov_len = iov[5].iov_len = iov[6].iov_len = 0;
    if (ioctl(nfd, CCL_FETCH_CONNECT, &iov[0]) < 0)
	return NOTOK;
    (void) if2gen (remote, &sck, ADDR_REMOTE);
    ioctl (nfd, CCL_SEND_TYPE, 0);
    return nfd;
}

int     join_x25_server (fd, remote)
int     fd;
struct NSAPaddr *remote;
{
    CONN_DB zsck;
    CONN_DB *sck = &zsck;
    int r;
    struct iovec *iov = &( zsck.ccl_iovec[0] );
    char        param1[128];
    char        param3[128];
    char        param4[256];

    if (remote == NULLNA || remote -> na_stack != NA_X25)
    {
	SLOG (compat_log, LLOG_EXCEPTIONS, NULLCP,
	      ("Invalid type na%d", remote->na_stack));
	return NOTOK;
    }

    iov[0].iov_base = param1;
    iov[1].iov_base = calling_dte;
    iov[1].iov_len = strlen(calling_dte);
    iov[2].iov_base = param3;
    iov[3].iov_base = param4;
    iov[4].iov_len = iov[5].iov_len = iov[6].iov_len = 0;

    (void) gen2if (remote, sck, ADDR_REMOTE);
    if ((r = connect (fd, sck, sizeof (CONN_DB))) >= 0)
	    ioctl (fd, CCL_SEND_TYPE, 0);
    bzero(calling_dte, sizeof calling_dte );
    return (r);
}

int     read_x25_socket (fd, buffer, len)
int     fd, len;
char    *buffer;
{
    static u_char mode;
    int cc, count = 0, total = len;
    char *p = buffer;

    do {
	    cc = recv (fd, p, total, 0);
	    switch (cc) {
		case NOTOK:
			if (errno == ECONNRESET) {
				struct iovec iov[7];
				char parm[34];
				int i;
				iov[0].iov_base = parm;
				iov[0].iov_len = 1;
				iov[1].iov_base = parm + 1;
				iov[1].iov_len = 32;
				for (i = 2; i < 7; i++) {
					iov[i].iov_base = (char *)0;
					iov[i].iov_len = 0;
				}
				ioctl(fd, CCL_FETCH_RESET, iov);
			elucidate_x25_err( 1 << RECV_DIAG, iov[0].iov_base);
			}
		case 0:
			return cc;

		default:
			ioctl (fd, CCL_RECV_TYPE, &mode);
			count += cc;
			p += cc;
			total -= cc;
			break;
	    }
    } while (total > 0 && (mode & MORE_DATA));

    DLOG (compat_log, LLOG_DEBUG,
	  ("X25 read, total %d", count ));
    return count;
}

int     write_x25_socket (fd, buffer, len)
int     fd, len;
char    *buffer;
{
    int         count;
    int         cc;

    count = send(fd, buffer, len, 0);
    DLOG (compat_log, LLOG_DEBUG,
	  ("X25 write, total %d/%d", count, len));
    return count;
}
#else 	/* CAMTEC_CCL */
int	_sunlink_stub2 () {};
#endif  /* CAMTEC_CCL */
#else	/* X25 */
int	_sunlink_stub () {};
#endif  /* X25 */
