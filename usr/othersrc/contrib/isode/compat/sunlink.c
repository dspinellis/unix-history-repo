/* sunlink.c - X.25 abstractions for SunLink X25 */

#ifndef lint
static char *rcsid = "$Header: /f/osi/compat/RCS/sunlink.c,v 7.6 91/02/22 09:16:06 mrose Interim $";
#endif

/*
 * $Header: /f/osi/compat/RCS/sunlink.c,v 7.6 91/02/22 09:16:06 mrose Interim $
 *
 * Contributed by John Pavel, Department of Trade and Industry/National
 * Physical Laboratory in the UK
 *
 *
 * $Log:	sunlink.c,v $
 * Revision 7.6  91/02/22  09:16:06  mrose
 * Interim 6.8
 * 
 * Revision 7.5  91/01/14  13:33:49  mrose
 * loader
 * 
 * Revision 7.4  91/01/07  12:39:59  mrose
 * update
 * 
 * Revision 7.3  90/10/15  18:19:36  mrose
 * sync
 * 
 * Revision 7.2  90/07/27  08:41:49  mrose
 * update
 * 
 * Revision 7.1  90/07/09  14:32:22  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:23:42  mrose
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

#include <stdio.h>
#include "general.h"
#include "manifest.h"
#include "tailor.h"

/*    SUN UNIX: SunLink X25 */

#ifdef  X25

#include "x25.h"
#include "isoaddrs.h"

#ifdef  SUN_X25

#define CALLING 0
#define CALLED  1
#define	PROBE	(-1)

/*  */

#ifdef  DEBUG
void    print_x25_facilities ();
#endif

/*  */

/* ARGSUSED */

int     start_x25_client (local, priv)
struct  NSAPaddr *local;
int     priv;
{
    int     sd;

    if ((sd = socket (AF_X25, SOCK_STREAM, 0)) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("socket"));
	return NOTOK;
    }

    return sd;
}

/*  */

int     start_x25_server (local, backlog, opt1, opt2)
struct  NSAPaddr *local;
int     backlog,
	opt1,
	opt2;
{
    CONN_DB     sbuf,
		xbuf;
    CONN_DB     *sock = &sbuf,
		*xs = &xbuf;
    int     sd, onoff;

    if ((sd = socket (AF_X25, SOCK_STREAM, 0)) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("socket"));
	return NOTOK;
    }

    onoff = 0;
    if (ioctl (sd, X25_CALL_ACPT_APPROVAL, (char *) &onoff) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed",
	      ("X25_CALL_ACPT_APPROVAL"));
	(void) close_x25_socket (sd);
	return NOTOK;
    }

    if (ioctl (sd, X25_RD_HOSTADR, (char *) xs) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("X25_RD_HOSTADR"));
	(void) close_x25_socket (sd);
	return NOTOK;
    }
					/* if null DTE in /etc/x25params
					   then ALWAYS use null DTE for
					   listen if PID is specified */
    if (xs -> hostlen == 0 && local -> na_pidlen > 0) {
	if (local -> na_pidlen > NPSIZE)
	    local -> na_pidlen = NPSIZE;
	*sock = *xs;    /* struct copy */
	bzero((char *) sock -> data, NPSIZE);
	bcopy (local -> na_pid, (char *) sock -> data, local -> na_pidlen);
	bcopy (local -> na_cudf, (char *) sock -> data + NPSIZE,
	       local -> na_cudflen);
	sock -> datalen = local -> na_pidlen + local -> na_cudflen;
    }
    else
	sock = gen2if(local, sock, ADDR_LISTEN);

    /* Adopt the convention that if a null DTE is given,
       we should get the one from /etc/x25params */
    if(!local->na_dtelen) {
	/* Now set the generic local address */
	local = if2gen(local, xs, ADDR_LOCAL);
	/* Modified by INRIA to avoid a null local address */
	if (!local->na_dtelen) {
		local->na_dtelen = 1;
		local->na_dte[0] = '0';
	}
    }

    if (bind (sd, (struct sockaddr *) sock, sizeof(CONN_DB)) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("bind"));
	(void) close_x25_socket (sd);
	return NOTOK;
    }

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

    if (set_x25_facilities(sd, CALLED, "Acceptable") == NOTOK) {
	(void) close_x25_socket (sd);
	return NOTOK;
    }

    (void) listen (sd, backlog);

    return sd;
}

/*  */

int     join_x25_server (fd, remote)
register int fd;
register struct NSAPaddr *remote;
{
    CONN_DB sbuf;
    CONN_DB *sock = &sbuf;
    register int nfd;

    if ((sock = gen2if (remote, sock, ADDR_REMOTE)) == NULL)
	return NOTOK;

    if (set_x25_facilities(fd, CALLING, "Proposed") == NOTOK)
	return NOTOK;
#ifdef ANY_LINK
    sock -> hostlen |= ANY_LINK;
#endif
    if ((nfd = connect (fd, (struct sockaddr *)sock, sizeof (CONN_DB)))
	    == NOTOK) {
	if (compat_log -> ll_events & LLOG_EXCEPTIONS)
	    (void) log_cause_and_diag(fd);      /* Sun's documentation throwns
						   no light as to whether, or
						   not this will result in any
						   useful information */
    }
#ifdef  DEBUG
    else
	if (compat_log -> ll_events & LLOG_DEBUG)
	    (void) log_x25_facilities(fd, CALLING, "Effective Calling");
#endif
#ifdef ANY_LINK
    sock -> hostlen &= ~ANY_LINK;
#endif
    remote = if2gen (remote, sock, ADDR_REMOTE);

    return nfd;
}

/*  */

int     join_x25_client (fd, remote)
int     fd;
struct  NSAPaddr *remote;
{
    CONN_DB     sbuf;
    CONN_DB     *sock = &sbuf;
    int     len = sizeof *sock;
    int     nfd;

    if ((nfd = accept (fd, (struct sockaddr *) sock, &len)) == NOTOK) {
	if (compat_log -> ll_events & LLOG_EXCEPTIONS)
	    (void) log_cause_and_diag(fd);      /* Sun's documentation throwns
						   no light as to whether, or
						   not this will result in any
						   useful information */
    }
#ifdef  DEBUG
     else
	 if (compat_log -> ll_events & LLOG_DEBUG)
	     (void) log_x25_facilities(fd, CALLED, "Effective Called");
#endif
    if (nfd < 0) return nfd;

    /* May also need to send call accept packet if using
     * FAST_ACPT_CLR, or X25_CALL_ACPT_APPROVAL
     * there was a SUNLINK bug in this area
     *
     * May as well try it -- if it fails, so what ??
     */
    if (ioctl(nfd,X25_SEND_CALL_ACPT, NULLCP) < 0)
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("X25_SEND_CALL_ACPT"));

    remote = if2gen (remote, sock, ADDR_REMOTE);

    return nfd;
}

/*  */

/* There is a bug whereby if the thruput is set, calls fail. pb@cl.cam.ac.uk */

int	sun_fixed_thruput = 0;

/* Set up X.25 Facilities.  Note that setting even one value causes
   the default (/etc/x25params) values to be set explicitly on the
   call request (and probably also call accept).  This can screw
   things up, if your /etc/x25params has not been properly
   localised as is normally the case.  */

int     set_x25_facilities(sd, coc, caption)
int     sd, coc;
char *caption;
{
    FACILITY_DB facilities;

    bzero ((char *) &facilities, sizeof facilities);

    if (coc != CALLED
	    && ioctl (sd, X25_RD_FACILITY, (char *) &facilities) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("X25_RD_FACILITY"));
	return NOTOK;
    }

    if (coc == PROBE
	    || !(coc == CALLED
		    || reverse_charge   || recvpktsize || sendpktsize
		    || recvwndsize      || sendwndsize || recvthruput
		    || sendthruput      || cug_req  /* || cug_index */
		    || fast_select_type || rpoa_req /* || rpoa */)) {
	if (facilities.recvpktsize)
	    recvpktsize = facilities.recvpktsize;
	if (facilities.sendpktsize)
	    sendpktsize = facilities.sendpktsize;

	return OK;
    }

    if (reverse_charge)
	facilities.reverse_charge = reverse_charge;
    if (recvpktsize)
	facilities.recvpktsize = recvpktsize;
    if (sendpktsize)
	facilities.sendpktsize = sendpktsize;
    if (recvwndsize)
	facilities.recvwndsize = recvwndsize;
    if (sendwndsize)
	facilities.sendwndsize = sendwndsize;
    if (sun_fixed_thruput) {	/* get round Sun bug */
	if (recvthruput)
	    facilities.recvthruput = recvthruput;
	if (sendthruput)
	    facilities.sendthruput = sendthruput;
    }
    else
	facilities.recvthruput = facilities.sendthruput = 0;
    if (cug_req)
	facilities.cug_req = cug_req;
    if (cug_index)
	facilities.cug_index = cug_index;
    if (fast_select_type)
	facilities.fast_select_type = fast_select_type;
      /* May as well accept FCS calls */
    else
	if (coc == CALLED)
	    facilities.fast_select_type = FAST_ACPT_CLR;
    if (rpoa_req)
	facilities.rpoa_req = rpoa_req;
    if (rpoa)
	facilities.rpoa = rpoa;

#define DO_NOT_USE_FACILITIES
#ifndef DO_NOT_USE_FACILITIES
#ifdef  DEBUG
    if (compat_log -> ll_events & LLOG_DEBUG)
	print_x25_facilities (facilities, coc, caption);
#endif

    if (ioctl (sd, X25_WR_FACILITY, (char *) &facilities) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("X25_WR_FACILITY"));
	return NOTOK;
    }
#endif

#ifdef	notyet
    if (facilities.recvpktsize)
	recvpktsize = facilities.recvpktsize;
    if (facilities.sendpktsize)
	sendpktsize = facilities.sendpktsize;
#endif

    return OK;
}

/*  */

int     log_cause_and_diag(fd)
int fd;
{
    X25_CAUSE_DIAG      diag;

    if (ioctl(fd, X25_RD_CAUSE_DIAG, (char *) &diag) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("X25_RD_CAUSE_DIAG"));
	return NOTOK;
    }

    return elucidate_x25_err ((int) diag.flags, (char *) diag.data);

}

/*  */

#ifdef  DEBUG

static int  log_x25_facilities (fd, coc, caption)
int     fd;
int     coc;
char   *caption;
{
    FACILITY_DB f;

    if (ioctl (fd, X25_RD_FACILITY, (char *) &f) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("X25_RD_FACILITY"));
	return NOTOK;
    }

    print_x25_facilities (f, coc, caption);

    return OK;
}

/*  */

static void  print_x25_facilities (f, coc, caption)
FACILITY_DB f;
int     coc;
char   *caption;
{
    int     baud;

    DLOG (compat_log, LLOG_DEBUG, ("%s X.25 Facilities:", caption));

    switch (f.reverse_charge) {
	case 0:
	    DLOG (compat_log, LLOG_DEBUG, ((coc == CALLED)
		      ? "reverse charging not requested"
		      : "reverse charging not allowed"));
	    break;

	case 1:
	    DLOG (compat_log, LLOG_DEBUG, ((coc == CALLING)
		      ? "reverse charging requested"
		      : "reverse charging allowed"));
	    break;

	default:
	    DLOG (compat_log, LLOG_DEBUG, ("invalid reverse_charge: %d",
		      f.reverse_charge));
	    break;
    }

    switch (f.recvpktsize) {
	case 0:
	    DLOG (compat_log, LLOG_DEBUG, ("default recv packet size"));
	    break;

	case 16:
	case 32:
	case 64:
	case 128:
	case 256:
	case 512:
	case 1024:
	    DLOG (compat_log, LLOG_DEBUG, ("recv packet size %d",
		      f.recvpktsize));
	    break;

	default:
	    DLOG (compat_log, LLOG_DEBUG, ("invalid recv packet size %d",
		      f.recvpktsize));
	    break;
    }

    switch (f.sendpktsize) {
	case 0:
	    DLOG (compat_log, LLOG_DEBUG, ("default send packet size"));
	    break;

	case 16:
	case 32:
	case 64:
	case 128:
	case 256:
	case 512:
	case 1024:
	    DLOG (compat_log, LLOG_DEBUG, ("send packet size %d",
		      f.sendpktsize));
	    break;

	default:
	    DLOG (compat_log, LLOG_DEBUG, ("invalid send packet size %d",
		      f.sendpktsize));
	    break;
    }

    DLOG (compat_log, LLOG_DEBUG,
	  (f.recvwndsize == 0 ? "default recv window size"
		  : 1 <= f.recvwndsize && f.recvwndsize <= 127
		      ? "recv window size %d"
		      : "invalid recv window size %d",
	      f.recvwndsize));

    DLOG (compat_log, LLOG_DEBUG,
	  (f.sendwndsize == 0 ? "default send window size"
		  : 1 <= f.sendwndsize && f.sendwndsize <= 127
		      ? "send window size %d"
		      : "invalid send window size %d",
	      f.sendwndsize));

    switch (f.recvthruput) {
	case 0:
	    DLOG (compat_log, LLOG_DEBUG, ("default recv throughput"));
	    break;

	case 3:
	    baud = 75;
print_recv: ;
	    DLOG (compat_log, LLOG_DEBUG, ("recv throughput %dbps", baud));
	    break;

	case 4:
	    baud = 150;
	    goto print_recv;

	case 5:
	    baud = 300;
	    goto print_recv;

	case 6:
	    baud = 600;
	    goto print_recv;

	case 7:
	    baud = 1200;
	    goto print_recv;

	case 8:
	    baud = 2400;
	    goto print_recv;

	case 9:
	    baud = 4800;
	    goto print_recv;

	case 10:
	    baud = 9600;
	    goto print_recv;

	case 11:
	    baud = 19200;
	    goto print_recv;

	case 12:
	    baud = 48000;
	    goto print_recv;

	default:
	    DLOG (compat_log, LLOG_DEBUG, ("invalid recv throughput %d",
		      f.recvthruput));
	    break;
    }

    switch (f.sendthruput) {
	case 0:
	    DLOG (compat_log, LLOG_DEBUG, ("default send throughput"));
	    break;

	case 3:
	    baud = 75;
print_send: ;
	    DLOG (compat_log, LLOG_DEBUG, ("send throughput %dbps", baud));
	    break;

	case 4:
	    baud = 150;
	    goto print_send;

	case 5:
	    baud = 300;
	    goto print_send;

	case 6:
	    baud = 600;
	    goto print_send;

	case 7:
	    baud = 1200;
	    goto print_send;

	case 8:
	    baud = 2400;
	    goto print_send;

	case 9:
	    baud = 4800;
	    goto print_send;

	case 10:
	    baud = 9600;
	    goto print_send;

	case 11:
	    baud = 19200;
	    goto print_send;

	case 12:
	    baud = 48000;
	    goto print_send;

	default:
	    DLOG (compat_log, LLOG_DEBUG, ("invalid send throughput %d",
		      f.sendthruput));
	    break;
    }

    switch (f.cug_req) {
	case 0:
	    DLOG (compat_log, LLOG_DEBUG, ("no closed user group"));
	    break;

	case 1:
	    DLOG (compat_log, LLOG_DEBUG, ("closed user group 0x%x (BCD)",
		      f.cug_req));
	    break;

	default:
	    DLOG (compat_log, LLOG_DEBUG, ("invalid closed user group %d",
		      f.cug_req));
	    break;
    }

    switch (f.fast_select_type) {
	case FAST_OFF:
	    DLOG (compat_log, LLOG_DEBUG, ("don't use fast select"));
	    break;

	case FAST_CLR_ONLY:
	    DLOG (compat_log, LLOG_DEBUG, ("clear is fast select response"));
	    break;

	case FAST_ACPT_CLR:
	    DLOG (compat_log, LLOG_DEBUG,
		  ("clear or call accepted is fast select response"));
	    break;

	default:
	    DLOG (compat_log, LLOG_DEBUG, ("invalid fast select type %d",
		      f.fast_select_type));
	    break;
    }

    switch (f.rpoa_req) {
	case 0:
	    DLOG (compat_log, LLOG_DEBUG, ("no RPOA transit request"));
	    break;

	case 1:
	    DLOG (compat_log, LLOG_DEBUG, ("RPOA transit request 0x%x",
		      f.rpoa_req));
	    break;

	default:
	    DLOG (compat_log, LLOG_DEBUG, ("invalid RPOA transit request %d",
		      f.rpoa_req));
    }
}
#endif
#endif
#else
int	_sunlink_stub () {};
#endif
