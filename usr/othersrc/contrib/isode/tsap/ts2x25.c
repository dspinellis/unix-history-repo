/* ts2x25.c - TPM: X.25 interface */

#ifndef lint
static char *rcsid = "$Header: /f/osi/tsap/RCS/ts2x25.c,v 7.5 91/02/22 09:47:24 mrose Interim $";
#endif

/*
 * $Header: /f/osi/tsap/RCS/ts2x25.c,v 7.5 91/02/22 09:47:24 mrose Interim $
 *
 *
 * $Log:	ts2x25.c,v $
 * Revision 7.5  91/02/22  09:47:24  mrose
 * Interim 6.8
 * 
 * Revision 7.4  91/01/14  13:34:39  mrose
 * loader
 * 
 * Revision 7.3  90/07/09  14:51:23  mrose
 * sync
 * 
 * Revision 7.2  90/03/23  17:31:30  mrose
 * 8
 * 
 * Revision 7.1  89/12/07  01:07:39  mrose
 * queued writes
 * 
 * Revision 7.0  89/11/23  22:30:41  mrose
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
#include "tpkt.h"
#include "tailor.h"

#ifdef  X25
#include "x25.h"
#include <sys/ioctl.h>

static  fd_set  inprogress;
static  struct NSAPaddr **peers = NULL;

extern int errno;

/*    N-CONNECT.REQUEST */

int     x25open (tb, local, remote, td, async)
register struct tsapblk *tb;
struct NSAPaddr *local,
                *remote;
struct TSAPdisconnect *td;
{
    register int    fd;
    int     onoff;

        /*
         * start_x25_client does nothing with its arguments in the CAMTEC
         * case but there's less #ifdef code this way so...
         */
    if ((fd = start_x25_client (local)) == NOTOK)
        return tsaplose (td, DR_CONGEST, "socket", "unable to start");

    if (async) {
        if (ioctl (fd, FIONBIO, (onoff = 1, (char *) &onoff)) < 0) {
            (void) tsaplose (td, DR_CONGEST, "ioctl", "FIONBIO");
            (void) close_x25_socket (fd);
            return NOTOK;
        }
    }
    tb -> tb_fd = fd;
    (void) XTService (tb);

    if (join_x25_server (fd, remote) == NOTOK) {
        if (async)
            switch (errno) {
                case EINPROGRESS:
                    if (peers == NULL) {
                        peers = (struct NSAPaddr **)
                                        calloc ((unsigned) getdtablesize (),
                                                sizeof *peers);
                        if (peers == NULL) {
                            (void) tsaplose (td, DR_CONGEST, NULLCP,
                                             "out of memory");
                            (void) close_x25_socket (fd);
                            return (tb -> tb_fd = NOTOK);
                        }

                        FD_ZERO (&inprogress);
                    }
		    if (peers[fd] == NULL
			    && (peers[fd] = (struct NSAPaddr *)
						malloc (sizeof **peers))
				    == NULL) {
			(void) tsaplose (td, DR_CONGEST, NULLCP,
					 "out of memory");
			(void) close_x25_socket (fd);
			return (tb -> tb_fd = NOTOK);
		    }
		    *(peers[fd]) = *remote;	/* struct copy */
                    FD_SET (fd, &inprogress);
                    return OK;

                case EISCONN:
                    goto done;

                default:
                    break;
            }

        (void) tsaplose (td, DR_REFUSED, "connection", "unable to establish");
        LLOG (x25_log, LLOG_NOTICE,
	      ("connection to %s failed", na2str (remote)));
        (void) close_x25_socket (fd);
        return (tb -> tb_fd = NOTOK);
    }
done: ;

    if (async)
        if (ioctl (fd, FIONBIO, (onoff = 0, (char *) &onoff)) < 0) {
            (void) tsaplose (td, DR_CONGEST, "ioctl", "FIONBIO");
            (void) close_x25_socket (fd);
            return NOTOK;
        }

    (void) XTService (tb);      /* in case pktsize changed... */
    LLOG (x25_log, LLOG_NOTICE,
	  ("connection %d to %s", fd, na2str (remote)));

    return DONE;
}

/*  */

static int  x25retry (tb, td)
struct tsapblk *tb;
struct TSAPdisconnect *td;
{
    int     onoff;
    int     fd = tb -> tb_fd;
    fd_set  mask;
    struct NSAPaddr *remote = peers[fd];

    FD_ZERO (&mask);
    FD_SET (fd, &mask);
    if (xselect (fd + 1, NULLFD, &mask, NULLFD, 0) < 1)
        return OK;

    if (!FD_ISSET (fd, &inprogress))
        return DONE;

    if (join_x25_server (fd, remote) == NOTOK) {
        switch (errno) {
            case EINPROGRESS:
                return OK;

            case EISCONN:
                goto done;

            case EINVAL:        /* UNIX bug: could be any socket errno, e.g.,
                                   ETIMEDOUT */
                errno = ECONNREFUSED;
                /* and fall */
            default:
                break;
        }

        (void) tsaplose (td, DR_REFUSED, "connection", "unable to establish");
        FD_CLR (fd, &inprogress);
        (void) close_x25_socket (fd);
        LLOG (x25_log, LLOG_NOTICE,
	      ("connection to %s failed", na2str (remote)));
        return (tb -> tb_fd = NOTOK);
    }
done: ;

    (void) ioctl (fd, FIONBIO, (onoff = 0, (char *) &onoff));
    FD_CLR (fd, &inprogress);

    (void) XTService (tb);      /* in case pktsize changed... */
    LLOG (x25_log, LLOG_NOTICE,
	  ("connection %d to %s", fd, na2str (remote)));

    return DONE;
}

/*    init for read from network */

static char nsdu[MAXNSDU];
static char *np;
static int  bl;


static int  x25init (fd, t)
int     fd;
register struct tsapkt *t;
{
    register int    cc;

/* XXX: cc should be set to the maximum acceptable NSDU length.
   Longer NSDUs will be truncated without notification.
   Should be configurable (or set during N-CONNECT and remembered) */

    cc = sizeof nsdu;

    switch (cc = read_x25_socket (fd, nsdu, cc)) {
        case OK:                /* no data ? */
        case NOTOK:
#ifdef  SUN_X25
            if (compat_log -> ll_events & LLOG_EXCEPTIONS)
                (void) log_cause_and_diag(fd);
#endif
            return DR_NETWORK;

        default:
            t -> t_length = cc + sizeof t -> t_pkthdr;
            break;
    }

    if (t -> t_length < TPKT_HDRLEN (t))
        return DR_LENGTH;

    t -> t_li = nsdu[0];
    t -> t_code = nsdu[1];

    np = nsdu + 2;
    bl = t -> t_length - TPKT_HDRLEN (t);

    t -> t_vrsn = TPKT_VRSN;    /* Not really needed! */

    return OK;
}


/* ARGSUSED */

static int  read_nsdu_buffer (fd, buffer, cc)
int     fd;
register char  *buffer;
register int    cc;
{
    if (cc > bl)
        cc = bl;

    if (cc > 0) {
        bcopy (np, buffer, cc);
        np += cc, bl -= cc;
    }

    return cc;
}

/*  */

/* ARGSUSED */

char   *x25save (fd, dte1, l1, dte2, l2, td)
int     fd;
char   *dte1;
int     l1;
char   *dte2;
int     l2;
struct TSAPdisconnect *td;
{
    static char buffer[BUFSIZ];

    (void) sprintf (buffer, "%c%d %*s %*s",
                        NT_X25, fd, l1, dte1, l2, dte2);

    return buffer;
}


int     x25restore (tb, buffer, td)
register struct tsapblk *tb;
char   *buffer;
struct TSAPdisconnect *td;
{
    int     fd;
    char    dte1[NSAP_DTELEN + 1],
            dte2[NSAP_DTELEN + 1];
    register struct NSAPaddr   *na;
    register struct tsapADDR   *ta;

    if (sscanf (buffer, "%d %s %s", &fd, dte1, dte2) != 3 || fd < 0)
        return tsaplose (td, DR_PARAMETER, NULLCP,
                "bad initialization vector \"%s\"", buffer);

    ta = &tb -> tb_initiating;
    ta -> ta_present = 1;
    na = &ta -> ta_addr;
    na -> na_stack = NA_X25;
    na -> na_community = ts_comm_x25_default;
    bcopy(dte1, na -> na_dte, strlen(dte1));
    na -> na_dtelen = strlen (na -> na_dte);

    tb -> tb_fd = fd;
    (void) XTService (tb);

    ta = &tb -> tb_responding;
    ta -> ta_present = 1;
    na = &ta -> ta_addr;
    na -> na_stack = NA_X25;
    na -> na_community = ts_comm_x25_default;
    bcopy(dte2, na -> na_dte, strlen(dte2));
    na -> na_dtelen = strlen (na -> na_dte);

#ifdef  SUN_X25
    (void) set_x25_facilities (tb -> tb_fd, -1, "Negotiated");
#endif

    return OK;
}

/*  */

int     XTService (tb)
register struct tsapblk *tb;
{
#ifndef	UBC_X25
    int     maxnsdu = MAXNSDU;
#else
    int     maxnsdu = X25_PACKETSIZE;
#endif

    tb -> tb_flags |= TB_X25;

#ifdef  notyet
    if (recvpktsize > DT_MAGIC && recvpktsize < maxnsdu)
        maxnsdu = recvpktsize;
    if (sendpktsize > DT_MAGIC && sendpktsize < maxnsdu)
        maxnsdu = sendpktsize;
#endif
    tb -> tb_tsdusize = maxnsdu - (tb -> tb_tpduslop = DT_MAGIC);

    tb -> tb_retryfnx = x25retry;

    tb -> tb_initfnx = x25init;
    tb -> tb_readfnx = read_nsdu_buffer;
    tb -> tb_writefnx = tp0write;
    tb -> tb_closefnx = close_x25_socket;
    tb -> tb_selectfnx = select_x25_socket;

    tp0init (tb);
}
#else
int	_ts2x25_stub () {};
#endif
