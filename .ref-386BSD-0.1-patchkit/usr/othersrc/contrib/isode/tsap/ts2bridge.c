/* ts2bridge.c - TPM: X.25 interface via bridge */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/tsap/RCS/ts2bridge.c,v 7.5 91/02/22 09:47:16 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/tsap/RCS/ts2bridge.c,v 7.5 91/02/22 09:47:16 mrose Interim $
 *
 * Contributed by Julian Onions, Nottingham University in the UK
 *
 *
 * $Log:	ts2bridge.c,v $
 * Revision 7.5  91/02/22  09:47:16  mrose
 * Interim 6.8
 * 
 * Revision 7.4  91/01/14  13:34:21  mrose
 * loader
 * 
 * Revision 7.3  90/07/09  14:51:11  mrose
 * sync
 * 
 * Revision 7.2  90/03/23  17:31:14  mrose
 * 8
 * 
 * Revision 7.1  89/12/07  01:07:34  mrose
 * queued writes
 * 
 * Revision 7.0  89/11/23  22:30:35  mrose
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
#ifdef BRIDGE_X25
#include <sys/uio.h>
#include <sys/ioctl.h>
#include "tailor.h"

/*
 * This could be anything up to the size TCP encapsualtion howver, to
 * interwork with the X.25 it should be the same as X.25's MAXNSDU.
 * This define is clearly a hack - but including x25.h messes things up more!
 */
#define       MAXNSDU (1024)
  
extern int errno;

/*    N-CONNECT.REQUEST */
/* ARGSUSED */
int	bridgeopen (tb, local, remote, td, async)
register struct tsapblk *tb;
struct NSAPaddr *local,
		*remote;
struct TSAPdisconnect *td;
{
    register int    fd;

    if ((fd = start_bridge_client (local)) == NOTOK)
	return tsaplose (td, DR_CONGEST, "socket", "unable to start");

    tb -> tb_fd = fd;
    (void) BTService (tb);

    if (join_bridge_server (fd, remote) == NOTOK) {
	(void) tsaplose (td, DR_REFUSED, "connection", "unable to establish");
	(void) close_bridge_socket (fd);
	return (tb -> tb_fd = NOTOK);
    }
    return DONE;
}

/*  */

/* ARGSUSED */

static int  bridgeretry (tb, td)
struct tsapblk *tb;
struct TSAPdisconnect *td;
{
    int	fd = tb -> tb_fd;
    fd_set mask;

    FD_ZERO (&mask);
    FD_SET (fd, &mask);

    if (xselect (fd + 1, NULLFD, &mask, NULLFD, 0) < 1)
	return OK;

    return DONE;
}

/*    init for read from network/write to network */

#define	bridgeinit		tcpinit
#define bridgeread		read
int	close_bridge_socket ();
#define select_bridge_socket	selsocket

int	tcpinit ();
int	tcpwrite ();
int	selsocket ();
int	read ();

/*  */

/* ARGSUSED */

char   *bridgesave (fd, dte1, l1, dte2, l2, td)
int	fd;
char   *dte1;
int     l1;
char   *dte2;
int     l2;
struct TSAPdisconnect *td;
{
    static char buffer[BUFSIZ];

    (void) sprintf (buffer, "%c%d %*s %*s",
                        NT_BRG, fd, l1, dte1, l2, dte2);
    return buffer;
}


int	bridgerestore (tb, buffer, td)
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
    na -> na_stack = NA_BRG;
    na -> na_community = ts_comm_x25_default;
    bcopy(dte1, na -> na_dte, strlen(dte1));
    na -> na_dtelen = strlen (na -> na_dte);

    tb -> tb_fd = fd;
    (void) BTService (tb);

    ta = &tb -> tb_responding;
    ta -> ta_present = 1;
    na = &ta -> ta_addr;
    na -> na_stack = NA_BRG;
    na -> na_community = ts_comm_x25_default;
    bcopy(dte1, na -> na_dte, strlen(dte2));
    na -> na_dtelen = strlen (na -> na_dte);

    return OK;
}

/*  */

int	BTService (tb)
register struct tsapblk *tb;
{
    tb -> tb_flags |= TB_BRG;

    tb -> tb_tsdusize = MAXNSDU - (tb -> tb_tpduslop = DT_MAGIC);

    tb -> tb_retryfnx = bridgeretry;

    tb -> tb_initfnx = bridgeinit;
    tb -> tb_readfnx = bridgeread;
    tb -> tb_writefnx = tp0write;
    tb -> tb_closefnx = close_bridge_socket;
    tb -> tb_selectfnx = select_bridge_socket;

    tp0init (tb);
}
#else
int	_ts2bridge_stub () {};
#endif
