/* tsaprovider.c - implement the transport service */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/tsap/RCS/tsaprovider.c,v 7.7 91/02/22 09:47:43 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/tsap/RCS/tsaprovider.c,v 7.7 91/02/22 09:47:43 mrose Interim $
 *
 *
 * $Log:	tsaprovider.c,v $
 * Revision 7.7  91/02/22  09:47:43  mrose
 * Interim 6.8
 * 
 * Revision 7.6  90/11/21  11:31:36  mrose
 * sun
 * 
 * Revision 7.5  90/10/29  18:39:17  mrose
 * updates
 * 
 * Revision 7.4  90/08/08  14:14:08  mrose
 * update
 * 
 * Revision 7.3  90/03/23  17:31:50  mrose
 * 8
 * 
 * Revision 7.2  90/01/11  18:38:09  mrose
 * real-sync
 * 
 * Revision 7.1  89/12/07  01:07:53  mrose
 * queued writes
 * 
 * Revision 7.0  89/11/23  22:30:56  mrose
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
#include <signal.h>
#include "tpkt.h"
#include "mpkt.h"
#include "isoservent.h"
#include "tailor.h"


#define	selmask(fd,m,n) \
{ \
    FD_SET (fd, &(m)); \
    if ((fd) >= (n)) \
	(n) = (fd) + 1; \
}

/*    DATA */

static int once_only = 0;
static struct tsapblk tsapque;
static struct tsapblk *THead = &tsapque;


#ifndef	SIGPOLL
static int TPid = NOTOK;
#endif


extern	int	xselect_blocking_on_intr;

/*    T-DATA.REQUEST */

int     TDataRequest (sd, data, cc, td)
int	sd;
char   *data;
int	cc;
struct TSAPdisconnect *td;
{
    SBV     smask,
	    imask;
    SFP	    istat;
    int     result;
    struct udvec uvs[2];
    register struct udvec *uv = uvs;
    register struct tsapblk *tb;

    missingP (data);
    if (cc <= 0)
	return tsaplose (td, DR_PARAMETER, NULLCP,
		    "illegal value for TSDU length (%d)", cc);
    missingP (td);

    smask = sigioblock ();

    tsapPsig (tb, sd);

    if ((istat = signal (SIGINT, SIG_DFL)) != SIG_DFL) {
	(void) signal (SIGINT, istat);
	imask = siginblock ();
    }

    uv -> uv_base = data, uv -> uv_len = cc, uv++;
    uv -> uv_base = NULL;

    result = (*tb -> tb_writePfnx) (tb, uvs, 0, td);

    if (istat != SIG_DFL)
	(void) siginmask (imask);

    (void) sigiomask (smask);

    return result;
}

/*    T-EXPEDITED-DATA.REQUEST */

int     TExpdRequest (sd, data, cc, td)
int	sd;
char   *data;
int	cc;
struct TSAPdisconnect *td;
{
    SBV     smask,
	    imask;
    SFP	    istat;
    int     result;
    struct udvec uvs[2];
    register struct udvec *uv = uvs;
    register struct tsapblk *tb;

    missingP (data);
    toomuchP (data, cc, TX_SIZE, "expedited");
    if (cc <= 0)
	return tsaplose (td, DR_PARAMETER, NULLCP,
		    "illegal value for XSDU length (%d)", cc);
    missingP (td);

    smask = sigioblock ();

    tsapPsig (tb, sd);

    if ((istat = signal (SIGINT, SIG_DFL)) != SIG_DFL) {
	(void) signal (SIGINT, istat);
	imask = siginblock ();
    }

    uv -> uv_base = data, uv -> uv_len = cc, uv++;
    uv -> uv_base = NULL;

    if (tb -> tb_flags & TB_EXPD)
	result = (*tb -> tb_writePfnx) (tb, uvs, 1, td);
    else
	result = tsaplose (td, DR_OPERATION, NULLCP,
			    "expedited service unavailable");

    if (istat != SIG_DFL)
	(void) siginmask (imask);

    (void) sigiomask (smask);

    return result;
}

/*    T-WRITE.REQUEST (pseudo; write user data vectors) */

int     TWriteRequest (sd, uv, td)
int     sd;
struct udvec *uv;
struct TSAPdisconnect  *td;
{
    register int    n;
    SBV     smask,
	    imask;
    SFP	    istat;
    int     result;
    register struct tsapblk *tb;
    register struct udvec *vv;

    missingP (uv);
    n = 0;
    for (vv = uv; vv -> uv_base; vv++)
	n += vv -> uv_len;
    if (n == 0)
	return tsaplose (td, DR_PARAMETER, NULLCP, "zero-length TSDU");
    missingP (td);

    smask = sigioblock ();

    tsapPsig (tb, sd);

    if ((istat = signal (SIGINT, SIG_DFL)) != SIG_DFL) {
	(void) signal (SIGINT, istat);
	imask = siginblock ();
    }

    result = (*tb -> tb_writePfnx) (tb, uv, 0, td);

    if (istat != SIG_DFL)
	(void) siginmask (imask);

    (void) sigiomask (smask);

    return result;
}

/*    T-READ.REQUEST (pseudo; synchronous read) */

int     TReadRequest (sd, tx, secs, td)
int	sd;
register struct TSAPdata *tx;
int	secs;
register struct TSAPdisconnect *td;
{
    SBV	    smask,
	    imask;
    SFP	    istat;
    int     nfds,
	    oob,
            result;
    fd_set  ifds,
	    efds,
    	    mask;
    register struct tsapblk *tb;

    missingP (tx);
    missingP (td);

    smask = sigioblock ();

    tsapPsig (tb, sd);

    if ((istat = signal (SIGINT, SIG_DFL)) != SIG_DFL) {
	(void) signal (SIGINT, istat);
	imask = siginblock ();
    }

    nfds = 0;
    FD_ZERO (&mask);
    selmask (tb -> tb_fd, mask, nfds);

    for (;;) {
	ifds = efds = mask;		/* struct copy */

	if (tb -> tb_checkfnx == NULLIFP || (*tb -> tb_checkfnx) (tb) != OK)
	    switch ((*tb -> tb_selectfnx) (nfds, &ifds, NULLFD, &efds, secs)) {
		case NOTOK:		/* let read function find error... */
		    ifds = mask;
		    break;

		case OK:
		    result = tsaplose (td, DR_TIMER, NULLCP, NULLCP);
		    goto out;

		default:
		    break;
	    }
	else
	    FD_ZERO (&efds);

	if ((oob = FD_ISSET (tb -> tb_fd, &efds))
		|| FD_ISSET (tb -> tb_fd, &ifds))
	    result = (*tb -> tb_readPfnx) (tb, tx, td, secs != NOTOK, oob);
	else
	    result = DONE;
	if (result != DONE)
	    break;
	if (secs != NOTOK) {
	    result = tsaplose (td, DR_TIMER, NULLCP, NULLCP);
	    break;
	}
    }
out: ;

    if (istat != SIG_DFL)
	(void) siginmask (imask);

    (void) sigiomask (smask);

    return result;
}

/*    T-DISCONNECT.REQUEST */

int     TDiscRequest (sd, data, cc, td)
int	sd;
char   *data;
int	cc;
register struct TSAPdisconnect *td;
{
    SBV     smask;
    int     result;
    register struct tsapblk *tb;

    toomuchP (data, cc, TD_SIZE, "disconnect");

    smask = sigioblock ();

    if ((tb = findtblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return tsaplose (td, DR_PARAMETER, NULLCP,
			 "invalid transport descriptor");
    }

    result = (*tb -> tb_discPfnx) (tb, data, cc, td);

    (void) sigiomask (smask);

    return result;
}

/*    set asynchronous event indications */

static	SFD DATAser ();


int	TSetIndications (sd, data, disc, td)
int	sd;
IFP	data,
	disc;
struct TSAPdisconnect *td;
{
    SBV	    smask;
    int     result;
    register struct tsapblk *tb;

    if (data || disc) {
	missingP (data);
	missingP (disc);
    }

    _iosignals_set = 1;
    smask = sigioblock ();

    tsapPsig (tb, sd);

    if (tb -> tb_DataIndication = data) {
	tb -> tb_flags |= TB_ASYN;
	xselect_blocking_on_intr = 1;
    }
    else {
	tb -> tb_flags &= ~TB_ASYN;
	xselect_blocking_on_intr = 0;
    }
    tb -> tb_DiscIndication = disc;

    result = TWakeUp (tb, td);

    /* Kick the signal handling routine once to make it hand up the
     * indications that were queued in the kernel when TSetIndications
     * was called.
     * TBD: We could be more efficient by only doing this for only
     * one file descriptor.
     */
    (void) DATAser(0, 0L, ((struct sigcontext *) NULL));

    (void) sigiomask (smask);

    return result;
}

/*    map transport descriptors for select() */

int	TSelectMask (sd, mask, nfds, td)
int	sd;
fd_set *mask;
int    *nfds;
register struct TSAPdisconnect *td;
{
    SBV     smask;
    register struct tsapblk *tb;

    missingP (mask);
    missingP (nfds);

    smask = sigioblock ();

    if ((tb = findtblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return tsaplose (td, DR_PARAMETER, NULLCP,
			    "invalid transport descriptor");
    }

    if (tb -> tb_checkfnx && (*tb -> tb_checkfnx) (tb) == OK) {
	(void) sigiomask (smask);
	return tsaplose (td, DR_WAITING, NULLCP, NULLCP);
    }

    selmask (tb -> tb_fd, *mask, *nfds);

    (void) sigiomask (smask);

    return OK;
}

/*    NSAP interface: N-DATA.INDICATION */

/* ARGSUSED */

static	SFD DATAser (sig, code, sc)
int	sig;
long	code;
struct sigcontext *sc;
{
    int     n,
	    nfds,
	    oob,
	    sd;
    fd_set  ifds,
	    efds,
	    imask,
	    emask;
#ifndef	BSDSIGS
    SBV	    smask;
#endif
    IFP	    disc;
    register struct tsapblk *tb,
                           *tb2;
    struct TSAPdata txs;
    register struct TSAPdata   *tx = &txs;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect *td = &tds;

#ifndef	BSDSIGS
    (void) signal (SIGEMT, DATAser);

    smask = sigioblock ();
#endif

    for (;;) {
	n = 0;
	FD_ZERO (&ifds);
	FD_ZERO (&efds);
	for (tb = THead -> tb_forw; tb != THead; tb = tb -> tb_forw)
	    if (tb -> tb_fd != NOTOK && (tb -> tb_flags & TB_ASYN)) {
		nfds = 0;
		FD_ZERO (&imask);
		selmask (tb -> tb_fd, imask, nfds);
		emask = imask;		/* struct copy */
		if ((*tb -> tb_selectfnx) (nfds, &imask, NULLFD, &emask, 0)
			> OK) {
		    if (FD_ISSET (tb -> tb_fd, &imask))
			FD_SET (tb -> tb_fd, &ifds);
		    if (FD_ISSET (tb -> tb_fd, &emask))
			FD_SET (tb -> tb_fd, &efds);
		    n++;
		}
	    }

	if (n == 0)
	    break;

	for (tb = THead -> tb_forw; tb != THead; tb = tb2) {
	    tb2 = tb -> tb_forw;

	    sd = tb -> tb_fd;
	    if ((oob = FD_ISSET (sd, &efds)) || FD_ISSET (sd, &ifds)) {
		disc = tb -> tb_DiscIndication;
		switch ((*tb -> tb_readPfnx) (tb, tx, td, 1, oob)) {
		    case NOTOK:
		        (*disc) (sd, td);
			break;

		    case OK: 
		        (*tb -> tb_DataIndication) (sd, tx);
			break;

		    case DONE:	/* partially assembled TSDU */
		        break;
		}
	    }
	}
    }

#ifndef	SIGPOLL
    (void) kill (TPid, SIGEMT);
#endif

#ifndef	BSDSIGS
    (void) sigiomask (smask);
#endif
}

/*  */

#ifndef	SIGPOLL
static int  TWakeUp (tb, td)
register struct tsapblk *tb;
struct TSAPdisconnect *td;
{
    int     i,
            nfds;
    fd_set  mask;
    char    buf1[10],
            buf2[10],
            buf3[10];
    register struct isoservent *is;
    static int  inited = 0;

    if (TPid > OK) {
	(void) kill (TPid, SIGTERM);
	TPid = NOTOK;
    }

    nfds = 0;
    FD_ZERO (&mask);
    for (tb = THead -> tb_forw; tb != THead; tb = tb -> tb_forw)
	if (tb -> tb_fd != NOTOK && (tb -> tb_flags & TB_ASYN))
	    selmask (tb -> tb_fd, mask, nfds);

    if (nfds == 0)
	return OK;

    if (nfds > sizeof (int) * 8)
	return tsaplose (td, DR_CONGEST, NULLCP, "you lose");
    if (!inited) {
#ifndef	BSDSIGS
	int    smask = sigsetmask (sigblock (0) & ~sigmask (SIGEMT));
#endif

	(void) signal (SIGEMT, DATAser);
#ifndef	BSDSIGS
	(void) sigiomask (smask);
#endif
	inited++;
    }

    if ((is = getisoserventbyname ("isore", "tsap")) == NULL)
	return tsaplose (td, DR_CONGEST, NULLCP,
			 "ISO service tsap/isore not found");

    (void) sprintf (buf1, "%d", nfds);
    *is -> is_tail++ = buf1;
    (void) sprintf (buf2, "0x%x", mask.fds_bits[0]);
    *is -> is_tail++ = buf2;
    (void) sprintf (buf3, "%d", getpid ());
    *is -> is_tail++ = buf3;
    *is -> is_tail = NULL;

    for (i = 0; i < 5; i++)
	switch (TPid = vfork ()) {
	    case NOTOK: 
		continue;

	    case OK: 
		(void) signal (SIGEMT, SIG_DFL);
		(void) execv (*is -> is_vec, is -> is_vec);
		_exit (1);

	    default:
		return OK;
	}

    return tsaplose (td, DR_CONGEST, "isore", "unable to fork");
}
#else
#ifdef	BSDSIGS
#include <fcntl.h>
#include <sys/ioctl.h>
#else
#include <sys/stropts.h>
#endif


static int  TWakeUp (tb, td)
register struct tsapblk *tb;
struct TSAPdisconnect *td;
{
    int	    result;
#ifndef	SUNOS4
    int	    pgrp;
#endif
    static int  inited = 0;

    if (tb -> tb_flags & TB_ASYN) {
	if (!inited) {
	    (void) signal (SIGPOLL, DATAser);

	    inited++;
	}

#ifdef	BSDSIGS
#ifdef	SUNOS4
	if (fcntl (tb -> tb_fd, F_SETOWN, getpid ()) == NOTOK)
	    return tsaplose (td, DR_CONGEST, "failed", "fcntl F_SETOWN");
#else
	pgrp = -getpid ();
	if (ioctl (tb -> tb_fd, SIOCSPGRP, (char *) &pgrp) == NOTOK)
	    return tsaplose (td, DR_CONGEST, "failed", "ioctl SIOCSPGRP %d",
			     pgrp);
#endif
	if ((result = fcntl (tb -> tb_fd, F_GETFL, 0x00)) == NOTOK)
	    return tsaplose (td, DR_CONGEST, "failed", "fcntl F_GETFL");
	result |= FASYNC;
	if (fcntl (tb -> tb_fd, F_SETFL, result) == NOTOK)
	    return tsaplose (td, DR_CONGEST, "failed", "fcntl F_SETFL 0x%x",
			     result);
#else
#ifdef	notdef
	if (ioctl (tb -> tb_fd, I_GETSIG, &result) == NOTOK)
	    result = 0;
	result |= S_INPUT;
	if (ioctl (tb -> tb_fd, I_SETSIG, result) == NOTOK)
	    return tsaplose (td, DR_CONGEST, "failed", "ioctl I_SETSIG 0x%x",
			     result);
#else
	return tsaplose (td, DR_CONGEST, NULLCP,
		     "asynchronous operations not yet supported under SVR3");
#endif
#endif
    }
    else {
#ifdef	BSDSIGS
	if ((result = fcntl (tb -> tb_fd, F_GETFL, 0x00)) == NOTOK)
	    return tsaplose (td, DR_CONGEST, "failed", "fcntl F_GETFL");
	result &= ~FASYNC;
	if (fcntl (tb -> tb_fd, F_SETFL, result) == NOTOK)
	    return tsaplose (td, DR_CONGEST, "failed", "fcntl F_SETFL 0x%x",
			     result);
#else
	if (ioctl (tb -> tb_fd, I_GETSIG, &result) == NOTOK)
	    return tsaplose (td, DR_CONGEST, "failed", "ioctl I_GETSIG");
	result &= ~S_INPUT;
	if (ioctl (tb -> tb_fd, I_SETSIG, result) == NOTOK)
	    return tsaplose (td, DR_CONGEST, "failed", "ioctl I_SETSIG 0x%x",
			     result);
#endif
    }

    return OK;
}
#endif

/*    INTERNAL */

struct tsapblk   *newtblk () {
    register struct tsapblk *tb;

    tb = (struct tsapblk   *) calloc (1, sizeof *tb);
    if (tb == NULL)
	return NULL;

    tb -> tb_fd = NOTOK;

    tb -> tb_qbuf.qb_forw = tb -> tb_qbuf.qb_back = &tb -> tb_qbuf;
    tb -> tb_qwrites.qb_forw = tb -> tb_qwrites.qb_back = &tb -> tb_qwrites;

    if (once_only == 0) {
	THead -> tb_forw = THead -> tb_back = THead;
	once_only++;
    }

    insque (tb, THead -> tb_back);

    return tb;
}


freetblk (tb)
register struct tsapblk *tb;
{
    SBV     smask;
#ifndef	SIGPOLL
    struct TSAPdisconnect   tds;
#endif

    if (tb == NULL)
	return;

    smask = sigioblock ();

    if (tb -> tb_fd != NOTOK) {
	(void) (*tb -> tb_closefnx) (tb -> tb_fd);
#ifdef  MGMT
	if (tb -> tb_manfnx)
	    (*tb -> tb_manfnx) (DISCREQ, tb);
#endif
    }

    if (tb -> tb_retry)
	freetpkt (tb -> tb_retry);

    if (tb -> tb_calling)
	free ((char *) tb -> tb_calling);
    if (tb -> tb_called)
	free ((char *) tb -> tb_called);
    if (tb -> tb_data)
	free (tb -> tb_data);

#ifndef	SIGPOLL
    if ((tb -> tb_flags & TB_ASYN) && TPid > OK) {
	(void) kill (TPid, SIGTERM);
	TPid = NOTOK;
    }
#endif

    QBFREE (&tb -> tb_qbuf);

    if (tb -> tb_queuePfnx)
	(*tb -> tb_queuePfnx) (tb, 0, (struct TSAPdisconnect *) 0);
    QBFREE (&tb -> tb_qwrites);

    remque (tb);

    free ((char *) tb);

#ifndef	SIGPOLL
    for (tb = THead -> tb_forw; tb != THead; tb = tb -> tb_forw)
	if (tb -> tb_fd != NOTOK && (tb -> tb_flags & TB_ASYN)) {
	    (void) TWakeUp (tb, &tds);
	    break;
	}
#endif

    (void) sigiomask (smask);
}

/*  */

struct tsapblk   *findtblk (sd)
register int sd;
{
    register struct tsapblk *tb;

    if (once_only == 0)
	return NULL;

    for (tb = THead -> tb_forw; tb != THead; tb = tb -> tb_forw)
	if (tb -> tb_fd == sd)
	    return tb;

    return NULL;
}

/*  */

int	copyTSAPaddrX (in, out)
struct tsapADDR *in;
struct TSAPaddr *out;
{
    bzero ((char *) out, sizeof *out);

    bcopy (in -> ta_selector, out -> ta_selector,
	   out -> ta_selectlen = in -> ta_selectlen);

    if (in -> ta_present) {
	out -> ta_addrs[0] = in -> ta_addr;		/* struct copy */
	out -> ta_naddr = 1;
    }
}


int	copyTSAPaddrY (in, out)
struct TSAPaddr *in;
struct tsapADDR *out;
{
    bzero ((char *) out, sizeof *out);

    bcopy (in -> ta_selector, out -> ta_selector,
	   out -> ta_selectlen = in -> ta_selectlen);

    if (out -> ta_present = (in -> ta_naddr >= 1) ? 1 : 0)
	out -> ta_addr = in -> ta_addrs[0];		/* struct copy */
}
