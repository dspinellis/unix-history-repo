/* tp0ts.c - TPM: TP0 engine */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/tsap/RCS/tp0ts.c,v 7.10 91/03/09 11:58:08 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/tsap/RCS/tp0ts.c,v 7.10 91/03/09 11:58:08 mrose Exp $
 *
 *
 * $Log:	tp0ts.c,v $
 * Revision 7.10  91/03/09  11:58:08  mrose
 * update
 * 
 * Revision 7.9  91/02/22  09:47:14  mrose
 * Interim 6.8
 * 
 * Revision 7.8  90/12/11  10:51:41  mrose
 * lock-and-load
 * 
 * Revision 7.7  90/11/05  13:33:19  mrose
 * update
 * 
 * Revision 7.6  90/07/27  08:48:10  mrose
 * update
 * 
 * Revision 7.5  90/03/23  17:31:10  mrose
 * 8
 * 
 * Revision 7.4  89/12/19  10:18:22  mrose
 * DLOG
 * 
 * Revision 7.3  89/12/08  09:41:23  mrose
 * touch-up
 * 
 * Revision 7.2  89/12/07  22:15:39  mrose
 * touch-up
 * 
 * Revision 7.1  89/12/07  01:07:30  mrose
 * queued writes
 * 
 * Revision 7.0  89/11/23  22:30:33  mrose
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
#include "tailor.h"


#if	defined(TCP) || defined(X25)

/*  */

static int  TConnect (tb, expedited, data, cc, td)
register struct tsapblk *tb;
char    *data;
int	expedited,
	cc;
struct TSAPdisconnect *td;
{
    register struct tsapkt *t;

    if (!(tb -> tb_flags & TB_TCP)) {
	expedited = 0;
	if (cc > 0)
	    return tsaplose (td, DR_PARAMETER, NULLCP,
		    "initial user data not allowed with class 0");
    }

    tb -> tb_srcref = htons ((u_short) (getpid () & 0xffff));
    tb -> tb_dstref = htons ((u_short) 0);

    if ((t = newtpkt (TPDU_CR)) == NULL)
	return tsaplose (td, DR_CONGEST, NULLCP, "out of memory");

    t -> t_cr.cr_dstref = tb -> tb_dstref;
    t -> t_cr.cr_srcref = tb -> tb_srcref;
    t -> t_cr.cr_class = CR_CLASS_TP0;
    {
	register int    i,
	                j;
	int     k;

	i = k = tb -> tb_tsdusize + tb -> tb_tpduslop;
	for (j = 0; i > 0; j++)
	    i >>= 1;
	if (k == (1 << (j - 1)))
	    j--;
	if (tb -> tb_flags & TB_TCP) {
	    if (j <= SIZE_8K)
		t -> t_tpdusize = j;
	}
	else {
	    if (j > SIZE_MAXTP0) {
		j = SIZE_MAXTP0;
		tb -> tb_tsdusize = (1 << j) - tb -> tb_tpduslop;
	    }
	    if (j != SIZE_DFLT)
		t -> t_tpdusize = j;
	}
    }
    bcopy (tb -> tb_initiating.ta_selector, t -> t_calling,
		t -> t_callinglen = tb -> tb_initiating.ta_selectlen);

    bcopy (tb -> tb_responding.ta_selector, t -> t_called,
		t -> t_calledlen = tb -> tb_responding.ta_selectlen);
    if (expedited) {
	tb -> tb_flags |= TB_EXPD;
	t -> t_options |= OPT_TEXPEDITE;
    }

    copyTPKTdata (t, data, cc);	/* XXX: user musn't touch! */

    tb -> tb_retry = t;

    return OK;
}

/*  */

static int  TRetry (tb, async, tc, td)
register struct tsapblk *tb;
int	async;
struct TSAPconnect *tc;
struct TSAPdisconnect *td;
{
    int	    len;
    register struct tsapkt *t;

    if (t = tb -> tb_retry) {
	tb -> tb_retry = NULL;

	if (async)
	    switch ((*tb -> tb_retryfnx) (tb, td)) {
		case NOTOK:
	    	    goto out;

	        case OK:
		    tb -> tb_retry = t;
		    return CONNECTING_1;

		case DONE:
		    break;		    
	    }

	if (tpkt2fd (tb, t, tb -> tb_writefnx) == NOTOK) {
	    (void) tsaplose (td, t -> t_errno, NULLCP, NULLCP);
	    goto out;
	}

	freetpkt (t), t = NULL;
    }

    if (async) {
	fd_set	mask;

	FD_ZERO (&mask);
	FD_SET (tb -> tb_fd, &mask);

	if (xselect (tb -> tb_fd + 1, &mask, NULLFD, NULLFD, 0) == OK)
	    return CONNECTING_2;
    }

    if ((t = fd2tpkt (tb -> tb_fd, tb -> tb_initfnx, tb -> tb_readfnx)) == NULL
	    || t -> t_errno != OK) {
	(void) tsaplose (td, t ? t -> t_errno : DR_CONGEST, NULLCP, NULLCP);
	goto out;
    }

    switch (TPDU_CODE (t)) {
	case TPDU_CC: 
	    tc -> tc_sd = tb -> tb_fd;
	    if (CR_CLASS (t) != CR_CLASS_TP0) {
		(void) tpktlose (tb, td, DR_PROTOCOL, NULLCP,
			"proposed class 0, got back 0x%x", CR_CLASS (t));
		goto out;
	    }
	    if (tb -> tb_srcref != t -> t_cc.cc_dstref) {
		(void) tpktlose (tb, td, DR_MISMATCH, NULLCP,
			"sent srcref of 0x%x, got 0x%x",
			ntohs (tb -> tb_srcref), ntohs (t -> t_cc.cc_dstref));
		goto out;
	    }
	    tb -> tb_dstref = t -> t_cc.cc_srcref;
	    if (!(tb -> tb_flags & TB_TCP) || t -> t_tpdusize) {
		if (t -> t_tpdusize == 0)
		    t -> t_tpdusize = SIZE_DFLT;
		else
		    if (t -> t_tpdusize > SIZE_MAXTP0
			    && !(tb -> tb_flags & TB_TCP))
			t -> t_tpdusize = SIZE_MAXTP0;
		tb -> tb_tpdusize = 1 << t -> t_tpdusize;
		tb -> tb_tsdusize = tb -> tb_tpdusize - tb -> tb_tpduslop;
	    }
	    if ((len = t -> t_calledlen) > 0) {
		if (len > sizeof tb -> tb_responding.ta_selector)
		    len = sizeof tb -> tb_responding.ta_selector;
		bcopy (t -> t_called, tb -> tb_responding.ta_selector,
			tb -> tb_responding.ta_selectlen = len);
	    }
	    copyTSAPaddrX (&tb -> tb_responding, &tc -> tc_responding);
	    if (!(t -> t_options & OPT_TEXPEDITE)
		    || !(tb -> tb_flags & TB_TCP))
		tb -> tb_flags &= ~TB_EXPD;
	    tc -> tc_expedited = (tb -> tb_flags & TB_EXPD) ? 1 : 0;
	    tc -> tc_tsdusize = tb -> tb_tsdusize;
	    tc -> tc_qos = tb -> tb_qos;	/* struct copy */
	    if (t -> t_qbuf) {
		copyTSAPdata (t -> t_qbuf -> qb_data, t -> t_qbuf -> qb_len,
			tc);
	    }
	    else
		tc -> tc_cc = 0;

	    freetpkt (t);
	    tb -> tb_flags |= TB_CONN;
#ifdef  MGMT
	    if (tb -> tb_manfnx)
		(*tb -> tb_manfnx) (OPREQOUT, tb);
#endif
	    if (tb -> tb_calling)
		free ((char *) tb -> tb_calling), tb -> tb_calling = NULL;
	    if (tb -> tb_called)
		free ((char *) tb -> tb_called), tb -> tb_called = NULL;

	    return DONE;

	case TPDU_DR: 
	    td -> td_reason = t -> t_dr.dr_reason;
	    if (t -> t_qbuf) {
		copyTSAPdata (t -> t_qbuf -> qb_data, t -> t_qbuf -> qb_len,
			td);
	    }
	    else
		td -> td_cc = 0;
	    goto out;

	case TPDU_ER:
	    switch (t -> t_er.er_reject) {
		case ER_REJ_NOTSPECIFIED:
		default:
		    td -> td_reason = DR_CONNECT;
		    break;

		case ER_REJ_CODE:
		case ER_REJ_TPDU:
		case ER_REJ_VALUE:
		    td -> td_reason = DR_PROTOCOL;
		    break;
	    }
	    td -> td_cc = 0;
	    goto out;
	    
	default: 
	    (void) tpktlose (tb, td, DR_PROTOCOL, NULLCP,
		    "transport protocol mangled: expecting 0x%x, got 0x%x",
		    TPDU_CC, TPDU_CODE (t));
	    goto out;
    }

out: ;
    freetpkt (t);
/*    freetblk (tb); */

    return NOTOK;
}

/*  */

static int  TStart (tb, cp, ts, td)
register struct tsapblk *tb;
char   *cp;
struct TSAPstart *ts;
struct TSAPdisconnect *td;
{
    int	    len,
	    result;
    register struct tsapkt *t;

    if ((t = str2tpkt (cp)) == NULL || t -> t_errno != OK) {
	result = tsaplose (td, DR_PARAMETER, NULLCP,
		    "bad initialization vector");
	goto out;
    }

    if (CR_CLASS (t) != CR_CLASS_TP0) {
	if (t -> t_cr.cr_alternate & (ALT_TP0 | ALT_TP1))
	    t -> t_cr.cr_class = CR_CLASS_TP0;
	else {
	    result = tpktlose (tb, td, DR_CONNECT, NULLCP,
			"only class 0 supported, not 0x%x", CR_CLASS (t));
	    goto out;
	}
    }

    tb -> tb_srcref = htons ((u_short) (getpid () & 0xffff));
    tb -> tb_dstref = t -> t_cr.cr_srcref;
    if (!(tb -> tb_flags & TB_TCP) || t -> t_tpdusize) {
	if (t -> t_tpdusize == 0)
	    t -> t_tpdusize = SIZE_DFLT;
	else
	    if (t -> t_tpdusize > SIZE_MAXTP0
		    && !(tb -> tb_flags & TB_TCP))
		t -> t_tpdusize = SIZE_MAXTP0;
	tb -> tb_tpdusize = 1 << t -> t_tpdusize;
	tb -> tb_tsdusize = tb -> tb_tpdusize - tb -> tb_tpduslop;
    }
    if ((len = t -> t_callinglen) > 0) {
	if (len > sizeof tb -> tb_initiating.ta_selector)
	    len = sizeof tb -> tb_initiating.ta_selector;
	bcopy (t -> t_calling, tb -> tb_initiating.ta_selector,
			tb -> tb_initiating.ta_selectlen = len);
    }
    if ((len = t -> t_calledlen) > 0) {
	if (len > sizeof tb -> tb_responding.ta_selector)
	    len = sizeof tb -> tb_responding.ta_selector;
	bcopy (t -> t_called, tb -> tb_responding.ta_selector,
			tb -> tb_responding.ta_selectlen = len);
    }
    if ((t -> t_options & OPT_TEXPEDITE) && (tb -> tb_flags & TB_TCP))
	tb -> tb_flags |= TB_EXPD;

    ts -> ts_sd = tb -> tb_fd;
    copyTSAPaddrX (&tb -> tb_initiating, &ts -> ts_calling);
    copyTSAPaddrX (&tb -> tb_responding, &ts -> ts_called);
    ts -> ts_expedited = (tb -> tb_flags & TB_EXPD) ? 1 : 0;
    ts -> ts_tsdusize = tb -> tb_tsdusize;
    ts -> ts_qos = tb -> tb_qos;	/* struct copy */

    if (t -> t_qbuf) {
	copyTSAPdata (t -> t_qbuf -> qb_data, t -> t_qbuf -> qb_len, ts);
    }
    else
	ts -> ts_cc = 0;

    result = OK;

out: ;
    freetpkt (t);

    return result;
}

/*  */

/* ARGSUSED */

static int  TAccept (tb, responding, data, cc, qos, td)
register struct tsapblk *tb;
char   *data;
int	responding,
	cc;
struct QOStype *qos;
struct TSAPdisconnect *td;
{
    int	    result;
    register struct tsapkt *t;

    if (!(tb -> tb_flags & TB_TCP) && cc > 0)
	return tsaplose (td, DR_PARAMETER, NULLCP,
		    "initial user data not allowed with class 0");

    if ((t = newtpkt (TPDU_CC)) == NULL)
	return tsaplose (td, DR_CONGEST, NULLCP, "out of memory");

    t -> t_cc.cc_dstref = tb -> tb_dstref;
    t -> t_cc.cc_srcref = tb -> tb_srcref;
    t -> t_cc.cc_class = CR_CLASS_TP0;
    {
	register int    i,
	                j;
	int     k;

	i = k = tb -> tb_tsdusize + tb -> tb_tpduslop;
	for (j = 0; i > 0; j++)
	    i >>= 1;
	if (k == (1 << (j - 1)))
	    j--;
	if (tb -> tb_flags & TB_TCP) {
	    if (j <= SIZE_8K)
		t -> t_tpdusize = j;
	}
	else {
	    if (j > SIZE_MAXTP0) {
		j = SIZE_MAXTP0;
		tb -> tb_tsdusize = (1 << j) - tb -> tb_tpduslop;
	    }
	    if (j != SIZE_DFLT)
		t -> t_tpdusize = j;
	}
    }
    if (responding)
	bcopy (tb -> tb_responding.ta_selector, t -> t_called,
			t -> t_calledlen = tb -> tb_responding.ta_selectlen);
    if (tb -> tb_flags & TB_EXPD)
	t -> t_options |= OPT_TEXPEDITE;
    copyTPKTdata (t, data, cc);

    if ((result = tpkt2fd (tb, t, tb -> tb_writefnx)) == NOTOK)
	(void) tsaplose (td, t -> t_errno, NULLCP, NULLCP);
    else {
	tb -> tb_flags |= TB_CONN;
#ifdef  MGMT
	if (tb -> tb_manfnx)
	    (*tb -> tb_manfnx) (OPREQIN, tb);
#endif
    }

    freetpkt (t);

    return result;
}

/*  */

static int  TWrite (tb, uv, expedited, td)
register struct tsapblk *tb;
register struct udvec *uv;
int	expedited;
struct TSAPdisconnect *td;
{
    int	    cc,
	    j,
	    len,
	    result;
#if	defined(X25) || defined(MGMT)
    int	    dlen;
#endif
    register char *bp,
		  *ep;
    register struct tsapkt *t;
    register struct udvec *vv,
			  *wv;

#if	defined(X25) || defined(MGMT)
    dlen = 0;
#endif

    ep = (bp = uv -> uv_base) + (cc = uv -> uv_len);
    while (uv -> uv_base) {
	if ((t = newtpkt (expedited ? TPDU_ED : TPDU_DT)) == NULL)
	    return tsaplose (td, DR_CONGEST, NULLCP, "out of memory");

	wv = (vv = t -> t_udvec) + NTPUV - 1;
	len = tb -> tb_tpdusize ? (tb -> tb_tpdusize - tb -> tb_tpduslop)
			        : tb -> tb_tsdusize;
	for (; len > 0 && vv < wv; len -= j) {
	    j = min (cc, len);
#if	defined(X25) || defined(MGMT)
	    dlen += j;
#endif
	    vv -> uv_base = bp, vv -> uv_len = j, vv++;
	    bp += j, cc -= j;

	    if (bp >= ep) {
		if ((bp = (++uv) -> uv_base) == NULL)
		    break;
		ep = bp + (cc = uv -> uv_len);
	    }
	}

	if (uv -> uv_base == NULL)
	    t -> t_dt.dt_nr |= DT_EOT;

	if ((result = tpkt2fd (tb, t, tb -> tb_writefnx)) == NOTOK) {
	    (void) tsaplose (td, t -> t_errno, NULLCP, NULLCP);
#ifdef	X25
	    if (tb -> tb_flags & TB_X25)
		LLOG (x25_log, LLOG_NOTICE,
		      ("connection %d broken, %d/%d octets sent/recv",
		       tb -> tb_fd, tb -> tb_sent, tb -> tb_recv));
#endif
	    freetblk (tb);
	}

	freetpkt (t);
	if (result == NOTOK)
	    return NOTOK;
    }

#ifdef	X25
    tb -> tb_sent += dlen;
#endif
#ifdef  MGMT
    if (tb -> tb_manfnx)
	(*tb -> tb_manfnx) (USERDT, tb, dlen);
#endif

    return OK;
}

/*  */

/* ARGSUSED */

static int  TRead (tb, tx, td, async, oob)
register struct tsapblk *tb;
register struct TSAPdata *tx;
struct TSAPdisconnect *td;
int	async,
	oob;
{
    int     eot;
    register struct tsapkt *t;

    bzero ((char *) tx, sizeof *tx);
    tx -> tx_qbuf.qb_forw = tx -> tx_qbuf.qb_back = &tx -> tx_qbuf;

    for (;;) {
	if ((t = fd2tpkt (tb -> tb_fd, tb -> tb_initfnx, tb -> tb_readfnx))
			== NULL
		|| t -> t_errno != OK) {
	    (void) tsaplose (td, t ? t -> t_errno : DR_CONGEST, NULLCP,
		    NULLCP);
#ifdef	X25
	    if (tb -> tb_flags & TB_X25)
		LLOG (x25_log, LLOG_NOTICE,
		      ("connection %d broken, %d/%d octets sent/recv",
		       tb -> tb_fd, tb -> tb_sent, tb -> tb_recv));
#endif
	    break;
	}

	switch (TPDU_CODE (t)) {
	    case TPDU_DT: 
		eot = t -> t_dt.dt_nr & DT_EOT;
		if (t -> t_qbuf) {
		    insque (t -> t_qbuf, tb -> tb_qbuf.qb_back);
		    tb -> tb_len += t -> t_qbuf -> qb_len;
#ifdef	X25
		    tb -> tb_recv += t -> t_qbuf -> qb_len;
#endif
		    t -> t_qbuf = NULL;
		}
		freetpkt (t);
#ifdef  MGMT
		if (tb -> tb_manfnx)
		    (*tb -> tb_manfnx) (USERDR, tb, tb -> tb_len);
#endif
		if (!eot) {
		    if (async)
			return DONE;

		    continue;
		}
		tx -> tx_expedited = 0;
		if (tb -> tb_qbuf.qb_forw != &tb -> tb_qbuf) {
		    tx -> tx_qbuf = tb -> tb_qbuf;/* struct copy */
		    tx -> tx_qbuf.qb_forw -> qb_back =
			    tx -> tx_qbuf.qb_back -> qb_forw = &tx -> tx_qbuf;
		    tx -> tx_cc = tb -> tb_len;
		    tb -> tb_qbuf.qb_forw =
			    tb -> tb_qbuf.qb_back = &tb -> tb_qbuf;
		    tb -> tb_len = 0;
		}
		return OK;

	    case TPDU_ED:
		if (t -> t_qbuf) {
		    insque (t -> t_qbuf, tx -> tx_qbuf.qb_back);
		    tx -> tx_cc = t -> t_qbuf -> qb_len;
		    t -> t_qbuf = NULL;
		}
		freetpkt (t);
		tx -> tx_expedited = 1;
		return OK;

	    case TPDU_DR: 
		td -> td_reason = t -> t_dr.dr_reason;
		if (t -> t_qbuf) {
		    copyTSAPdata (t -> t_qbuf -> qb_data,
			    t -> t_qbuf -> qb_len, td);
		}
		else
		    td -> td_cc = 0;
		break;

	    case TPDU_ER:
		switch (t -> t_er.er_reject) {
		    case ER_REJ_NOTSPECIFIED:
		    default:
			td -> td_reason = DR_UNKNOWN;
			break;

		    case ER_REJ_CODE:
		    case ER_REJ_TPDU:
		    case ER_REJ_VALUE:
			td -> td_reason = DR_PROTOCOL;
			break;
		}
		td -> td_cc = 0;
		break;

	    default: 
		(void) tpktlose (tb, td, DR_PROTOCOL, NULLCP,
			"transport protocol mangled: not expecting 0x%x",
			TPDU_CODE (t));
		break;
	}
	break;
    }

    freetpkt (t);
    freetblk (tb);

    return NOTOK;
}

/*  */

static int  TDisconnect (tb, data, cc, td)
register struct tsapblk *tb;
char   *data;
int	cc;
struct TSAPdisconnect *td;
{
    int     result;
#ifdef	TCP
    register struct tsapkt *t;
#endif

    result = OK;
#ifdef	TCP
    if (tb -> tb_flags & TB_TCP) {
	if (t = newtpkt (TPDU_DR)) {
	    t -> t_dr.dr_srcref = tb -> tb_srcref;
	    t -> t_dr.dr_dstref = tb -> tb_dstref;
	    t -> t_dr.dr_reason = DR_NORMAL;
	    copyTPKTdata (t, data, cc);

	    if ((result = tpkt2fd (tb, t, tb -> tb_writefnx)) == NOTOK)
		(void) tsaplose (td, t -> t_errno, NULLCP, NULLCP);

	    freetpkt (t);
	}
	else
	    result = tsaplose (td, DR_CONGEST, NULLCP, "out of memory");
    }
#endif
#ifdef X25
    if (tb -> tb_flags & TB_X25)
	LLOG (x25_log, LLOG_NOTICE,
	     ("connection %d closed, %d/%d octets sent/recv",
	      tb -> tb_fd, tb -> tb_sent, tb -> tb_recv));
#endif
    freetblk (tb);

    return result;
}

/*  */

static	TLose (tb, reason, td)
register struct tsapblk *tb;
int	reason;
struct TSAPdisconnect *td;
{
    struct tsapkt  *t;

    switch (reason) {
	case DR_UNKNOWN: 
	case DR_CONGEST: 
	case DR_SESSION: 
	case DR_ADDRESS: 
	    if ((t = newtpkt (TPDU_DR)) == NULLPKT)
		break;

	    t -> t_dr.dr_srcref = tb -> tb_srcref;
	    t -> t_dr.dr_dstref = tb -> tb_dstref;
	    t -> t_dr.dr_reason = reason;
	    copyTPKTdata (t, td -> td_data, td -> td_cc);
	    break;

	default: 
	    if ((t = newtpkt (TPDU_ER)) == NULLPKT)
		break;

	    t -> t_er.er_dstref = tb -> tb_dstref;
	    switch (reason) {
		case DR_PROTOCOL: 
		    t -> t_er.er_reject = ER_REJ_TPDU;
		    break;

		default: 
		    t -> t_er.er_reject = ER_REJ_NOTSPECIFIED;
		    break;
	    }
	    break;
    }
    if (t) {
	(void) tpkt2fd (tb, t, tb -> tb_writefnx);
	freetpkt (t);
    }
}

/*  */

/* at present, used by TCP, X.25 and BRG back-ends... */

#ifndef	TCP
#undef	WRITEV
#endif

#include <errno.h>
#include <sys/ioctl.h>
#ifdef	SYS5
#include <fcntl.h>
#endif
#ifdef	WRITEV
#include <sys/uio.h>
#endif
#ifdef	TCP
#include "internet.h"
#else
#define	write_tcp_socket	NULLIFP
#endif
#ifdef	X25
#include "x25.h"
#else
#define	write_x25_socket	NULLIFP
#endif

#if	defined(FIONBIO) || defined(O_NDELAY)
#define	NODELAY
#endif


extern	int	errno;

/*  */

int	tp0write (tb, t, cp, n)
register struct tsapblk *tb;
register struct tsapkt *t;
char   *cp;
int	n;
{
    register int    cc;
    register char   *p,
		    *q;
    register struct qbuf *qb;
    register struct udvec  *uv;
#if	defined(WRITEV) || defined(SUN_X25) || defined(CAMTEC_CCL)
#ifdef	UBC_X25_WRITEV
    struct iovec iovs[NTPUV + 5];
    char no_mbit = 0;
#else
    struct iovec iovs[NTPUV + 4];
#endif
    register struct iovec *iov;
#endif

#if	defined(WRITEV) || defined(SUN_X25) || defined(CAMTEC_CCL)
#ifdef	NODELAY
    if (tb -> tb_flags & TB_QWRITES)
	goto single;
#endif
    iov = iovs;
    cc = 0;

    if (tb -> tb_flags & TB_X25) {
#ifdef UBC_X25_WRITEV
	iov -> iov_base = &no_mbit;
	cc += (iov -> iov_len = sizeof no_mbit);
	iov++;
#endif
	iov -> iov_base = (char *) &t -> t_li;
	cc += (iov -> iov_len = sizeof t -> t_li);
	iov++;

	iov -> iov_base = (char *) &t -> t_code;
	cc += (iov -> iov_len = sizeof t -> t_code);
	iov++;

    }
    else {
	iov -> iov_base = (char *) &t -> t_pkthdr;
	cc += (iov -> iov_len = TPKT_HDRLEN (t));
	iov++;
    }

    iov -> iov_base = cp;
    cc += (iov -> iov_len = n);
    iov++;

    if (t -> t_vdata) {
	iov -> iov_base = t -> t_vdata;
	cc += (iov -> iov_len = t -> t_vlen);
	iov++;
    }

    for (uv = t -> t_udvec; uv -> uv_base; uv++) {
	iov -> iov_base = uv -> uv_base;
	cc += (iov -> iov_len = uv -> uv_len);
	iov++;
    }

    if ((n = writev (tb -> tb_fd, iovs, iov - iovs)) != cc) {
	cc = NOTOK;
#ifdef	SUN_X25
	if (tb -> tb_flags & TB_X25
	        && compat_log -> ll_events & LLOG_EXCEPTIONS)
	    (void) log_cause_and_diag (tb -> tb_fd);
#endif
    }
    else
	if (tb -> tb_flags & TB_X25) {
	    DLOG (compat_log, LLOG_DEBUG, ("X.25 write %d bytes", cc));
	}
    goto out;

single: ;
#endif

    cc = ((tb -> tb_flags & TB_X25) ? sizeof t -> t_li + sizeof t -> t_code
	  			    : TPKT_HDRLEN (t))
		+ n;
    if (t -> t_vdata)
	cc += t -> t_vlen;
    for (uv = t -> t_udvec; uv -> uv_base; uv++)
	cc += uv -> uv_len;

    if (p = malloc (sizeof *qb + (unsigned) cc)) {
	int	nc,
		onoff;
	IFP	wfnx = (tb -> tb_flags & TB_X25) ? write_x25_socket
						 : write_tcp_socket;

#ifdef	NODELAY
	if (tb -> tb_flags & TB_QWRITES) {
	    qb = (struct qbuf *) p;
	    qb -> qb_forw = qb -> qb_back = qb;
	    qb -> qb_data = qb -> qb_base, qb -> qb_len = cc;
	    p = qb -> qb_data;
	}
#endif

	if (tb -> tb_flags & TB_X25) {
	    bcopy ((char *) &t -> t_li, q = p, sizeof t -> t_li);
	    q += sizeof t -> t_li;

	    bcopy ((char *) &t -> t_code, q, sizeof t -> t_code);
	    q += sizeof t -> t_code;
	}
	else {
	    bcopy ((char *) &t -> t_pkthdr, q = p, TPKT_HDRLEN (t));
	    q += TPKT_HDRLEN (t);
	}

	bcopy (cp, q, n);
	q += n;

	if (t -> t_vdata) {
	    bcopy (t -> t_vdata, q, t -> t_vlen);
	    q += t -> t_vlen;
	}

	for (uv = t -> t_udvec; uv -> uv_base; uv++) {
	    bcopy (uv -> uv_base, q, uv -> uv_len);
	    q += uv -> uv_len;
	}

#ifdef	NODELAY
	if (tb -> tb_qwrites.qb_forw != &tb -> tb_qwrites) {
	    nc = 0;
	    goto insert;
	}

	if (tb -> tb_flags & TB_QWRITES) {
#ifdef	FIONBIO
	    (void) ioctl (tb -> tb_fd, FIONBIO, (onoff = 1, (char *) &onoff));
#else
#ifdef	O_NDELAY
	    (void) fcntl (tb -> tb_fd, F_SETFL, O_NDELAY);
#endif
#endif
	}
#endif

	nc = (*wfnx) (tb -> tb_fd, p, cc);

#ifdef	NODELAY
	if (tb -> tb_flags & TB_QWRITES) {
#ifdef	FIONBIO
	    (void) ioctl (tb -> tb_fd, FIONBIO, (onoff = 0, (char *) &onoff));
#else
#ifdef	O_NDELAY
	    (void) fcntl (tb -> tb_fd, F_SETFL, 0x00);
#endif
#endif
	}
#endif

	if (nc != cc) {
#ifdef	NODELAY
	    if (tb -> tb_flags & TB_QWRITES) {
		if (nc == NOTOK) {
		    if (errno != EWOULDBLOCK)
			goto losing;
		    nc = 0;
		}
		else
		    if (nc > 0 && (tb -> tb_flags & TB_X25)) {
			SLOG (tsap_log, LLOG_EXCEPTIONS, NULLCP,
			      ("partial write (%d of %d octets) to X.25",
			       nc, cc));
			goto losing;
		    }

		if ((*tb -> tb_queuePfnx) (tb, 1, (struct TSAPdisconnect *) 0)
		        == NOTOK)
		    goto losing;

		qb -> qb_data += nc, qb -> qb_len -= nc;
insert: ;
		insque (qb, tb -> tb_qwrites.qb_back);
		DLOG (tsap_log, LLOG_TRACE,
		      ("queueing blocked write of %d of %d octets", nc, cc));
		qb = NULL;
	    }
	    else
#endif
	    {
losing: ;
		cc = NOTOK;
	    }
	}

#ifdef	NODELAY
	if (tb -> tb_flags & TB_QWRITES) {
	    if (qb)
		free ((char *) qb);
	}
	else
#endif
	    free (p);
	goto out;
    }
    if ((tb -> tb_flags & TB_X25) || tb -> tb_flags & TB_QWRITES) {
	SLOG (tsap_log, LLOG_EXCEPTIONS, NULLCP,
	      ("unable to malloc %d octets for pseudo-writev, failing...",
	       cc));

	cc = NOTOK;
	goto out;
    }

#ifdef	TCP
    SLOG (tsap_log, LLOG_EXCEPTIONS, NULLCP,
	  ("unable to malloc %d octets for pseudo-writev, continuing...",
	   cc));

    cc = TPKT_HDRLEN (t);
    if (write_tcp_socket (tb -> tb_fd, (char *) &t -> t_pkthdr, cc) != cc) {
err: 	;
	cc = NOTOK;
	goto out;
    }

    if (write_tcp_socket (tb -> tb_fd, cp, n) != n)
	goto err;
    cc += n;

    if (t -> t_vdata
	    && write_tcp_socket (tb -> tb_fd, t -> t_vdata, t -> t_vlen)
		    != t -> t_vlen)
	goto err;
    cc += t -> t_vlen;

    for (uv = t -> t_udvec; uv -> uv_base; uv++) {
	if (write_tcp_socket (tb -> tb_fd, uv -> uv_base, uv -> uv_len)
		!= uv -> uv_len)
	    goto err;
	cc += uv -> uv_len;
    }
#endif

out: ;

    return cc;
}

/*  */

#ifdef	NODELAY
static int  TDrain (tb, td)
register struct tsapblk *tb;
struct TSAPdisconnect *td;
{
    int	    nc,
	    onoff,
	    result;
    register struct qbuf *qb;
    IFP	    wfnx = (tb -> tb_flags & TB_X25) ? write_x25_socket
					     : write_tcp_socket;
    SFP	    pstat;
    SBV	    smask;
  
    pstat = signal (SIGPIPE, SIG_IGN);
    smask = sigioblock ();

#ifdef	FIONBIO
    (void) ioctl (tb -> tb_fd, FIONBIO, (onoff = 1, (char *) &onoff));
#else
#ifdef	O_NDELAY
    (void) fcntl (tb -> tb_fd, F_SETFL, O_NDELAY);
#endif
#endif

    while ((qb = tb -> tb_qwrites.qb_forw) != &tb -> tb_qwrites) {
	if ((nc = (*wfnx) (tb -> tb_fd, qb -> qb_data, qb -> qb_len))
	        != qb -> qb_len) {
	    if (nc == NOTOK) {
		if (errno != EWOULDBLOCK) {
		    result = tsaplose (td, DR_NETWORK, "failed",
				      "write to network");
		    goto out;
		}

		nc = 0;
	    }
	    else
		if (nc > 0 && (tb -> tb_flags & TB_X25)) {
		    SLOG (tsap_log, LLOG_EXCEPTIONS, NULLCP,
			  ("partial write (%d of %d octets) to X.25",
			   nc, qb -> qb_len));
		    result = tsaplose (td, DR_NETWORK, NULLCP,
				       "partial write (%d of %d octets) to X.25",
				       nc, qb -> qb_len);
		    goto out;
		}

	    DLOG (tsap_log, LLOG_TRACE,
		  ("wrote %d of %d octets from blocked write", nc,
		   qb -> qb_len));
	    qb -> qb_data += nc, qb -> qb_len -= nc;

	    result = OK;
	    goto out;
	}

	DLOG (tsap_log, LLOG_TRACE,
	      ("finished blocked write of %d octets", qb -> qb_len));
	remque (qb);
	free ((char *) qb);
    }
    result = DONE;

out: ;
#ifdef	FIONBIO
    (void) ioctl (tb -> tb_fd, FIONBIO, (onoff = 0, (char *) &onoff));
#else
#ifdef	O_NDELAY
    (void) fcntl (tb -> tb_fd, F_SETFL, 0x00);
#endif
#endif

    (void) sigiomask (smask);
    (void) signal (SIGPIPE, pstat);

    return result;
}
#endif

/*  */

int	tp0init (tb)
register struct tsapblk *tb;
{
    tb -> tb_connPfnx = TConnect;
    tb -> tb_retryPfnx = TRetry;

    tb -> tb_startPfnx = TStart;
    tb -> tb_acceptPfnx = TAccept;

    tb -> tb_writePfnx = TWrite;
    tb -> tb_readPfnx = TRead;
    tb -> tb_discPfnx = TDisconnect;
    tb -> tb_losePfnx = TLose;

#ifdef	NODELAY
    tb -> tb_drainPfnx = TDrain;
#endif

#ifdef  MGMT
    tb -> tb_manfnx = TManGen;
#endif
}
#endif
