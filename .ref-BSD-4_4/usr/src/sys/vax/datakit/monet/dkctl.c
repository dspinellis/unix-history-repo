/*
 * Datakit driver
 * Communication with control computer
 */

#include "datakit.h"
#if NDATAKIT>0

#include "param.h"
#include "../machine/pte.h"
#include "time.h"
#include "kernel.h"
#include "mbuf.h"
#include "errno.h"

#include "dkit.h"
#include "dkcmc.h"
#include "dk.h"
#include "dkdev.h"
#include "syslog.h"


extern char *dkfcanon() ;
extern char *dktcanon() ;

	static char	ProtocolVersion[] = S_HOSTVERSION;

/*
 *	configuration parameters
 */

	extern int			dk_nchan;
	extern struct dksetupreq	*dkreq[];

short	dkcactive = 0 ;
short	dkc1omplete = 0 ;
short	dkc2omplete = 0 ;
int	dkcstall = 0;
int	dkctldebug = 512;

#ifdef	RADIAN
int	commchan = 4;		/* Supervisory control channel */
#else
int	commchan = 1;		/* Supervisory control channel */
#endif


/*
 *	messages to send to control
 */
	static struct lmsg	closepack =	{T_CHG, D_CLOSE, 0, 0, 0, 0, 0};
	static struct lmsg	notify = 	{T_LSTNR, 0, 0, 0, 0, 0, 0};
	static struct lmsg	cmcmsg;
	static char		cmcinp[CMCSIZ];


dk_setup(chan, cmd, p1, p2, p3, p4, endfcn, endparm)
int (*endfcn)() ;
caddr_t endparm ;
{
register struct dksetupreq *req ;
struct mbuf *mb;
extern dkctime() ;
extern dkcin() ;

#ifdef lint
	printf("Datakit protocol version %s\n", ProtocolVersion);
#endif
	if (chan >= dk_nchan || dkreq[chan])
		return -1 ;
	mb = m_get(M_DONTWAIT, DKMT_PCB);
	if (mb == NULL)
		return ENOBUFS ;
	mb->m_len = sizeof (struct dksetupreq);
	req = mtod(mb, struct dksetupreq *) ;
	req->rq_msg.type = cmd>>8 ;
	req->rq_msg.srv = cmd & 0377 ;
	req->rq_msg.param0 = chan ;
	req->rq_msg.param1 = p1 ;
	req->rq_msg.param2 = p2 ;
	req->rq_msg.param3 = p3 ;
	req->rq_msg.param4 = p4 ;
	req->rq_endfcn = endfcn ;
	req->rq_endparm = endparm ;
	if (dk_status(commchan) & DK_RESET) {
		(void) dk_close(commchan) ;
		dk_free(commchan) ;
		(void) dk_open(commchan, (int (*)()) NULL) ;
		(void) dk_recv(commchan, (caddr_t)cmcinp, CMCSIZ, 0, dkcin, (caddr_t) 0) ;
	}
	if (dkcactive == 0) dk_ccsinit();
	if ((dkcactive > 0) && (dkfpack((caddr_t)&req->rq_msg) < 0)) {
		if (chan > dkctldebug)
			log(LOG_ERR, "dk_setup %d: %x fail\n", chan, cmd);
		(void) m_free(dtom(req)) ;
		return -1 ;
	}
	dkreq[chan] = req ;
	if (chan > dkctldebug) log(LOG_ERR, "dk_setup %d: %x ok\n", chan, cmd);
	return 0 ;
}

dk_ccsinit()
{
	if (dkcactive) return;

	(void) dk_open(commchan, (int (*)()) NULL) ;
	notify.srv = 1 ;
	notify.param1 = dk_nchan;	/* tell max channel number */
	notify.param2 = HOSTVERSION;	/* tell version of host software */
	(void) dkfpack((caddr_t)&notify) ;
	notify.srv = 0 ;
	dkctime() ;
	dkcactive = 1 ;
	(void) dk_recv(commchan, (caddr_t)cmcinp, CMCSIZ, 0, dkcin, (caddr_t) 0) ;
}

dk_takedown(chan)
{
	register s ;
	register struct dksetupreq *req ;
	extern struct dkdev	dkdev[];
	register struct	dkdev	*tp = &dkdev[chan];
	register struct mbuf *m;

	if (dkcactive ) {
		s = spl5() ;
		if (req = dkreq[chan]) {
			dkreq[chan] = NULL ;
			if (req->rq_endfcn)
				(*req->rq_endfcn)(req->rq_endparm, chan, 2, -1, -1, -1) ;
			(void) m_free(dtom(req)) ;
		}
		if (tp->d_ctype) {
			MFREE(dtom(tp->d_ctype), m);
			tp->d_ctype = NULL;
		}
		splx(s) ;
		if ((dkcactive > 0) && (dk_status(commchan) & DK_OPEN)) {
			closepack.param0 = chan ;
			(void) dkfpack((caddr_t)&closepack) ;
		}
	} else
		dk_free(chan) ;
	return 0 ;
}



dkctime()  
{
	register chan ;
	register struct dksetupreq *req ;
	extern dkcin() ;
	int s = spl5();

	if (dk_status(commchan) & DK_RESET) {
		(void) dk_close(commchan) ;
		dk_free(commchan) ;
		(void) dk_open(commchan, (int (*)()) NULL) ;
		(void) dk_recv(commchan, (caddr_t)cmcinp, CMCSIZ, 0, dkcin, (caddr_t) 0) ;
	}
	if (dkc1omplete == dkc2omplete) {
		dkcstall++;
		dkcactive = -1 ;
	}
	else {
		dkcstall = 0;
		dkcactive = 1 ;
	}
	dkc2omplete = dkc1omplete ;

	if (dkcstall > 2)
		dk_cmd(commchan, (DKC_FLUSH | DKC_XINIT));

	if ((dk_status(commchan) & (DK_BUSY|DK_OPEN)) == DK_OPEN) {
		for (chan = 2; chan < dk_nchan; chan++)   {
			if (dk_status(chan) & DK_LINGR) {
				closepack.param0 = chan ;
				(void) dkfpack((caddr_t) &closepack) ;
			}
			if (req = dkreq[chan])
				(void) dkfpack((caddr_t)&req->rq_msg) ;
		}
		notify.param1 = dk_nchan;
		(void) dkfpack((caddr_t)&notify) ;
		notify.srv = 0 ;
	}
	
	timeout(dkctime, (caddr_t) 0, 5*hz) ;
	splx(s);
	return;
}

dkfpack(msg)
caddr_t msg ;
{
struct mbuf *mb;
extern dkfdone() ;

	mb = m_get(M_DONTWAIT, DKMT_DATA);
	if (mb == NULL)
		return -1 ;
	(void) dktcanon(CMCFMT, msg, mtod(mb, char *));
	mb->m_len = CMCSIZ;
	if (dk_xmit(commchan, mb, 1, 0, dkfdone, (caddr_t) 0) == 0) {
		return -1 ;
	}
	return 0 ;
}


/*ARGSUSED*/
dkfdone(cb)
{
	dkc1omplete++ ;
	dkcstall = 0;
}



/*ARGSUSED*/
dkcin(param, channel, rlen, code, ctlchr)
{
register chan ;
register err ;
register struct dksetupreq *req ;

	if (code == DKR_ABORT) {
		if (0 > dkctldebug) log(LOG_ERR, "dkcin: abort\n");
		for (chan=2; chan < dk_nchan; chan++)
			if (req = dkreq[chan]) {
				dkreq[chan] = NULL ;
				if (req->rq_endfcn)
					(*req->rq_endfcn)(req->rq_endparm, chan, 2, -1, -1, -1) ;
				(void) m_free(dtom(req)) ;
			}
		return(0);;
	}
	(void) dkfcanon(CMCFMT, cmcinp, (char *)&cmcmsg) ;
	if (cmcmsg.param0 < dk_nchan)
		chan = cmcmsg.param0 ;
	else
		chan = 0 ;
	if (chan > dkctldebug)
		log(LOG_ERR, "dkcin %d: type %d srv %o\n", chan, cmcmsg.type,
		    cmcmsg.srv);
	switch (cmcmsg.type) {
	case T_REPLY:
		if ((req = dkreq[chan]) == NULL)
			break ;
		dkreq[chan] = NULL ;
		err = 0 ;
		if (cmcmsg.srv == D_FAIL)
			err = 1 ;
		if (req->rq_endfcn != NULL)
			(*req->rq_endfcn)(req->rq_endparm, cmcmsg.param0, err,
				cmcmsg.param1, cmcmsg.param2, cmcmsg.param3) ;
		(void) m_free(dtom(req)) ;
		if (cmcmsg.srv == D_OPEN)
			dk_cmd(chan, DKC_XINIT);
		break;
	case T_RESTART:
		/*
		 * If the Common Control has fewer channels configured than
		 * we do, use its number.
		 */
		if (chan > dkctldebug)
			log(LOG_ERR, "Datakit restart: channels = %d (%d)\n", dk_nchan, cmcmsg.param1);
		if(cmcmsg.param1 > 0 && cmcmsg.param1 < dk_nchan)
			dk_nchan = cmcmsg.param1;
		dk_cmd(chan, DKC_XINIT) ;
		break ;
	case T_CHG:
		switch (cmcmsg.srv) {
		case D_CLOSE:
			if (dk_status(chan) & DK_OPEN) {
				dk_reset(chan) ;
				dkabtreq(chan, 1, 0) ;
			} else {
				closepack.param0 = cmcmsg.param0 ;
				(void) dkfpack((caddr_t)&closepack) ;
				(void) dk_close(chan) ;
			}
			break ;
		case D_ISCLOSED:
			if (dk_status(chan) & DK_LINGR) {
				(void) dk_close(chan) ;
				dk_free(chan) ;
			}
			break ;
		case D_CLOSEALL:
			for (chan = 2; chan < dk_nchan; chan++)
				if (dk_status(chan) & DK_OPEN) {
					dk_reset(chan) ;
					dkabtreq(chan, 1, 0) ;
				}
			break;
		}
		break;
	case T_SRV:
		if (cmcmsg.srv == D_REINIT)
			dkrsplice(chan) ;
		break ;
	}
	(void) dk_recv(commchan, (caddr_t)cmcinp, CMCSIZ, 0, dkcin, (caddr_t) 0) ;
	return 0 ;
}

dk_splice(chan1, chan2, endfun, endp1, endp2)
int (*endfun) () ;
caddr_t endp1, endp2 ;
{
	static short splice = (T_CHG << 8 | D_SPLICE) ;

	if (dk_setup(chan1, splice, chan2, 0, 0, 0, endfun, endp1) ||
		dk_setup(chan2, splice, chan1, 0, 0, 0, endfun, endp2))
			return -1 ;
	else
		return 0 ;
}

/* If we are waiting on a setup request,
 * abort it.
 */
dkabtreq(chan, err, usrerr)
int chan;
int err, usrerr;
{
	register struct dksetupreq *req ;

	if((req = dkreq[chan]) != NULL) {
		dkreq[chan] = NULL ;
		if(req->rq_endfcn != NULL)
			(*req->rq_endfcn)(req->rq_endparm, chan, err, usrerr, 0, 0);
		(void) m_free(dtom(req)) ;
	}
}


#ifdef	pdp11
#define	SALIGN(p)	(char *)(((int)p+1) & ~1)
#define	LALIGN(p)	(char *)(((int)p+1) & ~1)
#endif
#ifdef	vax 
#define	SALIGN(p)	(char *)(((int)p+1) & ~1)
#define	LALIGN(p)	(char *)(((int)p+3) & ~3)
#endif
#ifdef	u3b
#define	SALIGN(p)	(char *)(((int)p+1) & ~1)
#define	LALIGN(p)	(char *)(((int)p+3) & ~3)
#endif
#define	SNEXT(p)	(char *)((int)p + sizeof (short))
#define	LNEXT(p)	(char *)((int)p + sizeof (long))


/*
 * convert from canonical to
 * local representation.
 */
char *
dkfcanon(fmt, from, to)
register char *fmt, *from, *to;
{
short tmp;
long ltmp;
	while (*fmt) {
		switch(*fmt++) {
		case 's':			/* short */
			tmp = 0;
			tmp = (*from++)&0377;
			tmp |= ((*from++)&0377)<<8;
			to = SALIGN(to);
			*(short *)to = tmp;
			to = SNEXT(to);
			continue;
		case 'l':			/* long */
			ltmp = 0;
			ltmp = (*from++)&0377;
			ltmp |= (long)((*from++)&0377)<<8;
			ltmp |= (long)((*from++)&0377)<<16;
			ltmp |= (long)((*from++)&0377)<<24;
			to = LALIGN(to);
			*(long *)to = ltmp;
			to = LNEXT(to);
			continue;
		case 'b':			/* byte */
			*to++ = *from++;
			continue;
		default:
			return((char *)0);
		}
	}
	return(from);
}

/*
 * convert from local to
 * canonical representation
 */
char *
dktcanon(fmt, from, to)
register char *fmt, *from, *to;
{
short tmp;
long ltmp;

	while (*fmt) {
		switch(*fmt++) {
		case 's':
			from = SALIGN(from);
			tmp = *(short *)from;
			from = SNEXT(from);
			*to++ = tmp;
			tmp >>=8;
			*to++ = tmp;
			continue;
		case 'l':
			from = LALIGN(from);
			ltmp = *(long *)from;
			*to++ = ltmp;
			ltmp >>= 8;
			*to++ = ltmp;
			ltmp >>= 8;
			*to++ = ltmp;
			ltmp >>= 8;
			*to++ = ltmp;
			from = LNEXT(from);
			continue;
		case 'b':
			*to++ = *from++;
			continue;
		default:
			return((char *)0);
		}
	}
	return(from);
}
#endif
