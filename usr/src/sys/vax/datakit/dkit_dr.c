/*
 * Datakit driver
 * DR11C version without KMC
 *
 * uses mbufs for transmission
 *
 *	SCCSID[] = "@(#)dkit_dr.c	1.5 Garage 84/04/11"
 */

#include "dkitdr.h"
#if NDKITDR>0
#include "datakit.h"

#include "../include/pte.h"
#include "sys/param.h"
#include "sys/time.h"
#include "sys/kernel.h"
#include "sys/buf.h"
#include "sys/mbuf.h"
#include "sys/errno.h"
#include "sys/socket.h"
#include "sys/syslog.h"
#include "net/if.h"
#include "../uba/ubareg.h"
#include "../uba/ubavar.h"

#include "dk.h"
#include "dkit.h"
#include "dkdr.h"


#define PKBHOG		64		/* max temp buffers per channel	*/
#define	DKNPKB	(200+4*NDATAKIT)
#define	DKNSTB	10

struct dkchan	dkit[NDATAKIT];
extern int	dk_nchan;

int	dkdr_npk	= DKNPKB;
struct dkpkbufr	dk_pkbuf[DKNPKB];

int	dkdr_nstat	= DKNSTB;
struct dkstat	dkdr_stat[DKNSTB];

int	dkattached = 0;			/* Is it really there? */

#ifdef	KALYPSO
#define	URPDEBUG	5000
#else
#define	URPDEBUG	500
#endif

#ifdef	URPDEBUG
int	dkurpdebug = 0;
#define	URPTRACE(chan, chr, Dkp)	if (dkurpdebug == (chan)) \
	dkurptrace(chr, Dkp);
#endif

/*
 * structure of data in first mbuf on chain (type DKM_HDR)
 *
 */
struct	mpacket {
	short	mp_len;		/* Total length left */
	char	mp_ctl;		/* Control character */
	int	mp_eob;		/* Send end-of-block indicator */
	int	(*mp_endfcn)();	/* End-action function */
	caddr_t	mp_endparm;	/* Parameter to above function */
};

/*
 *	dr11-c bit definitions
 */
#define	DKTENAB	0100	/* transmit interrupt enable */
#define	DKRENAB	040	/* receiver interrupt enable */
#define	ENABS	0140	/* both enables */
#define	DKCOM	03	/* dr11-c command bits */
#define	DKTDONE	0200	/* transmit done bit */
#define	DKRDONE	0100000	/* receiver done bit */
#define	DKMARK	01000	/* start of packet bit */
#define	DKOVF	040000	/* receiver overflow bit (in drin) */
#define	DKDATA	0400	/* bit 9 ... indicates non-control */

#define DKCHUNK	16	/* packet size */

/* 
 * dr11c commands
 */
#define D_OSEQ  0
#define D_READ  1
#define D_WRITE 2
#define D_XPACK 3

/*
 *   error control protocol definitions
 */
#define	SEQ		0010	/* 8 sequence numbers to end trailers */
#define	ECHO		0020	/* 8 echoes, data given to host */
#define	REJ		0030	/* 8 rejections, transmission error */
#define	ACK		0040	/* first of 8 acks, correct reception */
#define	BOT		0050	/* normal beginning of trailer */
#define	BOTM		0051	/* trailer with more data to follow */
#define	BOTS		0052	/* seq update algorithm on this trailer */
#define	SOI		0053	/* start of interrupt trailer */
#define	EOI		0054	/* end of interrupt trailer */
#define	ENQ		0055	/* xmitter request flow/error status */
#define	CHECK		0056	/* xmitter request error status */
#define	INITREQ		0057	/* request initialization */
#define	INIT0		0060	/* disable trailer processing */
#define	INIT1		0061	/* enable trailer processing */
#define	AINIT		0062	/* response to INIT0/INIT1 */

#define	DKBMASK		03	/* this xmitter has window size of 4,  */
/* #define	DKBLOCK		60	/* each message is 60 bytes            */
#define	DKBLOCK		28	/* each message is 60 bytes            */


/*
 *   some commonly used macros
 */



	struct dkpkbufr		*dk_Sfree;
	extern int		dkdr_npk;
	extern struct dkpkbufr	dk_pkbuf[];

int	dknopkb = 1 ;		/* Counter for 'no dkpkbufr' condition.	*/
int dkstray;	/* number of stray interrupts since last timeout */
int	dkdrlostint;	/* Number of lost receiver interrupts */
int dkdisabled;	/* flag to indicate that DK interface has been disabled
			due to stray interrupts, etc. */
#define MAX_STRAY	10	/* maximum number of stray interrupts
				before temporarily disabling DK interrupts */

/*
 * dr11c device registers
 */
struct rdevice {
	short	dkcsr;
	short	dko;
	short	dki;
};

	extern int		dkdr_nstat;
	extern struct dkstat	dkdr_stat[];

	static char		Hstat, Tstat;

#define	DKADDR	((struct rdevice *) dkitdrdinfo[0]->ui_addr)


/*
 * Intermediate level command codes
 */
#define	KS_SEND		20
#define	KS_RDB		21
#define	KS_EOI		22
#define	KS_CNTL		23
#define	KS_ERR		24



int	dkdebug = 512 ;
int	dkactive ;
 
static int	timeron;


int	dkitdrprobe(), dkitdrattach();
struct	uba_device *dkitdrdinfo[1];
u_short	dkitdrstd[] = { 0 };
struct	uba_driver dkitdrdriver =
	{ dkitdrprobe, 0, dkitdrattach, 0, dkitdrstd, "dkitdr", dkitdrdinfo };

dkitdrprobe(reg)
caddr_t	reg;
{
	register int	br, cvec;		/* value-result */
	register struct rdevice	*draddr = (struct rdevice *) reg;
	register int i, c;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	dkdrrint(0); dkdrxint(0);
#endif
	draddr->dkcsr = D_READ;
	for (i = 0; i < 1024; i++)
		if (draddr->dkcsr & DKRDONE) c = draddr->dki;
		else break;
#ifdef lint
	c = c;
#endif
	draddr->dkcsr = D_WRITE;
	draddr->dko = DKMARK | (dk_nchan-1);	/* pack on 511 */
	draddr->dkcsr = D_XPACK + DKTENAB;
	draddr->dko = 0;
	DELAY(10000);
	draddr->dkcsr = 0;
	return(sizeof(struct rdevice));
}

/*ARGSUSED*/
dkitdrattach(ui)
	struct uba_device	*ui;
{
	dkattached = 1;

#if	defined(INET) && NDKI>0
	dkiattach();
#endif
}


static
dk_init()
{
register struct rdevice *raddr = DKADDR;
register s ;
	/*
	 *  At attach time for the hardware device
	 *  initialize and check things out to the
	 *  (grumble) limited extent that is possible.
	 */

	s = spl5() ;
	Hstat = Tstat = 0 ;
	dkdisabled = 0;
	dkstray = 0;
	{
		register struct dkchan *dkp ;

		for (dkp = &dkit[0]; dkp < &dkit[dk_nchan]; dkp++) {
			dkp->dk_rlen = 0 ;
			dkp->dk_xlen = 0 ;
			dkp->dk_X = XM_OFF;
			dkp->dk_rq = NULL ;
			dkp->dk_outq.ifq_len = 0;
			dkp->dk_outq.ifq_maxlen = 20;
			dkp->dk_outq.ifq_drops = 0;
			dkp->dk_outq.ifq_head = NULL;
			dkp->dk_outq.ifq_tail = NULL;
		}
	} {	register struct dkpkbufr *pkb ;

		for (pkb = &dk_pkbuf[1]; pkb < &dk_pkbuf[dkdr_npk-1]; pkb++) {
			pkb->Pnext = pkb + 1 ;
		}
		dk_pkbuf[dkdr_npk-1].Pnext = NULL ;
		dk_Sfree = &dk_pkbuf[1] ;
		dk_pkbuf[0].Pnext = NULL ;
	} {	
		register int seq, i, c ;

		raddr->dkcsr = D_OSEQ ;
		raddr->dko = 0 ;		/* clears all FIFO's */
		seq = 0 ;
		while (raddr->dkcsr & DKTDONE) {
			seq += (((raddr->dki)>>10) & 017) + 2 ;
			if (seq > 100) {
				dkreport(KS_ERR, 0, 1, 0, 1) ;
				splx(s) ;
				return -EIO ;
			}
		}
		raddr->dkcsr = D_READ;
		for (i = 0; i < 1024; i++)
			if (raddr->dkcsr & DKRDONE) c = raddr->dki;
			else break;
#ifdef lint
		c = c;
#endif
	}
	raddr->dkcsr = ENABS ;
	if(!timeron){
		dk_timeout();
		timeron++;
	}

	splx(s) ;
	dkactive = 1 ;


	return 0 ;
}

/*ARGSUSED*/
dkitreset(uban)
	int uban;
{
register struct rdevice *raddr ;

	raddr = DKADDR;
	raddr->dkcsr = ENABS;
	log(LOG_ERR, " dkit_dr%d", 0);
}

dk_open(chan, supfcn)
register chan ;
int (*supfcn)() ;
{
	register struct	dkchan	*dkp;
	register	s ;
	extern 	dkkint() ;
	static	firsttime = 1;
	static	init;
	extern int commchan;

	if (chan >= dk_nchan || !dkattached)
		return -ENXIO ;
	if (firsttime) {
		if ((init = dk_init()) < 0) return init;
		firsttime = 0;
	}
	dkp = &dkit[chan] ;
	s = spl5() ;
	/*
	 * Channel 0 (0-3 in ISN) is reserved for maintenance.
	 * An open on channel 0 is interpreted as a request
	 * for an unused channel.  Channel 1 (4 in ISN or RADIAN)
	 * is the common supervisory channel.
	 */
	if (chan == 0) {
		chan = commchan+1 ;		/* Start above commchan */
		while (1) {
			dkp = &dkit[chan] ;
			if (dkp->dk_state == 0)
				break ;
			chan++ ;
			if (chan >= dk_nchan) {
				splx(s) ;
				return -EADDRNOTAVAIL ;
			}
		}
	}


	/*
	 * Finish setting up dkp struct.
	 */
	if ((dkp->dk_state & DK_OPEN) ==0) {
		if (chan > dkdebug)
			log(LOG_ERR, "dkopen %d: %x\n", chan, supfcn) ;
		dkp->dk_S = 1 ;
		dkp->dk_R = 0 ;
		dkp->dk_X = 0 ;
		dkp->dk_A = 0 ;
		dkp->dk_rejcnt = 0;
		dkp->dk_srejcnt = 0;
		dkp->dk_ackrejcnt = 0;
		dkp->dk_enqcnt = 0;
		dksend(chan, INIT1) ;
		flushall(dkp, 0);
		dkp->dk_state &= ~DK_LINGR ;
		dkp->dk_state |= DK_OPEN;
	}
	dkp->dk_supfcn = supfcn ;
	splx(s) ;
	return chan ;
}


/*
 * Close a channel:
 */

dk_close(chan)
{
register struct	dkchan	*dkp;
register s ;

	s = spl5() ;
	if (chan > dkdebug)
		log(LOG_ERR, "dkclose %d\n", chan) ;
	dkp = &dkit[chan] ;
	if (chan == 0) {
		if (!dkattached) return -ENXIO;
		for (dkp = &dkit[1]; dkp < &dkit[dk_nchan]; dkp++) {
			if (dkp->dk_state & (DK_OPEN|DK_BUSY|DK_RCV)) {
				dkp->dk_state |= DK_RESET ;
				flushall(dkp, 0) ;
			}
		}
		dkactive = 0 ;
		splx(s);
		return dk_init() ;
	} else {
		dkp->dk_state |= DK_OPEN ;
		flushall(dkp, 0) ;
		dkp->dk_state = DK_LINGR ;
		dkp->dk_X = XM_OFF;
		dkp->dk_trmode = 0 ;
	}
	splx(s) ;
	return 0;
}

/*
 *	Close phase 2 - mark available for reassignment
 */
dk_free(chan)
{
	if (chan > dkdebug)
		log(LOG_ERR, "dkfree %d\n", chan) ;
	dkit[chan].dk_state &= ~DK_LINGR ;
}


/*
 *	Reset a channel
 *	 prevents further I/O until close
 */
dk_reset(chan)
{
register struct dkchan *dkp ;
register s ;

	if (chan > dkdebug)
		log(LOG_ERR, "dkreset %d\n", chan) ;
	s = spl5() ;
	dkp = &dkit[chan] ;
	dkp->dk_state |= DK_RESET ;
	flushall(dkp, 0) ;
	splx(s) ;
}



/*
 *	Xmit a short control (interrupt) packet
 */
dk_xint(chan, intr)
{
register struct rdevice *raddr ;
register s ;
register struct dkchan *dkp ;

	dkp = &dkit[chan] ;
	if (chan == 0 || dkp->dk_X < XM_INIT)
		return -1 ;
	s = spl5() ;
	if (chan > dkdebug)
		log(LOG_ERR, "dkxint %d: %o %o\n", chan, (intr & 0377), ((intr >>8)&0377)) ;
	raddr = DKADDR ;
	raddr->dkcsr = D_WRITE ;
	raddr->dko = chan | DKMARK ;
	raddr->dko = SOI ;
	raddr->dko = (intr & 0377) | DKDATA ;
	raddr->dko = ((intr >> 8) & 0377) | DKDATA ;
	raddr->dko = EOI ;
	raddr->dkcsr = D_XPACK ;
	raddr->dko = 0 ;
	if(dkdisabled)
		raddr->dko = 0;
	else
		raddr->dko = ENABS;
	splx(s) ;
	return 0 ;
}


/*
 * Adjust window size
 */
dk_winsize(chan, win)
	struct diocxwin win;
{
	return EINVAL;		/* For now... */
}


/*
 * Xmit data on a channel
 *   NOTE * * * * *
 *	Although it is never checked here, buffer addresses
 *    in this version of the driver must be kernel addressable.
 */
dk_xmit(chan, m, eob, ctlchar, endfcn, endparm)
struct mbuf *m ;
int (*endfcn)() ;
caddr_t endparm ;
{
	register struct dkchan *dkp ;
	register struct mpacket *mbp ;
	register struct mbuf *mb;
	int s ;

	s = spl5() ;
	dkp = &dkit[chan] ;
	if ((dkp->dk_state & DK_RESET) || (mb = m_get(M_DONTWAIT,DKMT_HDR)) == NULL) {
		m_freem(m);
		splx(s) ;
		return 0 ;
	}

	if (ctlchar == '\001') eob = 0;
	mb->m_len = 0;
	mbp = mtod(mb, struct mpacket *);
	mbp->mp_endfcn = endfcn ;
	mbp->mp_endparm = endparm ;
	mbp->mp_eob = eob;
	mb->m_next = m;

	mbp->mp_len = 0;
	while (m) {
#ifdef notdef
		if (m->m_type != DKMT_DATA && m->m_type != DKMT_CTL) {
			log(LOG_ERR, "dk_xmit %d: got type %x\n", chan, m->m_type);
			m_freem(mb);
			return 0;
		}
#endif
		mbp->mp_len += m->m_len;
		m = m->m_next;
	}

	if ((ctlchar & 0300) == 0100) {
		register struct mbuf *n = mb, *mc;

		mc = m_get(M_DONTWAIT, DKMT_CTL);
		if (mc == NULL) {
			m_freem(mb);
			splx(s);
			return 0;
		}
		*mtod(mc, char *) = ctlchar;
		mc->m_len = 1;

		/* Append it -- can't use m_cat because type field counts */
		while (n->m_next) n = n->m_next;
		n->m_next = mc;
		mbp->mp_len++;
		ctlchar = 0;	
	}
	mbp->mp_ctl = ctlchar;

	if ((dkp->dk_state & DK_BUSY) == 0) {
		dkp->dk_state |= DK_BUSY ;
		dkp->dk_curout = mb;
		dkp->dk_xlen = mbp->mp_len ;
		if (chan > dkdebug)
			log(LOG_ERR, "xmit %d: %x len %d\n", chan,
			   mb->m_next, mbp->mp_len) ;
		dkxmit(dkp, chan, 2) ;
		splx(s) ;
		return dkp->dk_state ;
	}
	if (IF_QFULL(&dkp->dk_outq)) {
		IF_DROP(&dkp->dk_outq);
		m_freem(mb);
	}
	else
		IF_ENQUEUE(&dkp->dk_outq, mb);
	splx(s) ;
	return dkp->dk_state ;
}

/*
 * Receive into a block buffer
 */
dk_recv(chan, addr, len, mode, endfcn, endparm)
caddr_t addr ;
int (*endfcn)() ;
caddr_t endparm ;
{
register struct dkchan *dkp ;
register s ;

	if (addr == 0) {
		log(LOG_ERR, "dk_recv: channel %d endfcn %x: invalid address specification\n", chan, endfcn);
		return 0;
	}

	s = spl5() ;
	dkp = &dkit[chan] ;
	if (dkp->dk_state & (DK_RCV | DK_RESET)) {
		splx(s) ;
		return 0 ;
	}
	dkp->dk_state |= DK_RCV ;
	dkp->dk_endfcn = endfcn ;
	dkp->dk_endparm = endparm ;
	dkp->dk_rmode = mode ;
	dkp->dk_rlen = len ;
	dkp->dk_raddr = (caddr_t)addr ;
	if (chan > dkdebug)
		log(LOG_ERR, "dkrecv %d: %x len %d mode %o\n", chan, (caddr_t)addr, len, mode) ;
	dkrcv(dkp, chan, 2) ;
	splx(s) ;
	return dkp->dk_state ;
}


dk_rabort(chan, nendfcn, nendparm)
int (*nendfcn)() ;
caddr_t nendparm ;
{
register struct dkchan *dkp ;
register s ;

	dkp = &dkit[chan] ;
	s = spl5() ;
	if (dkp->dk_state & DK_RCV) {
		dkp->dk_state &= ~DK_RCV ;
		if (dkp->dk_rlen) {
			if (chan > dkdebug)
				log(LOG_ERR, "rcvabo %d: rlen %d\n", chan, dkp->dk_rlen) ;
			(*nendfcn)(nendparm, chan, dkp->dk_rlen, DKR_ABORT, 0) ;
		}
		dkp->dk_rlen = 0 ;
	}
	splx(s) ;
	return dkp->dk_state ;
}



dk_status(chan)
{
	if (chan >= dk_nchan)
		return 0 ;
	return dkit[chan].dk_state ;
}


dk_timeout()
{
	register struct rdevice *raddr;
	register struct	dkchan	*dkp;
	register chan ;
	int s = spl5();

	chan = 0 ;
	for (dkp = &dkit[0]; dkp < &dkit[dk_nchan]; dkp++) {
		if (dkp->dk_X != XM_OFF) {
			if (dkp->dk_X == 0)
				dksend(chan, INIT1) ;
			else
			if (dkp->dk_S != ((dkp->dk_R + 1) & 07)) {
				if (dkp->dk_X & XM_ENQ) {
					dksend(chan, ENQ) ;
					dkp->dk_X &= ~XM_REJ ;
					dkp->dk_enqcnt++;
					URPTRACE(chan, ENQ, dkp);
				}
				else dkp->dk_X |= XM_ENQ;
			}
		}
		chan++ ;
	}

	dkstray = 0;
	if(dkdisabled){
		if(dkdisabled++ > 10){
			/* try re-enabling interrupts */
			dkdisabled = 0;
			log(LOG_ERR, "re-enabling DK interface\n");
			raddr = DKADDR;
			raddr->dkcsr = ENABS;
		}
	}
	else {
		/* Look for lost interrupts */
		if (raddr->dkcsr < 0) {
			dkdrlostint++;
			dkdrxint(0);
		}
	}

	timeout(dk_timeout, (caddr_t) 0, 1*hz);
	splx(s);
}

dk_cmd(chan, cmd)
{
	register struct dkchan *dkp ;
	int s = spl5();

	if (chan > dkdebug)
		log(LOG_ERR, "dkcmd %d: %o\n", chan, cmd) ;
	dkp = &dkit[chan] ;
	if (cmd & DKC_XINIT) {
		dkp->dk_X = dkp->dk_R = dkp->dk_A = 0 ;
		dkp->dk_S = 1 ;
		dksend(chan, INIT1) ;
	}
	if (cmd & DKC_FLUSH) {
		flushall(dkp, -1) ;
	}
	if (cmd & DKC_SPND)
		dkp->dk_state |= DK_SPND ;
	if (cmd & DKC_RSME) {
		dkp->dk_state &= ~DK_SPND ;
		dkxmit(dkp, chan, 2) ;
	}
	splx(s);
}


static
flushall(dkp, rwflag)
register struct dkchan *dkp ;
{
register s ;
struct mpacket *mbp;

	s = spl5() ;
	if ((dkp->dk_state & DK_RCV) && (rwflag >= 0)) {
		dkp->dk_state &= ~DK_RCV ;
		if (dkp->dk_endfcn)
			(*dkp->dk_endfcn)(dkp->dk_endparm, dkp-dkit, dkp->dk_rlen, DKR_ABORT, 0) ;
		dkp->dk_rlen = 0 ;
	}
	if ((dkp->dk_state & DK_BUSY) && (rwflag <= 0)) {
		register struct mbuf *m ;

		dkp->dk_xlen = 0;
		if (dkp->dk_curout) {
			mbp = mtod(dkp->dk_curout, struct mpacket *);
			if (mbp->mp_endfcn)
				(mbp->mp_endfcn)(mbp->mp_endparm, dkp-dkit);
			m_freem(dkp->dk_curout);
			dkp->dk_curout = NULL;
		}
		while (1) {
			IF_DEQUEUE(&dkp->dk_outq, m);
			if (!m) break;
			mbp = mtod(m, struct mpacket *);
			if (mbp->mp_endfcn)
				(mbp->mp_endfcn)(mbp->mp_endparm, dkp-dkit);
			m_freem(m);
		}
		dkp->dk_state &= ~DK_BUSY ;
	}
	if ((dkp->dk_state & DK_OPEN) && (rwflag >= 0)) {
		register struct dkpkbufr *dbp ;

		dkp->dk_rlen = 0 ;
		dkp->dk_xlen = 0 ;
		dkp->dk_C = 0 ;
		while (dbp = dkp->dk_rq) {
			dkp->dk_rq = dbp->Pnext ;
			dbp->Pnext = dk_Sfree ;
			dk_Sfree = dbp ;
		}
		while (dbp = dkp->dk_rb) {
			dkp->dk_rb = dbp->Pnext ;
			dbp->Pnext = dk_Sfree ;
			dk_Sfree = dbp ;
		}
	}
	splx(s) ;
}


/*
 * Routine to handle completion status
 */
static
dkkint()
{
register struct dkchan *dkp;
register struct dkstat *sp;
register chan;
struct mbuf *m ,*om;
struct mpacket *mbp;
static char *cmpltype[] = {"send", "rdb", "eoi", "cntl", "err"};

	while (Tstat != Hstat) {
		sp = &dkdr_stat[Hstat];
		chan = sp->k_chan;
		dkp = &dkit[chan];
		if (sp->k_chan > dkdebug) {
			if (sp->k_type >= KS_SEND && sp->k_type <= KS_ERR)
				log(LOG_ERR, "dkdone:  type %s chan %d info %o-%o\n",
				    cmpltype[sp->k_type-KS_SEND],
				    sp->k_chan, sp->k_info1, sp->k_info2) ;
			else log(LOG_ERR, "dkdone:  type %d chan %d info %o-%o\n",
			    sp->k_type, sp->k_chan, sp->k_info1, sp->k_info2) ;
		}
		if (Hstat==dkdr_nstat-1)  Hstat=0; else Hstat++;
		switch(sp->k_type) {
		case KS_CNTL:
			if (dkp->dk_supfcn)
				(*dkp->dk_supfcn)(chan, sp->k_info1) ;
			break ;
		case KS_EOI:
			break ;
		case KS_SEND:
			om = dkp->dk_curout ;
			if (om == NULL) {
				log(LOG_ERR, "dkk: xbufout\n");
				break;
			}
			IF_DEQUEUE(&dkp->dk_outq, m);
			if (m == NULL) {
				dkp->dk_state &= ~DK_BUSY;
				dkp->dk_curout = NULL;
			} else {
				dkp->dk_curout = m;
				mbp = mtod(m, struct mpacket *);
				dkp->dk_xlen = mbp->mp_len ;
				if (chan > dkdebug)
					log(LOG_ERR, "xmiti %d: %x len %d\n", chan,
					   m->m_next, mbp->mp_len) ;
				dkxmit(dkp, chan, 0) ;
			}
			mbp = mtod(om, struct mpacket *);
			if (mbp->mp_endfcn != NULL)
				(mbp->mp_endfcn)(mbp->mp_endparm, chan) ;
			m_freem(om);
			break;
		case KS_RDB:
			dkp->dk_state &= ~(DK_RCV|DK_RCVQ) ;
			if (sp->k_info2 == DKR_TIME && dkp->dk_rlen == 0)
				break ;		/* another coming later */
			if (dkp->dk_rlen) {
				sp->k_info1 = dkp->dk_rlen ;
				dkp->dk_rlen = 0 ;
			}
			if (dkp->dk_endfcn != NULL)
				(*dkp->dk_endfcn)(dkp->dk_endparm, dkp-dkit, sp->k_info1, sp->k_info2&0377,
						(sp->k_info2>>8)&0377) ;
			break;
		case KS_ERR:
			log(LOG_ERR, "err in dkit.c: chan - %d, code - %o\n",
				chan, sp->k_info1);
			break;
		}	/* end switch */

	}	/* end while */
}
 
 
 




/* static */
int	dkxmitpanic = 0;

dkxmit(dkp, stechan, intrpt)
	struct dkchan *dkp ;
{
	register struct rdevice *raddr ;
	register char *ptr ;
	register struct mbuf *m;
	register int wtype;
	short pklen ;
	short mlen, unacked ;
	short blklen ;
	unsigned short totlen ;
	struct mpacket *mbp ;
#ifdef notdef
	short scheck ;
#endif

	if( dkp->dk_curout == NULL || stechan ==0 )
		return ;
	mbp = mtod(dkp->dk_curout, struct mpacket *);
	raddr = DKADDR ;
	if ((dkp->dk_S & DKBMASK) == (dkp->dk_R & DKBMASK) || (dkp->dk_state & DK_SPND))
		goto ctlchk ;
	if ((dkp->dk_xlen || ((mbp->mp_ctl & 0200) == 0)) && dkp->dk_X < XM_INIT)
		goto ctlchk ;
#ifdef notdef
	if ((dkp->dk_S & DKBMASK) == ((dkp->dk_R + 1) & DKBMASK))
		scheck = 0 ;
	else
		scheck = 1 ;
#endif

	unacked = ((dkp->dk_S - dkp->dk_A - 1) & 07) * DKBLOCK ;
	mlen = MIN(unacked, dkp->dk_xlen) ;
	totlen = dkp->dk_xlen - mlen;
	if (totlen == 0)
		goto ctlchk ;

	/* Skip over stuff sent but not acked */
	for (m = dkp->dk_curout->m_next; m && (mlen > 0); m = m->m_next)
		if (mlen > m->m_len) mlen -= m->m_len;
		else break;

	while (totlen && ((dkp->dk_S ^ dkp->dk_R) & DKBMASK)) {
		if (dkxmitpanic) panic("dkxmit -- panic 1");
		blklen = MIN (totlen, DKBLOCK) ;
		pklen = 0 ;
		raddr->dkcsr = D_WRITE ;
		raddr->dko = stechan | DKMARK ;
		while (blklen) {
			if (dkxmitpanic) panic("dkxmit -- panic 2");
			if (m == NULL) panic("dkxmit mlen");
			ptr = mtod(m, char *) + mlen;
			mlen = MIN(blklen, m->m_len - mlen);
			blklen -= mlen;
			wtype = (m->m_type == DKMT_CTL ? 0 : DKDATA);
			while (mlen--) {
				if (dkxmitpanic) panic("dkxmit -- panic 3");
				raddr->dko = (*ptr++ & 0377) | wtype ;
				pklen++ ;
				if ((pklen & (DKCHUNK-1)) == 0) {
					raddr->dkcsr = D_XPACK ;
					raddr->dko = 0 ;
					raddr->dkcsr = D_WRITE ;
					raddr->dko = stechan|DKMARK ;
				}
			}
			if (ptr == (mtod(m, char *) + m->m_len)) {
				m = m->m_next;
				mlen = 0;
			}
			else mlen = ptr - mtod(m, char *);
		}
		blklen = MIN (totlen, DKBLOCK) ;
		if ((pklen & (DKCHUNK-1)) > (DKCHUNK-4)) {
			raddr->dkcsr = D_XPACK ;
			raddr->dko = 0 ;
			raddr->dkcsr = D_WRITE ;
			raddr->dko = stechan|DKMARK ;
		}
		if (blklen == totlen && mbp->mp_eob)
			raddr->dko = BOT ;
		else
			raddr->dko = BOTM ;
		raddr->dko = (blklen & 0377) | DKDATA ;
		raddr->dko = ((blklen>>8) & 0377) | DKDATA ;
		raddr->dko = SEQ + dkp->dk_S ;
		raddr->dkcsr = D_XPACK ;
		raddr->dko = 0 ;
		URPTRACE(stechan, SEQ + dkp->dk_S, dkp);
		dkp->dk_S++ ;
		dkp->dk_S &= 07 ;
		totlen -= blklen ;
	}
#ifdef notdef
	if (totlen == 0 && dkp->dk_xlen && scheck) {
		raddr->dkcsr = D_WRITE ;
		raddr->dko = stechan|DKMARK ;
		raddr->dko = CHECK ;
		raddr->dkcsr = D_XPACK ;
		raddr->dko = 0 ;
		URPTRACE(stechan, CHECK, dkp);
	}
#endif
ctlchk:
	if (mbp->mp_ctl & 0200) {
		raddr->dkcsr = D_WRITE ;
		raddr->dko = stechan|DKMARK ;
		raddr->dko = mbp->mp_ctl & 0377 ;
		raddr->dkcsr = D_XPACK ;
		raddr->dko = 0 ;
		mbp->mp_ctl = 0 ;
		if (dkp->dk_xlen == 0)
			dkreport(KS_SEND, stechan, 0, 0, intrpt) ;
	}
	if(dkdisabled)
		raddr->dkcsr = 0;
	else
		raddr->dkcsr = ENABS ;
}


static
dkrcv(dkp, stechan, intrpt)
struct dkchan *dkp ;
{
register char *ptr1 ;
register char *ptr2 ;
register len ;
short final ;
short hibits ;
struct dkpkbufr *pkb ;
short tlen ;

	if ((dkp->dk_rlen == 0) || (dkp->dk_rq == NULL))
		return ;
	final = 0 ;
	tlen = 0 ;
	while (final == 0 && (pkb = dkp->dk_rq)) {
		if (dkp->dk_rlen == 0)
			final |= DKR_FULL ;
		ptr1 = &pkb->Pdata[0] ;
		ptr2 = dkp->dk_raddr ;
		len = MIN(pkb->Plen, dkp->dk_rlen) ;
		hibits = pkb->Phibits ;
		while (len--) {
			if (hibits < 0)
				break ;
			hibits <<= 1 ;
			*ptr2++ = *ptr1++ ;
		}
		len = ptr2 - dkp->dk_raddr ;
		tlen += len ;
		dkp->dk_rlen -= len ;
		dkp->dk_raddr = ptr2 ;
		if ((pkb->Plen -= len) && hibits < 0) {
			final |= ((*ptr1++ & 0377) << 8) | DKR_CNTL ;
			hibits <<= 1 ;
			pkb->Plen-- ;
		}
		if (len = pkb->Plen) {
			ptr2 = &pkb->Pdata[0] ;
			while (len--)
				*ptr2++ = *ptr1++ ;
			pkb->Phibits = hibits ;
		}
		while (pkb && (pkb->Plen == 0)) {
			if ((pkb->Pseq & 0370) == ACK) {
				pkb->Pseq += ECHO - ACK ;
				final |= dkp->dk_rmode & DKR_BLOCK ;
			}
			if (pkb->Pseq) {
				dksend(stechan, pkb->Pseq) ;
				dkp->dk_C = pkb->Pseq ;
			}
			dkp->dk_rq = pkb->Pnext ;
			pkb->Pnext = dk_Sfree ;
			dk_Sfree = pkb ;
			pkb = dkp->dk_rq ;
		}
	}
	if (tlen && (dkp->dk_rmode & DKR_TIME))
		final |= DKR_TIME ;
	if (dkp->dk_rlen == 0)
		final |= DKR_FULL ;
	if (final && (final != DKR_TIME || ((dkp->dk_state & DK_RCVQ) == 0))) {
		dkp->dk_state |= DK_RCVQ ;
		len = dkp->dk_rlen ;
		if (final != DKR_TIME)
			dkp->dk_rlen = 0 ;
		dkreport(KS_RDB, stechan, len, final, (final == DKR_TIME)?2:intrpt) ;
	}
}



static
dksend(stechan, val)
{
register struct rdevice *raddr ;
register savcsr ;

	if(stechan == 0)
		return;

	raddr = DKADDR ;
	savcsr = raddr->dkcsr ;
	raddr->dkcsr = D_WRITE ;
	raddr->dko = stechan| DKMARK ;
	raddr->dko = val ;
	raddr->dkcsr = D_XPACK ;
	raddr->dko = 0 ;
	raddr->dkcsr = savcsr ;
}


/*ARGSUSED*/
dkdrrint(dev)	/* ?? */		/* needed for UNIX OS */
{
register struct rdevice *raddr ;
register c ;
register cnt;

#ifdef lint
	c = 0; cnt = c;
#endif
	cnt = 0;
	raddr = DKADDR ;
	raddr->dkcsr = D_OSEQ ;
	if((raddr->dkcsr & DKTDONE) == 0){
		if(dkstray++ >= MAX_STRAY){
			log(LOG_ERR, "DK err 1 (Cabling?)\n");
			raddr->dkcsr = 0;
			dkdisabled = 1;
			return;
		}
	}
	while (raddr->dkcsr & DKTDONE){
		c = raddr->dki ;
		if(cnt++ > 65){
			log(LOG_ERR, "DK err 2 (Cabling?)\n");
			raddr->dkcsr = 0;
			dkdisabled = 1;
			return;
		}
	}
	raddr->dkcsr = ENABS ;
	if (Hstat != Tstat)
		dkkint() ;
}

/*ARGSUSED*/
dkdrxint(dev)	/* ?? */
{
register struct rdevice *raddr ;
register char *ptr ;
struct dkchan  *dkp ;
register c ;
short badpacks ;
struct dkpkbufr *pkb ;
short stechan ;
short	len, xlen ;
unsigned short bitloc ;

	badpacks = 0 ;
	raddr = DKADDR ;
	pkb = NULL ;
	if(raddr->dkcsr >= 0){
		if(dkstray++ > MAX_STRAY){
			log(LOG_ERR, "DK err 3 (Cabling?)\n");
			goto disable;
		}
	}
	while (raddr->dkcsr < 0) {
		raddr->dkcsr = D_READ ;
		c = raddr->dki ;
		while (raddr->dkcsr < 0 && (c & DKMARK)) {
			c &= 0777 ;
			if (c >= dk_nchan) {
				if (++badpacks > 20) {
					log(LOG_ERR, "DK err 4 (Cabling?)\n");
					dkreport(KS_ERR, 0, 2, 0, 1) ;
					goto disable ;
				}
				break ;
			}
 			/* discard all chl 0 packets;
			 * the LDI version of the CPM-DR and CPM-422
			 * puts out packets on chl 0 occasionally.
			 */
			if(c == 0) break;

			dkp = &dkit[c] ;
			stechan = c ;
qpkb:
			if (pkb && pkb->Plen) {
				dkrcvq(stechan, dkp, pkb, 0) ;
				pkb = NULL ;
			}
			if (pkb == NULL) {
				if ((pkb = dk_Sfree) != NULL) {
					dk_Sfree = pkb->Pnext ;
					pkb->Pseq = 0 ;
					pkb->Plen = 0 ;
					pkb->Pnext = NULL ;
					pkb->Phibits = 0 ;
				} else {
					/*
					** Oops, no more dkpkbufr's.
					** Let outer loop gobble up
					** the entire packet.
					** Report to the console
					** every 100th occurrence.
					*/
					if ( dknopkb++ >= 100 ) {
						dknopkb = 1 ;
						dkreport(KS_ERR, 0, 3, 0, 1) ;
					}
					break ;
				}
			}
			raddr->dkcsr = D_READ ;
			ptr = &pkb->Pdata[0] ;
			bitloc = 0100000 ;
			while (raddr->dkcsr < 0 && ((c = raddr->dki) & DKMARK) == 0)
			switch (c = c & 0777) {
			case NULL:
				break ;
			case AINIT:
				dkp->dk_X = XM_INIT ;
				dkp->dk_R = 0 ;
				dkp->dk_S = 1 ;
				dkp->dk_A = 0 ;
				dkxmit(dkp, stechan, 1) ;
				raddr->dkcsr = D_READ ;
				break ;
			case INIT0:
				dksend(stechan, AINIT) ;
				dkp->dk_trmode = 0 ;
				dkp->dk_C = dkp->dk_rseq = 0 ;
				break ;
			case INIT1:
				dksend(stechan, AINIT) ;
				dkp->dk_trmode = 1 ;
				dkp->dk_C = dkp->dk_tail1 = dkp->dk_tail2 = dkp->dk_rseq = 0 ;
				while (pkb) {
					pkb->Pnext = dk_Sfree ;
					dk_Sfree = pkb ;
					if (pkb = dkp->dk_rq)
						dkp->dk_rq = pkb->Pnext ;
					if (pkb == NULL)
						if (pkb = dkp->dk_rb)
							dkp->dk_rb = pkb->Pnext ;
				}
				goto qpkb ;
			case INITREQ:
				dksend(stechan, INIT1) ;
				dkp->dk_X = 0 ;
				break ;
			case ENQ:
				dksend(stechan, dkp->dk_C) ;
			case CHECK:
				dksend(stechan, ACK+dkp->dk_rseq) ;
				while (pkb) {
					pkb->Pnext = dk_Sfree ;
					dk_Sfree = pkb ;
					if (pkb = dkp->dk_rb)
						dkp->dk_rb = pkb->Pnext ;
				}
				dkp->dk_rblen = 0 ;
				goto qpkb ;
			case EOI:
				if (dkp->dk_tail1 == ((SOI<<8)|2)) 
					dkreport(KS_EOI, stechan, dkp->dk_tail2, 0, 1) ;
				dkp->dk_tail1 = 0 ;
				break ;
			default:
				if (c & DKDATA) {
					if (dkp->dk_tail1) {
						dkp->dk_tail2 = ((dkp->dk_tail2 >> 8) & 0377) | (c<<8) ;
						dkp->dk_tail1++ ;
						if ((dkp->dk_tail1 & 0377) > 3)
							dkp->dk_tail1 = 0 ;
					} else {
						*ptr++ = c & 0377 ;
						pkb->Plen++ ;
						bitloc >>= 1 ;
					}
					break ;
				}
				if (((c & 0770) == ECHO) || ((c & 0770) == REJ)) {
					URPTRACE(stechan, c, dkp);
					dkp->dk_R = c & 07 ;
					if (((dkp->dk_S - dkp->dk_R - 1) & 07) <
					    ((dkp->dk_S - dkp->dk_A - 1) & 07)) {
gotack:
						dkp->dk_X &= ~(XM_REJ | XM_ENQ);
						xlen = dkp->dk_xlen ;
						len = ((c - dkp->dk_A) & 07) * DKBLOCK ;
						len = MIN(len, xlen);
						dkp->dk_xlen -= len;
						if (dkp->dk_curout)
							m_adj(dkp->dk_curout->m_next, len) ;
						dkp->dk_A = c & 07 ;
						if (len || xlen)
							if ((dkp->dk_xlen) == 0)
								dkreport(KS_SEND, stechan, 0, 0, 1) ;
					}
					dkxmit(dkp, stechan, 1) ;
					raddr->dkcsr = D_READ ;
					if ((c & 0770) == REJ && ((dkp->dk_X & XM_REJ) == 0)) {
						dkp->dk_rejcnt++;
gotrej:
						dkp->dk_S = (c+1) & 07 ;
						dkxmit(dkp, stechan, 1) ;
						raddr->dkcsr = D_READ ;
						dkp->dk_X |= XM_REJ ;
					}
					break ;
				}
				if ((c & 0770) == ACK) {
					URPTRACE(stechan, c, dkp);
					if (dkp->dk_A != (c & 07))
						goto gotack ;
					if ((dkp->dk_X & XM_REJ) == 0) {
						dkp->dk_ackrejcnt++;
						goto gotrej ;
					}
					break ;
				}
				if ((c & 0774) == BOT) {
					dkp->dk_tail1 = c << 8 ;
					break ;
				}
				if ((c & 0770) == SEQ) {
					pkb->Pseq = c - SEQ + ECHO ;
					dkrcvq(stechan, dkp, pkb, 0) ;
					if (dkp->dk_trmode) {
						if (dkp->dk_rblen == dkp->dk_tail2 &&
						    (dkp->dk_tail1 & 0377) == 2 &&
						    ((dkp->dk_tail1 >> 8) & 0377) != SOI &&
						    ((dkp->dk_rseq+1) & 07) == (c & 07)) {
							dkp->dk_rseq = c & 07 ;
							if (((dkp->dk_tail1>>8) & 0377) != BOTM)
								pkb->Pseq += ACK - ECHO ;
							dkrcvq(stechan, dkp, dkp->dk_rb, 1) ;
						} else {
							while (pkb = dkp->dk_rb) {
								dkp->dk_rb = pkb->Pnext ;
								pkb->Pnext = dk_Sfree ;
								dk_Sfree = pkb ;
							}
							pkb = dk_Sfree ;
							dk_Sfree = pkb->Pnext ;
							pkb->Plen = 0 ;
							pkb->Pnext = NULL ;
							pkb->Pseq = REJ + dkp->dk_rseq ;
							dkp->dk_srejcnt++;
							if (((dkp->dk_tail1>>8) & 0377) == BOTS)  {
								dkp->dk_rseq = c & 07 ;
								pkb->Pseq = ECHO + dkp->dk_rseq ;
							}
							dkrcvq(stechan, dkp, pkb, 1) ;
						}
						dkp->dk_rb = NULL ;
						dkp->dk_rblen = 0 ;
						dkp->dk_tail1 = 0 ;
					} else
						/* always keep seq no up to date */
						dkp->dk_rseq = c & 07;
					pkb = NULL ;
					goto qpkb ;
				}
				if (c & 0200) {
					dkreport(KS_CNTL, stechan, c, 0, 1) ;
					raddr->dkcsr = D_READ ;
				} else {
					*ptr++ = c & 0377 ;
					pkb->Plen++ ;
					pkb->Phibits |= bitloc ;
					bitloc >>= 1 ;
				}
			}
			if (pkb && pkb->Plen) {
				dkrcvq(stechan, dkp, pkb, 0) ;
				pkb = NULL ;
			}
		}
	}

	if (pkb) {
		if (pkb->Plen)
			dkrcvq(stechan, dkp, pkb, 0) ;
		else {
			pkb->Pnext = dk_Sfree ;
			dk_Sfree = pkb ;
		}
	}
	raddr->dkcsr = ENABS ;
	return;

disable:
	if(pkb){
		pkb->Pnext = dk_Sfree;
		dk_Sfree = pkb;
	}
	raddr->dkcsr = 0;
	dkdisabled = 1;
}

dkrcvq(stechan, dkp, npkb, where)
register struct dkchan *dkp ;
struct dkpkbufr *npkb ;
{
register struct dkpkbufr *pkb ;
	int	i ;

	i = 0 ;
	if (dkp->dk_trmode && where == 0)
		pkb = (struct dkpkbufr *)&dkp->dk_rb ;
	else
		pkb = (struct dkpkbufr *)&dkp->dk_rq ;

	while (pkb->Pnext) {
		pkb = pkb->Pnext ;
		i++ ;
	}
	if ( i >= PKBHOG ) {
		/*
		** This channel has too many buffers.
		** Do not queue any more.
		** Return the new buffer to the free list.
		*/
		npkb->Pnext = dk_Sfree ;
		dk_Sfree = npkb ;
		return ;
	}
	pkb->Pnext = npkb ;

	if (dkp->dk_trmode && where == 0)
		dkp->dk_rblen += npkb->Plen ;
	else
		dkrcv(dkp, stechan, 1) ;
}



dkreport(type, chan, info1, info2, intrpt)
/*  intrpt parameter controlls whether the pdp-11 interrupt is called.
 *  Value 0 says no (means dkxint queued already)
 *  Value 1 says call it immediately (like from dr11c interrupt)
 *  Value 2 says to queue a call as soon as processor priority lowers
 *       (by sending a dummy packet on a channel and getting dkxint)
 */
{
	register struct dkstat	*sp;

	if ((Tstat + 1) % dkdr_nstat == Hstat) {	/* room in queue? */
		log(LOG_ERR, "dkit_dr: No room in status queue, Channel %d\n", chan);
		return;
	}

	sp = &dkdr_stat[Tstat] ;
	sp->k_chan = chan ;
	sp->k_type = type ;
	sp->k_info1 = info1 ;
	sp->k_info2 = info2 ;
	if (Tstat == dkdr_nstat-1)  Tstat = 0 ;  else Tstat++ ;
	if (intrpt==1) {
		dkkint() ;
	} else if (intrpt==2) {
		register struct rdevice *raddr ;
		raddr = DKADDR ;
		raddr->dkcsr = D_WRITE ;

		/* Chl (dk_nchan-1) is used instead of 511 because
		 * the LDI switch module will complain if we use
		 * a chl outside the range set up in its route tables.
		 */
		raddr->dko = (dk_nchan-1) | DKMARK ;

		/*
		 * A null is used here because it should always
		 * be ignored by the far end of the circuit.
		 */
		raddr->dko = 0 ;

		raddr->dkcsr = D_XPACK ;
		raddr->dko = 0 ;
		raddr->dkcsr = ENABS ;
	}
}

#ifdef	URPDEBUG
struct	dkurps {
	char	dku_ctl;
	char	dku_S;
	char	dku_R;
	char	dku_A;
} dkurps[URPDEBUG];
int	dkurpsize = URPDEBUG;

struct	dkurps	*dkurpsp = dkurps;
int	dkurpreset;

dkurptrace(ctl, dkp) char ctl; register struct dkchan *dkp;
{
#ifdef lint
	dkurpsize = dkurpsize;
#endif
	if (dkurpreset) {
		dkurpsp = dkurps;
		dkurpreset = 0;
	}
	dkurpsp->dku_ctl = ctl;
	dkurpsp->dku_S = dkp->dk_S;
	dkurpsp->dku_R = dkp->dk_R;
	dkurpsp->dku_A = dkp->dk_A;
	if (++dkurpsp == dkurps+URPDEBUG)
		dkurpsp = dkurps;
}
#endif	URPDEBUG
#endif
