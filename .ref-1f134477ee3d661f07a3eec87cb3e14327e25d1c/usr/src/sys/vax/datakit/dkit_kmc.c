/*
 * Datakit driver
 * KMC assistance, with or without DR11C
 *	@(#)dkit_kmc.c	1.4 (Berkeley) %G%
 */

#include "dkitkmc.h"
#if NDKITKMC>0

#include "datakit.h"

#include "../include/pte.h"
#include "sys/param.h"
#include "sys/syslog.h"
#include "sys/time.h"
#include "sys/kernel.h"
#include "sys/buf.h"
#include "sys/mbuf.h"
#include "sys/errno.h"
#include "sys/socket.h"
#include "net/if.h"
#include "../if/if_uba.h"
#include "../uba/ubareg.h"
#include "../uba/ubavar.h"

#include "dkit.h"
#include "dkkmc.h"
#include "dk.h"

#define KMXSMALL	0
#define KMXBIG		1

#define	MONITOR	1

#ifdef	MONITOR
static	int	dummy ;
int	*DKP = &dummy ;
#define	M_ON(a)		*DKP |= (a)
#define M_OFF(a)	*DKP &= ~(a)
#define	M_TRACE(a)	*DKP |= (a);*DKP &= ~(a)

#define	Mxmit	01
#define	Mrecv	02
#define	Mkint	04
#define	Mint	010
#define	Mcmd	020

#else
#define	M_ON(a)
#define	M_OFF(a)
#define	M_TRACE(a)
#endif

extern int		dk_nchan;

struct dkchan	dkit[NDATAKIT];

#define	DKNCMDB	20
#define	DKNSTB	20

int dkk_ncmd = DKNCMDB;
struct dkkin dkkcmdbuf[DKNCMDB];
int dkk_nstat = DKNSTB;
struct dkkin dkkstat[DKNSTB];
char dkkbuf[16*1024];
int dkubmbuf;

static struct kdevice *dkkaddr;

/*
 * initial information to the KMC
 */
struct dkinit {
	caddr_t	cmdaddr;	/* command buffer */
	caddr_t	stataddr;	/* status buffer  */
	caddr_t	bufaddr ;	/* kmc workspace  */
	caddr_t	csraddr;	/* for KMC/DR DR register address */
} dkkmcinit;

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
 * The circular buffer, cmdbuf, is used to pass command to kmc:
 * while the circular buffer statbuf is used to report status.
 * There are 8 control and status registers (csr) accessible to
 * both cpu and kmc.
 * Csr4-csr5 are used to indicate the head and tail respectively
 * of the cmdbuf.  Likewise, csr6-csr7 for statbuf.
 * At initialization time, the cpu and kmc would agree on the beginning
 * address of both buffers and their sizes.
 */
#define	csr0	dkkaddr->un.bsel.bsel0	/* kmc state */
#define	csr1	dkkaddr->un.bsel.bsel1
#define	csr2	dkkaddr->un.bsel.bsel2	/* used at init time, to pass */
#define	csr3	dkkaddr->un.bsel.bsel3	/* addresses to the kmc       */
#define	csr4	dkkaddr->un.bsel.bsel4  /* head cmdbuf */
#define	csr5	dkkaddr->un.bsel.bsel5  /* tail cmdbuf */
#define	csr6	dkkaddr->un.bsel.bsel6	/* head statbuf */
#define	csr7	dkkaddr->un.bsel.bsel7	/* tail statbuf */


/*
 * kmc device registers
 */
struct kdevice {
union {
struct	{ short sel0, sel2, sel4, sel6;} wsel ;
struct	{ char bsel0, bsel1, bsel2, bsel3;
	  char bsel4, bsel5, bsel6, bsel7; } bsel ;
} un;
};


/*
 * For the moment, only support one kmc (kmc0)
 * More need be done for multiple kmc's
 */

int	dkdebug = 512 ;
int	dkactive = 0 ;
int	dkbindex ;
	static int	kseqchk = 0;	/* used to check seq. #'s in statbuf */
	static int	dkpanic = 0;	/* # of dk_close(0)'s in this run */
	static int	kseq = 0;	/* # kmc responses mod 0377 */
	static int	pseq = 0;
 
	static struct dkkin *cmd4;		/* dkkcmdbuf[csr4] pointer */
	static struct dkkin *stat7;		/* dkkstat[csr7] pointer */

int dkk_cnt ;
static struct uba_device *ui;

dkkmc_attach(kui) struct uba_device *kui;
{
	ui = kui;
	dkk_cnt = kui->ui_unit;
}

dk_init()
{
	int t, kt ;
	extern dkkint() ;

	/*
	 *  On the first open of the hardware interface
	 */
	if (!ui) return -ENXIO;
	if (kmcset((dkk_cnt)<<6,03,dkkint)) {
/* debug */	log(LOG_ERR, "dkkmcinit bad: kmcset failed\n");
		return -1;
	}
	dkkaddr = ((struct kdevice *) ui->ui_addr);

	/* if csr0 != 0, then error
	   else pass the address of struct init
	   in csr2~4 and set csr0 to 1	*/

	if ((csr0 & 3) != 0) {
/* debug */	log(LOG_ERR, "dkkmcinit: csr0 != 0\n");
		return EIO;
	}

	/* Map UBA registers to point to our stuff */
	kt = dk_ubainit();
	if (kt == 0) {
		log(LOG_ERR, "dkkmcinit: no uba resources\n");
		return ENOBUFS;
	}

	/* Address of DR11-C (if any) */
	t = ui->ui_flags & ~03 ;
	dkkmcinit.csraddr = (caddr_t) ((t<<16) + 3) ;	/* bits 17 + 18 must be 1 */

	/* append new init info here, if it is needed */

	dkkaddr->un.wsel.sel2 = (short)(UBAI_ADDR(kt) & 0xFFFF);/* bits 0-15 */
	dkkaddr->un.bsel.bsel4 = (char)(UBAI_ADDR(kt) >> 16);	/* bits 16-17 */

	csr0 = 1;	/* tell KMC to read csr2 */
	kseq = 0 ;

	cmd4 = &dkkcmdbuf[0] ;	/* driver's pointers into cmdbuf and statbuf */
	stat7 = &dkkstat[0] ;
	dkactive = 1 ;
	return 0 ;
}

int	dk_ubaed = 0;

dk_ubainit()
{
	int t;
	static int kt;

	if (dk_ubaed) {
		if (dkdebug < dk_nchan)
			log(LOG_ERR, "dk_ubainit: reinit\n");
		return kt;
	}
	dk_ubaed = 1;

	/* Initialization buffer */
	kt = uballoc(ui->ui_ubanum, (caddr_t) &dkkmcinit, sizeof dkkmcinit, UBA_CANTWAIT);
	if (kt == 0) return 0;

	/* Command input buffer */
	t = uballoc(ui->ui_ubanum, (caddr_t) dkkcmdbuf, sizeof dkkcmdbuf, UBA_CANTWAIT) ;
	if (t == 0) return 0;
	dkkmcinit.cmdaddr = (caddr_t)((UBAI_ADDR(t)<<16) | ((UBAI_ADDR(t)>>16)));	/* must swap bytes for unibus */

	/* Status out buffer */
	t = uballoc(ui->ui_ubanum, (caddr_t) dkkstat, sizeof dkkstat, UBA_CANTWAIT);
	if (t == 0) return 0;
	dkkmcinit.stataddr = (caddr_t)((UBAI_ADDR(t)<<16) | (UBAI_ADDR(t)>>16));

	/* KMC buffer */
	dkubmbuf = uballoc(ui->ui_ubanum, (caddr_t) dkkbuf, sizeof dkkbuf, UBA_CANTWAIT);
	if (dkubmbuf == 0) return 0;
	dkkmcinit.bufaddr = (caddr_t) ((UBAI_ADDR(dkubmbuf)<<16) |
	    (UBAI_ADDR(dkubmbuf)>>16));
	if (dkdebug < dk_nchan)
		log(LOG_ERR, "dk_ubainit: bufaddr %x mapped %x\n", (caddr_t)dkkbuf,
		    dkubmbuf);

	return kt;
}

dk_open(chan, supfcn)
register chan ;
int (*supfcn)() ;
{
	register struct	dkchan	*dkp;
	register	s ;
	extern 	dkkint() ;
	extern int commchan;
	int init;

	if (chan >= dk_nchan)
		return -ENXIO ;
	if (dkactive == -1)	/* fail request if reset is in progress */
		return(-ENETRESET) ;
	dkp = &dkit[chan] ;
	s = splimp() ;
	/*
	 * Channel 0 (0-3 in ISN) is reserved for maintenance.
	 * An open on channel 0 is interpreted as a request
	 * for an unused channel.  Channel 1 (4 in ISN or RADIAN)
	 * is the common supervisory channel.
	 */
	if (chan == 0) {
		chan = commchan+1;		/* Start above commchan */
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
	splx(s) ;

	if (dkactive == 0)
		if ((init = dk_init()) < 0)
			return init ;

	/*
	 * On first use of a channel, we must
	 * allocate per-channel UBA resource for transmit.
	 * Some day, when we convert the receivers to use mbufs,
	 * we'll do the same for reads.
	 * Note that these resources can't easily be freed (I think).
	 */
	if (!dkp->dk_uba.ifu_uba) {
		dkp->dk_uba.ifu_flags = UBA_CANTWAIT;
		if (if_ubaminit(&dkp->dk_uba.ifu_info, ui->ui_ubanum, 0,
		    btoc(CLBYTES), 
		    &dkp->dk_uba.ifu_r, 0,
		    &dkp->dk_uba.ifu_xmt, 1) == 0) {
			log(LOG_ERR, "dkkmc: no ubamap for channel %d\n", chan);
			return -ENOBUFS;
		}
		dkp->dk_outq.ifq_maxlen = 20;
	}

	/*
	 * Finish setting up dkp struct.
	 */
	if ((dkp->dk_state & DK_OPEN) ==0) {
		dkcmd(KC_XINIT, chan, (caddr_t)0, (unsigned) 0, KMXBIG, 0);
		flushall(dkp, 0);
		dkp->dk_state |= DK_OPEN;
		dkp->dk_state &= ~(DK_LINGR | DK_RESET);
		dkactive++ ;
	}
	dkp->dk_supfcn = supfcn ;
	return chan ;
}


/*
 * Close a channel:
 */

dk_close(chan)
{
	register struct	dkchan	*dkp;
	register s ;
	int init;

	if (dkkaddr == 0) return(-ENODEV);	/* if no init, can't close */
						/* ie: can't do dkmaint */

	s = splimp() ;
	dkp = &dkit[chan] ;
	if (chan == 0) {
		for (dkp = &dkit[1]; dkp < &dkit[dk_nchan]; dkp++) {
			if (dkp->dk_state & (DK_OPEN|DK_BUSY|DK_RCV)) {
				dkp->dk_state |= DK_RESET ;
				flushall(dkp, 0) ;
				dkp->dk_state = DK_RESET ;
			}
		}
		dkpanic++ ;
		kseq = 0 ;
		pseq = 0 ;
#ifdef notdef
		if(dkubmbuf){   /* only deallocate mem if still allocated */
			ubarelse(ui->ui_ubanum, &dkubmbuf);
			dkubmbuf = NULL;
		}
#endif
		/* wait for protocols to close channels */
		dkactive = -1 ;
		DELAY(4 * hz) ;
		/* do a dk_free for all channels */
		for (dkp = &dkit[1]; dkp < &dkit[dk_nchan]; dkp++) {
			dkp->dk_state &= ~DK_LINGR ;
		}
		dkactive = 0 ;
		csr0 = 0 ;	/* set kmc to idle mode */
		if ((init = dk_init()) < 0) {
			splx(s) ;
			return init ;
		}
	} else {
		flushall(dkp, 0) ;
		dkp->dk_state = DK_LINGR ;	/* set while UNIXP protocol
						   closes up channel with DK */
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
		log(LOG_ERR, "dk_free %d\n", chan) ;
	dkit[chan].dk_state &= ~(DK_LINGR | DK_RESET);
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
		log(LOG_ERR, "dk_reset %d\n", chan) ;
	s = splimp() ;
	dkp = &dkit[chan] ;
	dkp->dk_state |= DK_RESET ;
	flushall(dkp, 0) ;
	splx(s) ;
}



/*
 *	Xmit a short control (interrupt) packet (max 2 bytes)
 */
dk_xint(chan, intr)
{
register s ;

	s = splimp() ;
	dkcmd(KC_SOI, chan, (caddr_t)0, (unsigned) intr, 0, 0) ;
	splx(s) ;
	return 0 ;
}


/*
 * Adjust window size
 */
/*ARGSUSED*/
dk_winsize(chan, win)
	struct diocxwin *win;
{
	return EINVAL;		/* For now... */
}



/*
 * Xmit data on a channel
 */
dk_xmit(chan, m, eob, ctlchar, endfcn, endparm)
	struct mbuf *m ;
	int (*endfcn)() ;
	caddr_t endparm ;
{
	register struct dkchan *dkp ;
	short s ;
	register struct mpacket *mbp ;
	register struct mbuf *mb;
	int len;

	M_ON(Mxmit) ;
	s = splimp() ;
	dkp = &dkit[chan] ;
	if ((dkp->dk_state & DK_RESET) || (mb = m_get(M_DONTWAIT,DKMT_HDR)) == NULL) {
		m_freem(m);
		splx(s) ;
		return 0 ;
	}

	mb->m_len = 0;
	mbp = mtod(mb, struct mpacket *);
	mbp->mp_endfcn = endfcn ;
	mbp->mp_endparm = endparm ;
	mbp->mp_eob = eob;
	mbp->mp_ctl = ctlchar;

	if ((dkp->dk_state & DK_BUSY) == 0) {
		dkp->dk_state |= DK_BUSY ;
		dkp->dk_obuf = mbp ;
		mb->m_next = NULL;
		len = if_wubaput(&dkp->dk_uba, m);
		dkcmd(KC_SEND, chan, (caddr_t) UBAI_ADDR(dkp->dk_uba.ifu_w.ifrw_info),
		    (unsigned) len, eob ? SBOT : SBOTM, ctlchar) ;
		splx(s) ;
		M_OFF(Mxmit) ;
		return dkp->dk_state ;
	}

	mb->m_next = m;
	if (IF_QFULL(&dkp->dk_outq)) {
		IF_DROP(&dkp->dk_outq);
		m_freem(mb);
	}
	else
		IF_ENQUEUE(&dkp->dk_outq, mb);
	splx(s) ;
	M_OFF(Mxmit) ;
	return dkp->dk_state ;
}

/*
 * Receive into a block buffer
 */
dk_recv(chan, addr, len, mode, endfcn, endparm)
int len;
int (*endfcn)() ;
caddr_t addr, endparm ;
{
	register struct dkchan *dkp ;
	int s;

	s = splimp() ;
	M_ON(Mrecv) ;
	dkp = &dkit[chan] ;
	if (dkp->dk_state & (DK_RCV | DK_RESET)) {
		splx(s) ;
		return(0) ;
	}
	dkp->dk_ubmbase = uballoc(ui->ui_ubanum, addr, len, UBA_CANTWAIT);
	if (dkp->dk_ubmbase == NULL) {
		splx(s) ;
		return(0) ;
	}
	dkp->dk_state |= DK_RCV ;
	dkp->dk_endfcn = endfcn ;
	dkp->dk_endparm = endparm ;
	dkp->dk_rlen = len;
	dkcmd(KC_RCVB, chan, (caddr_t) UBAI_ADDR(dkp->dk_ubmbase), (unsigned) len, mode&0377, mode>>8);
	M_OFF(Mrecv) ;
	splx(s);
	return dkp->dk_state ;
}

/* Abort pending receive */


dk_rabort(chan, nendfcn, nendparm)
int (*nendfcn)() ;
caddr_t nendparm;
{
register struct dkchan *dkp ;
register s ;

	dkp = &dkit[chan] ;
	s = splimp() ;
	if (dkp->dk_state & DK_RCV) {	/* cancel outstanding receive */
		dkcmd(KC_RCVB, chan, (caddr_t)0, (unsigned) 0, 0, 0) ;
		dkp->dk_endfcn = nendfcn ;
		dkp->dk_endparm = nendparm ;
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


/*
 * Various control commands to KMC
 */
dk_cmd(chan, cmd)
{
	register struct dkchan *dkp ;
	register s ;

	dkp = &dkit[chan] ;
	if (cmd & (DKC_XINIT|DKC_FLUSH)) {
		/*for either command do the same thing:
		 * reinit the transmitter and flush any pending output.
		 * NOTE: for the kmc, there is no response to XINIT
		 * and no send complete for flush
		 */
		s = splimp() ;
		dkcmd(KC_XINIT, chan, (caddr_t)0, (unsigned) 0, KMXBIG, 0) ;
		flushall(dkp, -1) ;
		dkcmd(KC_CMD, chan, (caddr_t)0, (unsigned) DKC_FLUSH, 0, 0) ;
		splx(s);
		cmd &= ~(DKC_XINIT|DKC_FLUSH) ;
	}
	if (cmd)
		dkcmd(KC_CMD, chan, (caddr_t)0, (unsigned) cmd, 0, 0) ;
}


/*
 *	Note that flushall is often recursive when a tty driver
 *	is involved.
 */

static
flushall(dkp, rwflag)
register struct dkchan *dkp ;
{
	register s ;
	register struct mpacket *mbp ;
	int (*endfcn)();

	s = splimp() ;
	if ((dkp->dk_state & DK_RCV) && (rwflag >= 0)) {
		dkcmd(KC_RCVB, dkp-dkit, (caddr_t)0, (unsigned) 0, 0, 0) ;
		dkp->dk_state &= ~DK_RCV ;
		if (dkp->dk_ubmbase) {
			ubarelse(ui->ui_ubanum, &dkp->dk_ubmbase);
			dkp->dk_ubmbase = NULL;
		}
		if (endfcn = dkp->dk_endfcn) {
			dkp->dk_endfcn = NULL ;
			(*endfcn)(dkp->dk_endparm, dkp-dkit, dkp->dk_rlen, DKR_ABORT, 0) ;
		}
	}

	/* flush all writes current and pending */

	if ((dkp->dk_state & DK_BUSY) && (rwflag <= 0)) {
		register struct mbuf *m ;

		/* flush current write */
		if (mbp = dkp->dk_obuf) {
			dkp->dk_obuf = NULL;
			if (endfcn = mbp->mp_endfcn) {
				mbp->mp_endfcn = NULL;
				(endfcn)(mbp->mp_endparm, dkp-dkit);
			}
			m_free(dtom(mbp));
		}
		/* flush any pending writes which may be queued up */
		while (1) {
			IF_DEQUEUE(&dkp->dk_outq, m);
			if (!m) break;
			mbp = mtod(m, struct mpacket *);
			if (endfcn = mbp->mp_endfcn) {
				mbp->mp_endfcn = NULL;
				(endfcn)(mbp->mp_endparm, dkp-dkit);
			}
			m_freem(m);
		}
		/* mark channel as not busy */
		dkp->dk_state &= ~DK_BUSY ;
	}
	if ((dkp->dk_state & DK_OPEN) && (rwflag >= 0)) {
		dkcmd(KC_CLOSE, dkp-dkit, (caddr_t)0, (unsigned) 0, 0, 0) ;
		if (dkp->dk_state & DK_BUSY){
			mbp = dkp->dk_obuf;
			dkp->dk_obuf = NULL;
			if (endfcn = mbp->mp_endfcn) {
				mbp->mp_endfcn = NULL;
				(endfcn)(mbp->mp_endparm, dkp-dkit);
			}
			m_free(dtom(mbp));
			dkp->dk_state &= ~DK_BUSY ;
		}
	}
	splx(s) ;
}


short dup_count = 0; /* counter for number of duplicate sends */

/*
 * Routine to handle interrupts from the KMC
 *
 * This routine is called when
 * the KMC generates an unsolicited interrupt (VEC4 == 1)
 *
 * These interrupts are used by the KMC to notify dkit_kmc.c
 * of events such as output buffer completions
 * csr6 & csr7 point to dkkstat
 */
dkkint()
{
	register struct dkchan *dkp;
	register struct dkkin *sp;
	register chan;
	struct mpacket *mbp ;
	int (*endfcn)();

	M_ON(Mkint) ;

	chan = csr0 ;	/* temp for cc -O bug */
	if((chan & 01) == 1)	/* 1 or 3 -> ignore */
		return;
	sp = stat7 ;	/* next response to be processed */
	while (csr6 != csr7) {
		if (kseqchk)
			if ((((sp->k_chan >> 8)&0377) != kseq) ||
			    (((sp->k_type >> 8)&0377) != kseq)) {
				log(LOG_ERR, "dkkint: kseq %x chan %d type %x\n",
					kseq, sp->k_chan, sp->k_type) ;
				goto reset ;
			}
		kseq = (kseq + 1) & 0377 ;
		sp->k_addr = pseq ;
		pseq++ ;
		chan = sp->k_chan & 0377 ;	/* mask off seq. # */
		dkp = &dkit[chan];
		if (chan > dkdebug) {
			log(LOG_ERR, " dkkint: head %d tail %d", csr6, csr7) ;
			log(LOG_ERR, " type %x chan %d len %d mode %x ctl %x\n", sp->k_type&0377, sp->k_chan&0377, sp->k_len, sp->k_mode, sp->k_ctl) ;
		}
		switch(sp->k_type & 0377) {
		case KS_CNTL:
			if (dkp->dk_supfcn)
				(*dkp->dk_supfcn)(chan, sp->k_ctl) ;
			break ;
		case KS_EOI:
			break ;
		case KS_SEND:
			mbp = dkp->dk_obuf ;
			if (mbp == NULL) {
				if (dkp->dk_state & (DK_RESET|DK_LINGR))
					break;	/* flushall was already called */
				log(LOG_ERR, "dkkint: xbufout chan %d state %x\n", chan, dkp->dk_state) ;
				log(LOG_ERR, "head %d tail %d", csr6, csr7) ;
				log(LOG_ERR, " type %x len %d mode %x ctl %x\n", 
				sp->k_type&0377, sp->k_len, sp->k_mode, sp->k_ctl) ;
				break ;
			}
			dkp->dk_state &= ~DK_BUSY;
			dkp->dk_obuf = NULL ;
			if (endfcn = mbp->mp_endfcn) {
				mbp->mp_endfcn = NULL;
				(endfcn)(mbp->mp_endparm, chan) ;
			}
			m_free(dtom(mbp)) ;
			if (dkp->dk_uba.ifu_xtofree) {
				m_freem(dkp->dk_uba.ifu_xtofree);
				dkp->dk_uba.ifu_xtofree = 0;
			}
			if (dkp->dk_outq.ifq_head)
				dkstart(dkp) ;
			break;
		case KS_RDB:
			if (((dkp->dk_state & DK_RCV) == 0) && dkp->dk_endfcn) {
				log(LOG_ERR, "dkkint: rbufin chan %d state %x\n", chan, dkp->dk_state) ;
				log(LOG_ERR, " head %d tail %d\n", csr6, csr7) ;
				log(LOG_ERR, " type %x len %d mode %x ctl %x\n", 
				sp->k_type&0377, sp->k_len, sp->k_mode, sp->k_ctl) ;

				if (sp->k_ctl)
					break ;
				else {
					stat7 = sp ;	/* save it for dump */
					csr0 = 3 ;	/* stop KMC */
					panic("") ;	/* KMC probably wrote
						into a mbuf we don't own */
				}
			}
			dkp->dk_state &= ~DK_RCV ;
			if (dkp->dk_ubmbase) {
				ubarelse(ui->ui_ubanum, &dkp->dk_ubmbase);
				dkp->dk_ubmbase = NULL;
			}
			if (endfcn = dkp->dk_endfcn) {
				dkp->dk_endfcn = NULL;
				(endfcn)(dkp->dk_endparm, chan, sp->k_len, sp->k_mode, sp->k_ctl) ;
			}
			break;
		case KS_ERR:
			log(LOG_ERR, "dkkint: err : chan %d, code %x\nchead: %d, ctail: %d, rhead: %d, rtail: %d\n",
				chan, sp->k_len, csr4, csr5, csr6, csr7);

			/* if error is duplicate send, only close that chan, */
			/* not the whole interface */

			if (sp->k_len == E_DUP)  {
				dup_count++;
				if (dkp->dk_state & DK_OPEN)  {
					dk_reset(chan);
				}
				break;
			}
reset:
			(void) dk_close(0) ;
			return ;
		default:
			log(LOG_ERR, "dkkint: chan %d, type %x, len %d, ctl %x, mode %x\n",
			chan, sp->k_type&0377, sp->k_len, sp->k_ctl, sp->k_mode);
			goto reset ;
		}	/* end switch */

		if (csr7 == dkk_nstat-1) {
			csr7 = 0 ;
			sp = &dkkstat[0] ;
		} else {
			csr7++ ;
			sp++ ;
		}

	}	/* end while */
	stat7 = sp ;
	M_OFF(Mkint) ;
}
 
/*
 * Start (Restart) transmission on the given line
 */
dkstart(dkp)
register struct dkchan *dkp;
{
	register struct mpacket *mbp;
	register struct mbuf *m;
	int len;

	/*
	 * If it is currently active, just return
	 */
	if (dkp->dk_state&DK_BUSY)
		return;

	IF_DEQUEUE(&dkp->dk_outq, m);
	if (!m)
		return;
	mbp = mtod(m, struct mpacket *);

	dkp->dk_state |= DK_BUSY ;
	dkp->dk_obuf = mbp ;
	len = if_wubaput(&dkp->dk_uba, m->m_next);
	dkcmd(KC_SEND, dkp-dkit, (caddr_t) UBAI_ADDR(dkp->dk_uba.ifu_w.ifrw_info),
	    (unsigned) len, mbp->mp_eob ? SBOT : SBOTM, mbp->mp_ctl) ;
}
 
/*
 * Put command in dkkcmdbuf which is pointed by csr4~5
 */
dkcmd(type, chan, addr, len, mode, ctl)
int type, chan;
caddr_t addr ;
unsigned len ;
{
	register struct dkkin *sp;
	register s;
	register next;
	register loop;
	struct timeval tv1, tv2;

	M_ON(Mcmd) ;
	s = csr0 ;
	if ((s & 3) != 2)
		return;

	s = splimp();
	next = (csr4+1)%dkk_ncmd;
	loop = 0;
	while (csr5 == next) {
		/* give it a chance to empty the buffer */
		if (loop++>10000000) {
			log(LOG_ERR, "KMC DIED, restart\n");
			dk_close(0);
			splx(s);
			return;
		}
		log(LOG_ERR, "KMC cmd overrun for %ld\n", loop);
	}

	sp = cmd4 ;
	sp->k_type = type | ((pseq&0177)<<9) ;
	sp->k_chan = chan | ((kseq&0377)<<8) ;
	sp->k_addr = ((int)addr << 16) + ((int)addr >> 16) ;
	sp->k_len = len ;
	sp->k_mode = mode ;
	sp->k_ctl = ctl ;
	pseq++ ;

	csr4 = next;
	cmd4 = &dkkcmdbuf[next];
	
	if (chan > dkdebug) {
	    	log(LOG_ERR, " dkcmd: head %d, tail %d", csr4, csr5);
	    	log(LOG_ERR, " type %x, chan %d, addr %x, len %d", 
						type, chan, addr, len);
		log(LOG_ERR, " mode %x, ctl %x\n", mode, ctl);
	}
	splx(s) ;
	M_OFF(Mcmd) ;
}

#endif
