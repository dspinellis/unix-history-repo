/*
 *	@(#)ik.c	7.1 (Berkeley) %G%
 */

#include "ik.h"
#if NIK > 0
/*
 * PS300/IKON DR-11W Device Driver.
 */
#include "param.h"
#include "buf.h"
#include "cmap.h"
#include "conf.h"
#include "dir.h"
#include "dkstat.h"
#include "map.h"
#include "systm.h"
#include "user.h"
#include "vmmac.h"
#include "proc.h"
#include "uio.h"
#include "kernel.h"
#include "syslog.h"

#include "../tahoe/mtpr.h"
#include "../tahoe/pte.h"

#include "../tahoevba/vbavar.h"
#include "../tahoevba/ikreg.h"
#include "../tahoevba/psreg.h"
#include "../tahoevba/psproto.h"

int	ikprobe(), ikattach(), iktimer();
struct	vba_device *ikinfo[NIK];
long	ikstd[] = { 0 };
struct	vba_driver ikdriver = { ikprobe, 0, ikattach, 0, ikstd, "ik", ikinfo };

#define splik()		spl4()
/*
 * Devices are organized in pairs with the odd valued
 * device being used for ``diagnostic'' purposes.  That
 * is diagnostic devices don't get auto-attach'd and
 * detach'd on open-close.
 */
#define IKUNIT(dev)	(minor(dev) >> 1)
#define IKDIAG(dev)	(minor(dev) & 01)	/* is a diagnostic unit */

struct	ik_softc {
	uid_t	is_uid;		/* uid of open processes */
	u_short is_timeout;	/* current timeout (seconds) */
	u_short is_error;	/* internal error codes */
	u_short is_flags;
#define IKF_ATTACHED	0x1	/* unit is attached (not used yet) */
	union {
		u_short w[2];
		u_long	l;
	} is_nameaddr;		/* address of last symbol lookup */
	caddr_t is_buf[PS_MAXDMA];/* i/o buffer XXX */
} ik_softc[NIK];

struct	buf iktab[NIK];		/* unit command queue headers */
struct	buf rikbuf[NIK];	/* buffers for read/write operations */
struct	buf cikbuf[NIK];	/* buffers for control operations */

/* buf overlay definitions */
#define b_command	b_resid

int	ikdiotimo = PS_DIOTIMO; /* dio polling timeout */
int	iktimeout = PS_TIMEOUT; /* attention/dma timeout (in hz) */

ikprobe(reg, vi)
	caddr_t reg;
	struct vba_device *vi;
{
	register int br, cvec;		/* r12, r11 */
	register struct ikdevice *ik;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	ikintr(0);
#endif
	if (badaddr(reg, 2))
		return (0);
	ik = (struct ikdevice *)reg;
	ik->ik_vec = --vi->ui_hd->vh_lastiv;
	/*
	 * Use extended non-privileged address modifier
	 * to avoid address overlap with 24-bit devices.
	 */
	ik->ik_mod = 0xf1;			/* address modifier */
	/*
	 * Try and reset the PS300.  Since this
	 * won't work if it's powered off, we
	 * can't use sucess/failure to decide
	 * if the device is present.
	 */
	br = 0;
	(void) psreset(ik, IKCSR_IENA);
	if (br == 0)				/* XXX */
		br = 0x18, cvec = ik->ik_vec;	/* XXX */
	return (sizeof (struct ikdevice));
}

/*
 * Perform a ``hard'' reset.
 */
psreset(ik, iena)
	register struct ikdevice *ik;
{

	ik->ik_csr = IKCSR_MCLR|iena;
	DELAY(10000);
	ik->ik_csr = IKCSR_FNC3|iena;
	if (!iena)
		return (dioread(ik) == PS_RESET);
	return (1);
}

ikattach(vi)
	struct vba_device *vi;
{

	ik_softc[vi->ui_unit].is_uid = -1;
}

/*
 * Open a PS300 and attach.  We allow multiple
 * processes with the same uid to share a unit.
 */
/*ARGSUSED*/
ikopen(dev, flag)
	dev_t dev;
	int flag;
{
	register int unit = IKUNIT(dev);
	register struct ik_softc *sc;
	struct vba_device *vi;
	struct ikdevice *ik;
	int reset;

	if (unit >= NIK || (vi = ikinfo[unit]) == 0 || vi->ui_alive == 0)
		return (ENXIO);
	sc = &ik_softc[unit];
	if (sc->is_uid != (uid_t)-1 && sc->is_uid != u.u_uid)
		return (EBUSY);
	if (sc->is_uid == (uid_t)-1) {
		sc->is_timeout = 0;
		timeout(iktimer, (caddr_t)unit, hz);
		/*
		 * Perform PS300 attach for first process.
		 */
		if (!IKDIAG(dev)) {
			reset = 0;
		again:
			if (ikcommand(dev, PS_ATTACH, 1)) {
				/*
				 * If attach fails, perform a hard
				 * reset once, then retry the command.
				 */
				ik = (struct ikdevice *)ikinfo[unit]->ui_addr;
				if (!reset++ && psreset(ik, 0))
					goto again;
				untimeout(iktimer, (caddr_t)unit);
				return (EIO);
			}
		}
		sc->is_uid = u.u_uid;
	}
	return (0);
}

/*ARGSUSED*/
ikclose(dev, flag)
	dev_t dev;
	int flag;
{
	int unit = IKUNIT(dev);
	register struct ik_softc *sc = &ik_softc[unit];

	if (!IKDIAG(dev))
		(void) ikcommand(dev, PS_DETACH, 1);	/* auto detach */
	sc->is_uid = -1;
	untimeout(iktimer, (caddr_t)unit);
}

ikread(dev, uio)
	dev_t dev;
	struct uio *uio;
{

	return (ikrw(dev, uio, B_READ));
}

ikwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{

	return (ikrw(dev, uio, B_WRITE));
}

/*
 * Take read/write request and perform physical i/o
 * transaction with PS300.  This involves constructing
 * a physical i/o request vector based on the uio
 * vector, performing the dma, and, finally, moving
 * the data to it's final destination (because of CCI
 * VERSAbus bogosities).
 */
ikrw(dev, uio, rw)
	dev_t dev;
	register struct uio *uio;
	int rw;
{
	int error, unit = IKUNIT(dev), s, wrcmd;
	register struct buf *bp;
	register struct iovec *iov;
	register struct psalist *ap;
	struct ik_softc *sc = &ik_softc[unit];

	if (unit >= NIK)
		return (ENXIO);
	bp = &rikbuf[unit];
	error = 0, iov = uio->uio_iov, wrcmd = PS_WRPHY;
	for (; !error && uio->uio_iovcnt; iov++, uio->uio_iovcnt--) { 
		/*
		 * Hack way to set PS300 address w/o doing an lseek
		 * and specify write physical w/ refresh synchronization.
		 */
		if (iov->iov_len == 0) {
			if ((int)iov->iov_base&PSIO_SYNC)
				wrcmd = PS_WRPHY_SYNC;
			uio->uio_offset = (int)iov->iov_base & ~PSIO_SYNC;
			continue;
		}
		if (iov->iov_len > PS_MAXDMA) {
			sc->is_error = PSERROR_INVALBC, error = EINVAL;
			continue;
		}
		if ((int)uio->uio_offset&01) {
			sc->is_error = PSERROR_BADADDR, error = EINVAL;
			continue;
		}
		s = splbio();
		while (bp->b_flags&B_BUSY) {
			bp->b_flags |= B_WANTED;
			sleep((caddr_t)bp, PRIBIO+1);
		}
		splx(s);
		bp->b_flags = B_BUSY | rw;
		/*
		 * Construct address descriptor in buffer.
		 */
		ap = (struct psalist *)sc->is_buf;
		ap->nblocks = 1;
		/* work-around dr300 word swapping */
		ap->addr[0] = uio->uio_offset & 0xffff;
		ap->addr[1] = uio->uio_offset >> 16;
		ap->wc = (iov->iov_len + 1) >> 1;
		if (rw == B_WRITE) {
			error = copyin(iov->iov_base, (caddr_t)&ap[1],
			    (unsigned)iov->iov_len);
			if (!error)
				error = ikcommand(dev, wrcmd,
				    iov->iov_len + sizeof (*ap));
		} else {
			caddr_t cp;
			int len;

			error = ikcommand(dev, PS_RDPHY, sizeof (*ap));
			cp = (caddr_t)&ap[1], len = iov->iov_len;
			for (; len > 0; len -= NBPG, cp += NBPG)
				mtpr(P1DC, cp);
			if (!error)
				error = copyout((caddr_t)&ap[1], iov->iov_base,
				    (unsigned)iov->iov_len);
		}
		(void) splbio();
		if (bp->b_flags&B_WANTED)
			wakeup((caddr_t)bp);
		splx(s);
		uio->uio_resid -= iov->iov_len;
		uio->uio_offset += iov->iov_len;
		bp->b_flags &= ~(B_BUSY|B_WANTED);
	}
	return (error);
}

/*
 * Perform a PS300 command.
 */
ikcommand(dev, com, count)
	dev_t dev;
	int com, count;
{
	register struct buf *bp;
	register int s;

	bp = &cikbuf[IKUNIT(dev)];
	s = splik();
	while (bp->b_flags&B_BUSY) {
		if (bp->b_flags&B_DONE)
			break;
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO);
	}
	bp->b_flags = B_BUSY|B_READ;
	splx(s);
	bp->b_dev = dev;
	bp->b_command = com;
	bp->b_bcount = count;
	ikstrategy(bp);
	biowait(bp);
	if (bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	bp->b_flags &= B_ERROR;
	return (geterror(bp));
}

/*
 * Physio strategy routine
 */
ikstrategy(bp)
	register struct buf *bp;
{
	register struct buf *dp;

	/*
	 * Put request at end of controller queue.
	 */
	dp = &iktab[IKUNIT(bp->b_dev)];
	bp->av_forw = NULL;
	(void) splik();
	if (dp->b_actf != NULL) {
		dp->b_actl->av_forw = bp;
		dp->b_actl = bp;
	} else
		dp->b_actf = dp->b_actl = bp;
	if (!dp->b_active)
		ikstart(dp);
	(void) spl0();
}

/*
 * Start the next command on the controller's queue.
 */
ikstart(dp)
	register struct buf *dp;
{
	register struct buf *bp;
	register struct ikdevice *ik;
	register struct ik_softc *sc;
	u_short bc, csr;
	u_int addr;
	int unit;

loop:
	/*
	 * Pull a request off the controller queue
	 */
	if ((bp = dp->b_actf) == NULL) {
		dp->b_active = 0;
		return;
	}
	/*
	 * Mark controller busy and process this request.
	 */
	dp->b_active = 1;
	unit = IKUNIT(bp->b_dev);
	sc = &ik_softc[unit];
	ik = (struct ikdevice *)ikinfo[unit]->ui_addr;
	switch ((int)bp->b_command) {

	case PS_ATTACH:		/* logical unit attach */
	case PS_DETACH:		/* logical unit detach */
	case PS_LOOKUP:		/* name lookup */
	case PS_RDPHY:		/* physical i/o read */
	case PS_WRPHY:		/* physical i/o write */
	case PS_WRPHY_SYNC:	/* physical i/o write w/ sync */
		/*
		 * Handshake command and, optionally,
		 * byte count and byte swap flag.
		 */
		if (sc->is_error = diowrite(ik, (u_short)bp->b_command))
			goto bad;
		if (bp->b_command < PS_DETACH) {
			if (sc->is_error = diowrite(ik, (u_short)bp->b_bcount))
				goto bad;
			if (sc->is_error = diowrite(ik, (u_short)0 /* !swab */))
				goto bad;
		}
		/*
		 * Set timeout and wait for an attention interrupt.
		 */
		sc->is_timeout = iktimeout;
		return;

	case PS_DMAOUT:		/* dma data host->PS300 */
		bc = bp->b_bcount;
		csr = IKCSR_CYCLE;
		break;

	case PS_DMAIN:		/* dma data PS300->host */
		bc = bp->b_bcount;
		csr = IKCSR_CYCLE|IKCSR_FNC1;
		break;

	default:
		log(LOG_ERR, "ik%d: bad cmd %x\n", unit, bp->b_command);
		sc->is_error = PSERROR_BADCMD;
		goto bad;
	}
	/* initiate dma transfer */
	addr = vtoph((struct proc *)0, (unsigned)sc->is_buf);
	ik->ik_bahi = addr >> 17;
	ik->ik_balo = (addr >> 1) & 0xffff;
	ik->ik_wc = ((bc + 1) >> 1) - 1;	/* round & convert */
	ik->ik_pulse = IKPULSE_RATTF|IKPULSE_RDMAF;
	sc->is_timeout = iktimeout;
	ik->ik_csr = IKCSR_IENA|IKCSR_GO|csr;
	return;
bad:
	bp->b_flags |= B_ERROR;
	dp->b_actf = bp->av_forw;		/* remove from queue */
	biodone(bp);
	goto loop;
}

#define FETCHWORD(i) { \
	v = dioread(ik); \
	if (v == -1) { \
		sc->is_error = PSERROR_NAMETIMO; \
		goto bad; \
	} \
	sc->is_nameaddr.w[i] = v; \
}

/*
 * Process a device interrupt.
 */
ikintr(ikon)
	int ikon;
{
	register struct ikdevice *ik;
	register struct buf *bp, *dp;
	struct ik_softc *sc;
	register u_short data;
	int v;

	/* should go by controller, but for now... */
	if (ikinfo[ikon] == 0)
		return;
	ik = (struct ikdevice *)ikinfo[ikon]->ui_addr;
	/*
	 * Discard all non-attention interrupts.  The
	 * interrupts we're throwing away should all be
	 * associated with DMA completion.
	 */
	data = ik->ik_data;
	if ((ik->ik_csr&(IKCSR_ATTF|IKCSR_STATC)) != IKCSR_ATTF) {
		ik->ik_pulse = IKPULSE_RATTF|IKPULSE_RDMAF|IKPULSE_SIENA;
		return;
	}
	/*
	 * Fetch attention code immediately.
	 */
	ik->ik_csr = IKCSR_RATTF|IKCSR_RDMAF|IKCSR_FNC1;
	ik->ik_pulse = IKPULSE_FNC2;
	/*
	 * Get device and block structures, and a pointer
	 * to the vba_device for the device.  We receive an
	 * unsolicited interrupt whenever the PS300 is power
	 * cycled (so ignore it in that case).
	 */
	dp = &iktab[ikon];
	if ((bp = dp->b_actf) == NULL) {
		if (PS_CODE(data) != PS_RESET)		/* power failure */
			log(LOG_WARNING, "ik%d: spurious interrupt, code %x\n",
			    ikon, data);
		goto enable;
	}
	sc = &ik_softc[IKUNIT(bp->b_dev)];
	sc->is_timeout = 0;			/* disable timer */
	switch (PS_CODE(data)) {

	case PS_LOOKUP:				/* name lookup */
		if (data == PS_LOOKUP) {	/* dma name */
			bp->b_command = PS_DMAOUT;
			goto opcont;
		}
		if (data == PS_DMAOK(PS_LOOKUP)) {
			/* reenable interrupt and wait for address */
			sc->is_timeout = iktimeout;
			goto enable;
		}
		/*
		 * Address should be present, extract it one
		 * word at a time from the PS300 (yech).
		 */
		if (data != PS_ADROK(PS_LOOKUP))
			goto bad;
		FETCHWORD(0);
		FETCHWORD(1);
		goto opdone;

	case PS_WRPHY_SYNC:			/* physical i/o write w/ sync */
		if (data == PS_WRPHY_SYNC) {	/* start dma transfer */
			bp->b_command = PS_DMAOUT;
			goto opcont;
		}
		if (data != PS_DMAOK(PS_WRPHY_SYNC))
			goto bad;
		goto opdone;

	case PS_WRPHY:				/* physical i/o write */
		if (data == PS_WRPHY) { /* start dma transfer */
			bp->b_command = PS_DMAOUT;
			goto opcont;
		}
		if (data != PS_DMAOK(PS_WRPHY))
			goto bad;
		goto opdone;

	case PS_ATTACH:				/* attach unit */
	case PS_DETACH:				/* detach unit */
	case PS_ABORT:				/* abort code from ps300 */
		if (data != bp->b_command)
			goto bad;
		goto opdone;

	case PS_RDPHY:				/* physical i/o read */
		if (data == PS_RDPHY) {		/* dma address list */
			bp->b_command = PS_DMAOUT;
			goto opcont;
		}
		if (data == PS_ADROK(PS_RDPHY)) {
			/* collect read byte count and start dma */
			bp->b_bcount = dioread(ik);
			if (bp->b_bcount == -1)
				goto bad;
			bp->b_command = PS_DMAIN;
			goto opcont;
		}
		if (data == PS_DMAOK(PS_RDPHY))
			goto opdone;
		goto bad;
	}
bad:
	sc->is_error = data;
	bp->b_flags |= B_ERROR;
opdone:
	dp->b_actf = bp->av_forw;		/* remove from queue */
	biodone(bp);
opcont:
	ikstart(dp);
enable:
	ik->ik_pulse = IKPULSE_SIENA;		/* explicitly reenable */
}

/*
 * Watchdog timer.
 */
iktimer(unit)
	int unit;
{
	register struct ik_softc *sc = &ik_softc[unit];

	if (sc->is_timeout && --sc->is_timeout == 0) {
		register struct buf *dp, *bp;
		int s;

		log(LOG_ERR, "ik%d: timeout\n", unit);
		s = splik();
		/* should abort current command */
		dp = &iktab[unit];
		if (bp = dp->b_actf) {
			sc->is_error = PSERROR_CMDTIMO;
			bp->b_flags |= B_ERROR;
			dp->b_actf = bp->av_forw;	/* remove from queue */
			biodone(bp);
			ikstart(dp);
		}
		splx(s);
	}
	timeout(iktimer, (caddr_t)unit, hz);
}

/*
 * Handshake read from DR300.
 */
dioread(ik)
	register struct ikdevice *ik;
{
	register int t;
	u_short data;

	for (t = ikdiotimo; t > 0; t--)
		if ((ik->ik_csr&(IKCSR_ATTF|IKCSR_STATC)) == IKCSR_ATTF) {
			data = ik->ik_data;
			ik->ik_csr = IKCSR_RATTF|IKCSR_RDMAF|IKCSR_FNC1;
			ik->ik_pulse = IKPULSE_FNC2;
			return (data);
		}
	return (-1);
}

/*
 * Handshake write to DR300. 
 *
 * Interrupts are enabled before completing the work
 * so the caller should either be at splik or be
 * prepared to take the interrupt immediately.
 */
diowrite(ik, v)
	register struct ikdevice *ik;
	u_short v;
{
	register int t;
	register u_short csr;

top:
	/*
	 * Deposit data and generate dr300 attention
	 */
	ik->ik_data = v;
	ik->ik_csr = IKCSR_RDMAF|IKCSR_RATTF;
	ik->ik_pulse = IKPULSE_FNC2;
	for (t = ikdiotimo; t > 0; t--) {
		csr = ik->ik_csr;
#define IKCSR_DONE	(IKCSR_STATA|IKCSR_STATC)
		if ((csr&IKCSR_DONE) == IKCSR_DONE) {
			/* 
			 * Done, complete handshake by notifying dr300.
			 */
			ik->ik_csr = IKCSR_IENA;	/* ~IKCSR_FNC1 */
			ik->ik_pulse = IKPULSE_FNC2;
			return (0);
		}
		/* beware of potential deadlock with dioread */
		if ((csr&(IKCSR_ATTF|IKCSR_STATC)) == IKCSR_ATTF)
			goto top;
	}
	ik->ik_csr = IKCSR_IENA;
	return (PSERROR_DIOTIMO);
}

/*ARGSUSED*/
ikioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
{
	int error = 0, unit = IKUNIT(dev), s;
	register struct ik_softc *sc = &ik_softc[unit];

	switch (cmd) {

	case PSIOGETERROR:		/* get error code for last operation */
		*(int *)data = sc->is_error;
		break;

	case PSIOLOOKUP: {		/* PS300 name lookup */
		register struct pslookup *lp = (struct pslookup *)data;
		register struct buf *bp;

		if (lp->pl_len > PS_MAXNAMELEN)
			return (EINVAL);
		bp = &rikbuf[unit];
		s = splbio();
		while (bp->b_flags&B_BUSY) {
			bp->b_flags |= B_WANTED;
			sleep((caddr_t)bp, PRIBIO+1);
		}
		splx(s);
		bp->b_flags = B_BUSY | B_WRITE;
		error = copyin(lp->pl_name, (caddr_t)sc->is_buf,
		    (unsigned)lp->pl_len);
		if (error == 0) {
			if (lp->pl_len&1)
				sc->is_buf[lp->pl_len] = '\0';
			error = ikcommand(dev, PS_LOOKUP, lp->pl_len);
		}
		s = splbio();
		if (bp->b_flags&B_WANTED)
			wakeup((caddr_t)bp);
		splx(s);
		bp->b_flags &= ~(B_BUSY|B_WANTED);
		lp->pl_addr = sc->is_nameaddr.l;
		break;
	}
	default:
		return (ENOTTY);
	}
	return (error);
}
#endif
