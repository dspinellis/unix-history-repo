/*	dmf.c	4.2	82/05/27	*/

#include "dmf.h"
#if NDMF > 0
/*
 * DMF32 driver
 *
 * TODO:
 *	test with modem
 *	load as much as possible into silo
 *	get correct numbers for receive silo parameter timeout
 *	use auto XON/XOFF
 *	test reset code
 *	test with more than one unit
 *	optimize for efficient DMA and dynamically
 *	  decide between silo and DMA mode
 */
#include "bk.h"
#include "../h/param.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/vm.h"
#include "../h/ubareg.h"
#include "../h/ubavar.h"
#include "../h/bk.h"
#include "../h/clist.h"
#include "../h/file.h"

/*
 * Definition of the driver for the auto-configuration program.
 */
int	dmfprobe(), dmfattach(), dmfrint(), dmfxint();
struct	uba_device *dmfinfo[NDMF];
u_short	dmfstd[] = { 0 };
struct	uba_driver dmfdriver =
	{ dmfprobe, 0, dmfattach, 0, dmfstd, "dmf", dmfinfo };

/*
 * In this driver, "dmf" (unqualified) refers to the async portion
 * of the dmf32, "dmfc" to the combo portion, "dmfs" to the sync
 * portion, "dmfl" to the lp portion, and "dmfd" to the dr portion.
 */
struct dmfdevice
{
	short	dmfccsr0;		/* combo csr 0 */
	short	dmfccsr1;		/* combo csr 1 */
	short	dmfs[4];
	short	dmfcsr;			/* control-status register */
	short	dmflpr;			/* line parameter register */
	short	dmfrbuf;		/* receiver buffer (ro) */
	union {
		u_short	dmfirw;		/* indirect register word */
		u_char	dmfirc[2];	/*    "         "    bytes */
	} dmfun;
	short	dmfl[2];
	short	dmfd[4];
};

#define	dmfrsp	dmfrbuf		/* receive silo parameter register (wo) */
#define	dmftbuf	dmfun.dmfirc[0]	/* transmit buffer */
#define	dmftsc	dmfun.dmfirc[0]	/* transmit silo count */
#define	dmfrms	dmfun.dmfirc[1]	/* receive modem status */
#define	dmflcr	dmfun.dmfirc[0]	/* line control register */
#define	dmftms	dmfun.dmfirc[1]	/* transmit modem status */
#define	dmftba	dmfun.dmfirw	/* transmit buffer address */
#define	dmftcc	dmfun.dmfirw	/* transmit character count */

/* bits in dmfcsr */
#define	DMF_TI	0100000		/* transmit interrupt */
#define	DMF_TIE	0040000		/* transmit interrupt enable */
#define	DMF_NXM	0020000		/* non-existant memory */
#define	DMF_LIN	0003400		/* transmit line number */
#define	DMF_RI	0000200		/* receiver interrupt */
#define	DMF_RIE	0000100		/* receiver interrupt enable */
#define	DMF_CLR	0000040		/* master reset */
#define	DMF_IAD	0000037		/* indirect address register */

#define	DMFIR_TBUF	000	/* select tbuf indirect register */
#define	DMFIR_LCR	010	/* select lcr indirect register */
#define	DMFIR_TBA	020	/* select tba indirect register */
#define	DMFIR_TCC	030	/* select tcc indirect register */

/* bits in dmflpr */
#define	BITS6	(01<<3)
#define	BITS7	(02<<3)
#define	BITS8	(03<<3)
#define	TWOSB	0200
#define	PENABLE	040
/* DEC manuals incorrectly say this bit causes generation of even parity. */
#define	OPAR	0100

#define	DMF_IE	(DMF_TIE|DMF_RIE)

#define	DMF_SILOCNT	32		/* size of DMF output silo (per line) */

/* bits in dmfrbuf */
#define	DMF_DSC		0004000		/* data set change */
#define	DMF_PE		0010000		/* parity error */
#define	DMF_FE		0020000		/* framing error */
#define	DMF_DO		0040000		/* data overrun */

/* bits in dmfrms */
#define	DMF_USRR	0004		/* user modem signal (pin 25) */
#define	DMF_SR		0010		/* secondary receive */
#define	DMF_CTS		0020		/* clear to send */
#define	DMF_CAR		0040		/* carrier detect */
#define	DMF_RNG		0100		/* ring */
#define	DMF_DSR		0200		/* data set ready */

/* bits in dmftms */
#define	DMF_USRW	0001		/* user modem signal (pin 18) */
#define	DMF_DTR		0002		/* data terminal ready */
#define	DMF_RATE	0004		/* data signal rate select */
#define	DMF_ST		0010		/* secondary transmit */
#define	DMF_RTS		0020		/* request to send */
#define	DMF_BRK		0040		/* pseudo break bit */
#define	DMF_PREEMPT	0200		/* preempt output */

/* flags for modem control */
#define	DMF_ON	(DMF_DTR|DMF_RTS)
#define	DMF_OFF	0

/* bits in dmflcr */
#define	DMF_MIE		0040		/* modem interrupt enable */
#define	DMF_FLUSH	0020		/* flush transmit silo */
#define	DMF_RBRK	0010		/* real break bit */
#define	DMF_RE		0004		/* receive enable */
#define	DMF_AUTOX	0002		/* auto XON/XOFF */
#define	DMF_TE		0001		/* transmit enable */

#define	DMFLCR_ENA	(DMF_MIE|DMF_RE|DMF_TE)

/* bits in dm lsr, copied from dh.c */
#define	DML_USR		0001000		/* usr modem sig, not a real DM bit */
#define	DML_DSR		0000400		/* data set ready, not a real DM bit */
#define	DML_RNG		0000200		/* ring */
#define	DML_CAR		0000100		/* carrier detect */
#define	DML_CTS		0000040		/* clear to send */
#define	DML_SR		0000020		/* secondary receive */
#define	DML_ST		0000010		/* secondary transmit */
#define	DML_RTS		0000004		/* request to send */
#define	DML_DTR		0000002		/* data terminal ready */
#define	DML_LE		0000001		/* line enable */

/*
 * Local variables for the driver
 */
char	dmf_speeds[] =
	{ 0, 0, 1, 2, 3, 4, 0, 5, 6, 7, 010, 012, 014, 016, 017, 0 };

struct	tty dmf_tty[NDMF*8];
char	dmfsoftCAR[NDMF];
int	ndmf = NDMF*8;
int	dmfact;				/* mask of active dmf's */
int	dmfstart(), ttrstrt();

#ifdef DMFDMA
/*
 * The clist space is mapped by the driver onto each UNIBUS.
 * The UBACVT macro converts a clist space address for unibus uban
 * into an i/o space address for the DMA routine.
 */
int	dmf_ubinfo[MAXNUBA];		/* info about allocated unibus map */
static int cbase[MAXNUBA];		/* base address in unibus map */
#define	UBACVT(x, uban)		(cbase[uban] + ((x)-(char *)cfree))
#endif

/*
 * Routine for configuration to set dmf interrupt.
 */
/*ARGSUSED*/
dmfprobe(reg, ctlr)
	caddr_t reg;
	int ctlr;
{
	register int br, cvec;		/* these are ``value-result'' */
	register struct dmfdevice *dmfaddr = (struct dmfdevice *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
#endif
	br = 0x15;
	cvec = (uba_hd[numuba].uh_lastiv -= 4*8);
	dmfaddr->dmfccsr0 = cvec >> 2;
	/* NEED TO SAVE IT SOMEWHERE FOR OTHER DEVICES */
	return (1);
}

/*
 * Routine called to attach a dmf.
 */
dmfattach(ui)
	struct uba_device *ui;
{

	dmfsoftCAR[ui->ui_unit] = ui->ui_flags;
}


/*
 * Open a DMF32 line, mapping the clist onto the uba if this
 * is the first dmf on this uba.  Turn on this dmf if this is
 * the first use of it.
 */
/*ARGSUSED*/
dmfopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	register int unit, dmf;
	register struct dmfdevice *addr;
	register struct uba_device *ui;
	int s;

	unit = minor(dev);
	dmf = unit >> 3;
	if (unit >= NDMF*8 || (ui = dmfinfo[dmf])== 0 || ui->ui_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	tp = &dmf_tty[unit];
	if (tp->t_state&XCLUDE && u.u_uid!=0) {
		u.u_error = EBUSY;
		return;
	}
	addr = (struct dmfdevice *)ui->ui_addr;
	tp->t_addr = (caddr_t)addr;
	tp->t_oproc = dmfstart;
	tp->t_iproc = NULL;
	tp->t_state |= WOPEN;
	/*
	 * While setting up state for this uba and this dmf,
	 * block uba resets which can clear the state.
	 */
	s = spl5();
#ifdef DMFDMA
	if (dmf_ubinfo[ui->ui_ubanum] == 0) {
		dmf_ubinfo[ui->ui_ubanum] =
		    uballoc(ui->ui_ubanum, (caddr_t)cfree,
			nclist*sizeof(struct cblock), 0);
		cbase[ui->ui_ubanum] = dmf_ubinfo[ui->ui_ubanum]&0x3ffff;
	}
#endif
	if ((dmfact&(1<<dmf)) == 0) {
		addr->dmfcsr |= DMF_IE;
		dmfact |= (1<<dmf);
		addr->dmfrsp = 1;	/* DON'T KNOW WHAT TO SET IT TO YET */
	}
	splx(s);
	/*
	 * If this is first open, initialze tty state to default.
	 */
	if ((tp->t_state&ISOPEN) == 0) {
		ttychars(tp);
		if (tp->t_ispeed == 0) {
			tp->t_ispeed = B300;
			tp->t_ospeed = B300;
			tp->t_flags = ODDP|EVENP|ECHO;
		}
		dmfparam(unit);
	}
	/*
	 * Wait for carrier, then process line discipline specific open.
	 */
	if ((dmfmctl(dev, DMF_ON, DMSET) & (DMF_CAR<<8)) ||
	    (dmfsoftCAR[dmf] & (1<<(unit&07))))
		tp->t_state |= CARR_ON;
	s = spl5();
	while ((tp->t_state & CARR_ON) == 0) {
		tp->t_state |= WOPEN;
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	}
	splx(s);
	(*linesw[tp->t_line].l_open)(dev, tp);
}

/*
 * Close a DMF32 line.
 */
/*ARGSUSED*/
dmfclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tty *tp;
	register unit;

	unit = minor(dev);
	tp = &dmf_tty[unit];
	(*linesw[tp->t_line].l_close)(tp);
	dmfmctl(unit, DMF_BRK, DMBIC);
	if (tp->t_state&HUPCLS || (tp->t_state&ISOPEN)==0)
		dmfmctl(unit, DMF_OFF, DMSET);
	ttyclose(tp);
}

dmfread(dev)
	dev_t dev;
{
	register struct tty *tp;

	tp = &dmf_tty[minor(dev)];
	(*linesw[tp->t_line].l_read)(tp);
}

dmfwrite(dev)
	dev_t dev;
{
	register struct tty *tp;

	tp = &dmf_tty[minor(dev)];
	(*linesw[tp->t_line].l_write)(tp);
}

/*
 * DMF32 receiver interrupt.
 */
dmfrint(dmf)
	int dmf;
{
	register struct tty *tp;
	register c;
	register struct dmfdevice *addr;
	register struct tty *tp0;
	register struct uba_device *ui;
	int overrun = 0;

	ui = dmfinfo[dmf];
	if (ui == 0 || ui->ui_alive == 0)
		return;
	addr = (struct dmfdevice *)ui->ui_addr;
	tp0 = &dmf_tty[dmf<<3];
	/*
	 * Loop fetching characters from the silo for this
	 * dmf until there are no more in the silo.
	 */
	while ((c = addr->dmfrbuf) < 0) {
		tp = tp0 + ((c>>8)&07);
		if (c & DMF_DSC) {
			addr->dmfcsr = DMF_IE | DMFIR_TBUF | ((c>>8)&07);
			if (addr->dmfrms & DMF_CAR) {
				if ((tp->t_state & CARR_ON) == 0) {
					wakeup((caddr_t)&tp->t_rawq);
					tp->t_state |= CARR_ON;
				}
			} else {
				if (tp->t_state & CARR_ON) {
					gsignal(tp->t_pgrp, SIGHUP);
					gsignal(tp->t_pgrp, SIGCONT);
					addr->dmfcsr = DMF_IE | DMFIR_LCR |
						((c>>8)&07);
					addr->dmftms = 0;
					flushtty(tp, FREAD|FWRITE);
				}
				tp->t_state &= ~CARR_ON;
			}
			continue;
		}
		if ((tp->t_state&ISOPEN)==0) {
			wakeup((caddr_t)tp);
			continue;
		}
		if (c & DMF_PE)
			if ((tp->t_flags&(EVENP|ODDP))==EVENP
			 || (tp->t_flags&(EVENP|ODDP))==ODDP )
				continue;
		if ((c & DMF_DO) && overrun == 0) {
			printf("dmf%d: silo overflow\n", dmf);
			overrun = 1;
		}
		if (c & DMF_FE)
			/*
			 * At framing error (break) generate
			 * a null (in raw mode, for getty), or a
			 * interrupt (in cooked/cbreak mode).
			 */
			if (tp->t_flags&RAW)
				c = 0;
			else
				c = tun.t_intrc;
#if NBK > 0
		if (tp->t_line == NETLDISC) {
			c &= 0177;
			BKINPUT(c, tp);
		} else
#endif
			(*linesw[tp->t_line].l_rint)(c, tp);
	}
}

/*
 * Ioctl for DMF32.
 */
/*ARGSUSED*/
dmfioctl(dev, cmd, addr, flag)
	dev_t dev;
	caddr_t addr;
{
	register struct tty *tp;
	register int unit = minor(dev);
	register int dmf = unit >> 3;
	register struct device *dmfaddr;
	int temp;
 
	tp = &dmf_tty[unit];
	cmd = (*linesw[tp->t_line].l_ioctl)(tp, cmd, addr);
	if (cmd == 0)
		return;
	if (ttioctl(tp, cmd, addr, flag)) {
		if (cmd==TIOCSETP || cmd==TIOCSETN)
			dmfparam(unit);
	} else switch(cmd) {

	case TIOCSBRK:
		dmfmctl(dev, DMF_BRK, DMBIS);
		break;
	case TIOCCBRK:
		dmfmctl(dev, DMF_BRK, DMBIC);
		break;
	case TIOCSDTR:
		dmfmctl(dev, DMF_DTR|DMF_RTS, DMBIS);
		break;
	case TIOCCDTR:
		dmfmctl(dev, DMF_DTR|DMF_RTS, DMBIC);
		break;
	case TIOCMSET:
		if (copyin(addr, (caddr_t) &temp, sizeof(temp)))
			u.u_error = EFAULT;
		else
			dmfmctl(dev, dmtodmf(temp), DMSET);
		break;
	case TIOCMBIS:
		if (copyin(addr, (caddr_t) &temp, sizeof(temp)))
			u.u_error = EFAULT;
		else
			dmfmctl(dev, dmtodmf(temp), DMBIS);
		break;
	case TIOCMBIC:
		if (copyin(addr, (caddr_t) &temp, sizeof(temp)))
			u.u_error = EFAULT;
		else
			dmfmctl(dev, dmtodmf(temp), DMBIC);
		break;
	case TIOCMGET:
		temp = dmftodm(dmfmctl(dev, 0, DMGET));
		if (copyout((caddr_t) &temp, addr, sizeof(temp)))
			u.u_error = EFAULT;
		break;
	default:
		u.u_error = ENOTTY;
	}
}

dmtodmf(bits)
	register int bits;
{
	register int b;

	b = bits & 012;
	if (bits & DML_ST) b |= DMF_RATE;
	if (bits & DML_RTS) b |= DMF_RTS;
	if (bits & DML_USR) b |= DMF_USRW;
	return(b);
}

dmftodm(bits)
	register int bits;
{
	register int b;

	b = (bits & 012) | ((bits >> 7) & 0760) | DML_LE;
	if (bits & DMF_USRR) b |= DML_USR;
	if (bits & DMF_RTS) b |= DML_RTS;
	return(b);
}
 

/*
 * Set parameters from open or stty into the DMF hardware
 * registers.
 */
dmfparam(unit)
	register int unit;
{
	register struct tty *tp;
	register struct dmfdevice *addr;
	register int lpar, lcr;
	int s;

	tp = &dmf_tty[unit];
	addr = (struct dmfdevice *)tp->t_addr;
	/*
	 * Block interrupts so parameters will be set
	 * before the line interrupts.
	 */
	s = spl5();
	addr->dmfcsr = (unit&07) | DMFIR_LCR | DMF_IE;
	if ((tp->t_ispeed)==0) {
		tp->t_state |= HUPCLS;
		dmfmctl(unit, DMF_OFF, DMSET);
		return;
	}
	lpar = (dmf_speeds[tp->t_ospeed]<<12) | (dmf_speeds[tp->t_ispeed]<<8);
	lcr = DMFLCR_ENA;
	if ((tp->t_ispeed) == B134)
		lpar |= BITS6|PENABLE;
	else if ((tp->t_flags&RAW) || (tp->t_local&LLITOUT))
		lpar |= BITS8;
	else {
		lpar |= BITS7|PENABLE;
		/* CHECK FOR XON/XOFF AND SET lcr |= DMF_AUTOX; */
	}
	if ((tp->t_flags&EVENP) == 0)
		lpar |= OPAR;
	if ((tp->t_ospeed) == B110)
		lpar |= TWOSB;
	lpar |= (unit&07);
	addr->dmflpr = lpar;
	addr->dmflcr = lcr;
	splx(s);
}

/*
 * DMF32 transmitter interrupt.
 * Restart the idle line.
 */
dmfxint(dmf)
	int dmf;
{
	register struct tty *tp;
	register struct dmfdevice *addr;
	register struct uba_device *ui;
	register int unit, t;
#ifdef DMFDMA
	short cntr;
#endif

	ui = dmfinfo[dmf];
	addr = (struct dmfdevice *)ui->ui_addr;
	while ((t = addr->dmfcsr) & DMF_TI) {
		unit = dmf*8 + ((t>>8)&07);
		tp = &dmf_tty[unit];
		tp->t_state &= ~BUSY;
		if (t & DMF_NXM) {
			printf("dmf%d: NXM line %d\n", dmf, unit&7);
			/* SHOULD RESTART OR SOMETHING... */
		}
		if (tp->t_state&FLUSH)
			tp->t_state &= ~FLUSH;
#ifdef DMFDMA
		else {
			addr->dmfcsr = DMFIR_TBUF | DMF_IE | (unit&07);
			if (addr->dmftsc == 0) {
				/*
				 * Do arithmetic in a short to make up
				 * for lost 16&17 bits.
				 */
				addr->dmfcsr = DMFIR_TBA | DMF_IE | (unit&07);
				cntr = addr->dmftba -
				    UBACVT(tp->t_outq.c_cf, ui->ui_ubanum);
				ndflush(&tp->t_outq, (int)cntr);
			}
		}
#endif
		if (tp->t_line)
			(*linesw[tp->t_line].l_start)(tp);
		else
			dmfstart(tp);
	}
}

/*
 * Start (restart) transmission on the given DMF32 line.
 */
dmfstart(tp)
	register struct tty *tp;
{
	register struct dmfdevice *addr;
	register int car, dmf, unit, nch;
	int s;

	unit = minor(tp->t_dev);
	dmf = unit >> 3;
	unit &= 07;
	addr = (struct dmfdevice *)tp->t_addr;

	/*
	 * Must hold interrupts in following code to prevent
	 * state of the tp from changing.
	 */
	s = spl5();
	/*
	 * If it's currently active, or delaying, no need to do anything.
	 */
	if (tp->t_state&(TIMEOUT|BUSY|TTSTOP))
		goto out;
	/*
	 * If there are still characters in the silo,
	 * just reenable the transmitter.
	 */
	addr->dmfcsr = DMF_IE | DMFIR_TBUF | unit;
	if (addr->dmftsc) {
		addr->dmfcsr = DMF_IE | DMFIR_LCR | unit;
		addr->dmflcr |= DMF_TE;
		tp->t_state |= BUSY;
		goto out;
	}
	/*
	 * If there are sleepers, and output has drained below low
	 * water mark, wake up the sleepers.
	 */
	if ((tp->t_state&ASLEEP) && tp->t_outq.c_cc<=TTLOWAT(tp)) {
		tp->t_state &= ~ASLEEP;
		wakeup((caddr_t)&tp->t_outq);
	}
	/*
	 * Now restart transmission unless the output queue is
	 * empty.
	 */
	if (tp->t_outq.c_cc == 0)
		goto out;
	if (tp->t_flags&RAW || tp->t_local&LLITOUT)
		nch = ndqb(&tp->t_outq, 0);
	else {
		nch = ndqb(&tp->t_outq, 0200);
		/*
		 * If first thing on queue is a delay process it.
		 */
		if (nch == 0) {
			nch = getc(&tp->t_outq);
			timeout(ttrstrt, (caddr_t)tp, (nch&0x7f)+6);
			tp->t_state |= TIMEOUT;
			goto out;
		}
	}
	/*
	 * If characters to transmit, restart transmission.
	 */
	if (nch) {
#ifdef DMFDMA
		addr->dmfcsr = DMF_IE | DMFIR_LCR | unit;
		addr->dmflcr |= DMF_TE;
		car = UBACVT(tp->t_outq.c_cf, dmfinfo[dmf]->ui_ubanum);
		addr->dmfcsr = DMF_IE | DMFIR_TBA | unit;
		addr->dmftba = car;
		addr->dmftcc = ((car>>2)&0xc000) | nch;
#else
		register char *cp = tp->t_outq.c_cf;
		register int i;

		nch = MIN(nch, DMF_SILOCNT);
		addr->dmfcsr = DMF_IE | DMFIR_LCR | unit;
		addr->dmflcr |= DMF_TE;
		addr->dmfcsr = DMF_IE | DMFIR_TBUF | unit;
		for (i = 0; i < nch; i++)
			addr->dmftbuf = *cp++;
		ndflush(&tp->t_outq, nch);
#endif
		tp->t_state |= BUSY;
	}
out:
	splx(s);
}

/*
 * Stop output on a line, e.g. for ^S/^Q or output flush.
 */
/*ARGSUSED*/
dmfstop(tp, flag)
	register struct tty *tp;
{
	register struct dmfdevice *addr;
	register int unit, s;

	addr = (struct dmfdevice *)tp->t_addr;
	/*
	 * Block input/output interrupts while messing with state.
	 */
	s = spl5();
	if (tp->t_state & BUSY) {
		/*
		 * Device is transmitting; stop output
		 * by selecting the line and disabling
		 * the transmitter.  If this is a flush
		 * request then flush the output silo,
		 * otherwise we will pick up where we
		 * left off by enabling the transmitter.
		 */
		unit = minor(tp->t_dev);
		addr->dmfcsr = DMFIR_LCR | (unit&07) | DMF_IE;
		addr->dmflcr &= ~DMF_TE;
		if ((tp->t_state&TTSTOP)==0) {
			tp->t_state |= FLUSH;
			addr->dmflcr |= DMF_FLUSH;
		} else
			tp->t_state &= ~BUSY;
	}
	splx(s);
}

/*
 * DMF32 modem control
 */
dmfmctl(dev, bits, how)
	dev_t dev;
	int bits, how;
{
	register struct dmfdevice *dmfaddr;
	register int unit, mbits, lcr;
	int s;

	unit = minor(dev);
	dmfaddr = (struct dmfdevice *)(dmf_tty[unit].t_addr);
	unit &= 07;
	s = spl5();
	dmfaddr->dmfcsr = DMF_IE | DMFIR_TBUF | unit;
	mbits = dmfaddr->dmfrms << 8;
	dmfaddr->dmfcsr = DMF_IE | DMFIR_LCR | unit;
	mbits |= dmfaddr->dmftms;
	lcr = dmfaddr->dmflcr;
	switch (how) {
	case DMSET:
		mbits = bits;
		break;

	case DMBIS:
		mbits |= bits;
		break;

	case DMBIC:
		mbits &= ~bits;
		break;

	case DMGET:
		(void) splx(s);
		return(mbits);
	}
	dmfaddr->dmftms = mbits&037;
	if (mbits & DMF_BRK)
		lcr |= DMF_RBRK;
	else
		lcr &= ~DMF_RBRK;
	dmfaddr->dmflcr = lcr;
	(void) splx(s);
	return(mbits);
}

/*
 * Reset state of driver if UBA reset was necessary.
 * Reset the csr, lpr, and lcr registers on open lines, and
 * restart transmitters.
 */
dmfreset(uban)
	int uban;
{
	register int dmf, unit;
	register struct tty *tp;
	register struct uba_device *ui;
	register struct dmfdevice *addr;
	int i;

#ifdef DMFDMA
	if (dmf_ubinfo[uban] == 0)
		return;
	ubarelse(uban, &dmf_ubinfo[uban]);
	dmf_ubinfo[uban] = uballoc(uban, (caddr_t)cfree,
	    nclist*sizeof (struct cblock), 0);
	cbase[uban] = dmf_ubinfo[uban]&0x3ffff;
#endif
	for (dmf = 0; dmf < NDMF; dmf++) {
		ui = dmfinfo[dmf];
		if (ui == 0 || ui->ui_alive == 0 || ui->ui_ubanum != uban)
			continue;
		printf(" dmf%d", dmf);
		addr = (struct dmfdevice *)ui->ui_addr;
		addr->dmfcsr = DMF_IE;
		addr->dmfrsp = 1;
		unit = dmf * 8;
		for (i = 0; i < 8; i++) {
			tp = &dmf_tty[unit];
			if (tp->t_state & (ISOPEN|WOPEN)) {
				dmfparam(unit);
				dmfmctl(unit, DMF_ON, DMSET);
				tp->t_state &= ~BUSY;
				dmfstart(tp);
			}
			unit++;
		}
	}
}

/* stubs for interrupt routines for devices not yet supported */

dmfsrint() { printf("dmfsrint\n"); }

dmfsxint() { printf("dmfsxint\n"); }

dmfdaint() { printf("dmfdaint\n"); }

dmfdbint() { printf("dmfdbint\n"); }

dmflint() { printf("dmflint\n"); }
#endif
