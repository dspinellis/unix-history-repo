/*	dmf.c	6.6	85/06/04	*/

#include "dmf.h"
#if NDMF > 0
/*
 * DMF32 driver
 *
 *
 * TODO:
 *	test with modem
 *	load as much as possible into silo
 *	use auto XON/XOFF
 *	test reset code
 ****************************
 * DMF32 line printer driver
 *
 * the line printer on dmfx is indicated by a minor device code of 128+x
 *
 * the flags field of the config file is interpreted like so:
 * bits		meaning
 * ----		-------
 * 0-7		soft carrier bits for ttys part of dmf32
 * 8-15		number of cols/line on the line printer
 *			if 0, 132 will be used.
 * 16-23	number of lines/page on the line printer
 *			if 0, 66 will be used.
 *
 */
#include "../machine/pte.h"

#include "bk.h"
#include "uba.h"
#include "param.h"
#include "conf.h"
#include "dir.h"
#include "user.h"
#include "ioctl.h"
#include "tty.h"
#include "map.h"
#include "buf.h"
#include "vm.h"
#include "bkmac.h"
#include "clist.h"
#include "file.h"
#include "uio.h"
#include "kernel.h"
#include "syslog.h"

#include "ubareg.h"
#include "ubavar.h"
#include "dmfreg.h"

/*
 * Definition of the driver for the auto-configuration program.
 */
int	dmfprobe(), dmfattach(), dmfrint(), dmfxint();
int	dmflint();
struct	uba_device *dmfinfo[NDMF];
u_short	dmfstd[] = { 0 };
struct	uba_driver dmfdriver =
	{ dmfprobe, 0, dmfattach, 0, dmfstd, "dmf", dmfinfo };

int	dmf_timeout = 50;		/* silo timeout, in ms */
int	dmf_mindma = 4;			/* don't dma below this point */

/*
 * Local variables for the driver
 */
char	dmf_speeds[] =
	{ 0, 0, 1, 2, 3, 4, 0, 5, 6, 7, 010, 012, 014, 016, 017, 0 };

struct	tty dmf_tty[NDMF*8];
char	dmfsoftCAR[NDMF];

struct dmfl_softc
{
	unsigned dmfl_state; 		/* soft state bits */
	unsigned dmfl_info;		/* uba info */
	unsigned short dmfl_lines;	/* lines per page (66 def.) */
	unsigned short dmfl_cols; 	/* cols per line (132 def.) */
	char dmfl_buf[DMFL_BUFSIZ];
} dmfl_softc[NDMF];

/*
 * convert device number into DMF line printer unit number
 */
#define	DMFL_UNIT(d)	(minor(d)&0xF)	/* up to 16 DMFs */

#define ASLP 1		/* waiting for interrupt from dmf */
#define OPEN 2		/* line printer is open */
#define ERROR 4		/* error while printing, driver
			 refuses to do anything till closed */

#ifndef lint
int	ndmf = NDMF*8;			/* used by iostat */
#endif
int	dmfact;				/* mask of active dmf's */
int	dmfstart(), ttrstrt();

/*
 * The clist space is mapped by the driver onto each UNIBUS.
 * The UBACVT macro converts a clist space address for unibus uban
 * into an i/o space address for the DMA routine.
 */
int	dmf_ubinfo[NUBA];		/* info about allocated unibus map */
static int cbase[NUBA];			/* base address in unibus map */
#define	UBACVT(x, uban)		(cbase[uban] + ((x)-(char *)cfree))
char	dmf_dma[NDMF*8];

/*
 * Routine for configuration to set dmf interrupt.
 */
/*ARGSUSED*/
dmfprobe(reg, ctlr)
	caddr_t reg;
	struct uba_device *ctlr;
{
	register int br, cvec;		/* these are ``value-result'' */
	register struct dmfdevice *dmfaddr = (struct dmfdevice *)reg;
	register int i;
	register unsigned int a;
	static char *dmfdevs[]=
		{"parallel","printer","synch","asynch"};
	unsigned int dmfoptions;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	dmfxint(0); dmfrint(0);
	dmfsrint(); dmfsxint(); dmfdaint(); dmfdbint(); dmflint();
#endif
	/*
	 * Pick the usual size DMF vector here (don't decrement it here).
	 * grab configuration; note that the DMF32
	 * doesn't seem to put the right bits in this
	 * register until AFTER the interrupt vector is set.
	 */
	br = 0x15;
	cvec = (uba_hd[numuba].uh_lastiv - 4*8);
	dmfaddr->dmfccsr0 = (cvec >> 2) ;
	dmfoptions = dmfaddr->dmfccsr0 & DMFC_CONFMASK;

	/* catch a couple of special cases:  Able vmz/32n and vmz/lp	*/
	if (dmfoptions == DMFC_ASYNC) {
		/* Async portion only  -  vmz/32n */
		/* device dmf0 csr 0160400	vector dmfrint dmfxint  */
		cvec = (uba_hd[numuba].uh_lastiv -= 8);	/* only 8 bytes req'd */
		dmfaddr->dmfccsr0 = (cvec - 2*8) >> 2;	/* program 020 less   */
		printf("dmf%d: Able vmz32/n\n", ctlr->ui_unit);
	}
	else if (dmfoptions == DMFC_LP) {
		/* LP portion only - vmz/lp */
		/* device dmf0 csr 0160400	vector dmflint */

		cvec = (uba_hd[numuba].uh_lastiv -= 8);	/* only 8 bytes req'd */
		dmfaddr->dmfccsr0 = (cvec - 3*8) >> 2;	/* program 030 less   */
		printf("dmf%d: Able vmz/lp\n", ctlr->ui_unit);
	}
	else {
		/* anything else we program like a dec dmf32	*/
		/* device dmf0 vector dmfsrint dmfsxint dmfdaint dmfdbint dmfrint dmfxint dmflint */

		cvec = (uba_hd[numuba].uh_lastiv -= 4*8);
		dmfaddr->dmfccsr0 = (cvec >> 2) ;
		a = (dmfaddr->dmfccsr0>>12) & 0xf;
		printf("dmf%d:", ctlr->ui_unit);
		for(i=0;a != 0;++i,a >>= 1) {
			if(a&1)
				printf(" %s",dmfdevs[i]);
		}
		printf(".\n");
	}

	if (dmfoptions & DMFC_LP)
		dmfaddr->dmfl[0] = DMFL_RESET;
	/* NEED TO SAVE IT SOMEWHERE FOR OTHER DEVICES */
	return (sizeof (struct dmfdevice));
}

/*
 * Routine called to attach a dmf.
 */
dmfattach(ui)
	struct uba_device *ui;
{
	register int cols = (ui->ui_flags>>8) & 0xff;
	register int lines = (ui->ui_flags>>16) & 0xff;

	dmfsoftCAR[ui->ui_unit] = ui->ui_flags & 0xff;
	dmfl_softc[ui->ui_unit].dmfl_cols = cols==0?DMFL_DEFCOLS:cols;
	dmfl_softc[ui->ui_unit].dmfl_lines = lines==0?DMFL_DEFLINES:lines;
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
	if(unit & 0200)
		return(dmflopen(dev,flag));
	dmf = unit >> 3;
	if (unit >= NDMF*8 || (ui = dmfinfo[dmf])== 0 || ui->ui_alive == 0)
		return (ENXIO);
	tp = &dmf_tty[unit];
	if (tp->t_state&TS_XCLUDE && u.u_uid!=0)
		return (EBUSY);
	addr = (struct dmfdevice *)ui->ui_addr;
	tp->t_addr = (caddr_t)addr;
	tp->t_oproc = dmfstart;
	tp->t_state |= TS_WOPEN;
	/*
	 * While setting up state for this uba and this dmf,
	 * block uba resets which can clear the state.
	 */
	s = spltty();
	if (dmf_ubinfo[ui->ui_ubanum] == 0) {
		dmf_ubinfo[ui->ui_ubanum] =
		    uballoc(ui->ui_ubanum, (caddr_t)cfree,
			nclist*sizeof(struct cblock), 0);
		cbase[ui->ui_ubanum] = dmf_ubinfo[ui->ui_ubanum]&0x3ffff;
	}
	if ((dmfact&(1<<dmf)) == 0) {
		addr->dmfcsr |= DMF_IE;
		dmfact |= (1<<dmf);
		addr->dmfrsp = dmf_timeout;
	}
	splx(s);
	/*
	 * If this is first open, initialze tty state to default.
	 */
	if ((tp->t_state&TS_ISOPEN) == 0) {
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
		tp->t_state |= TS_CARR_ON;
	s = spltty();
	while ((tp->t_state & TS_CARR_ON) == 0) {
		tp->t_state |= TS_WOPEN;
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	}
	splx(s);
	return ((*linesw[tp->t_line].l_open)(dev, tp));
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
	if(unit & 0200)
		return(dmflclose(dev,flag));
		
	tp = &dmf_tty[unit];
	(*linesw[tp->t_line].l_close)(tp);
	(void) dmfmctl(unit, DMF_BRK, DMBIC);
	if (tp->t_state&TS_HUPCLS || (tp->t_state&TS_ISOPEN)==0)
		(void) dmfmctl(unit, DMF_OFF, DMSET);
	ttyclose(tp);
}

dmfread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;

	if(minor(dev)&0200)
		return(ENXIO);
	tp = &dmf_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_read)(tp, uio));
}

dmfwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;

	if(minor(dev)&0200)
		return(dmflwrite(dev,uio));
	tp = &dmf_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_write)(tp, uio));
}

/*
 * DMF32 receiver interrupt.
 */
dmfrint(dmf)
	int dmf;
{
	register c;
	register struct dmfdevice *addr;
	register struct tty *tp0;
	register dev;
	int unit;
	int overrun = 0, s;

	{
		register struct uba_device *ui;

		ui = dmfinfo[dmf];
		if (ui == 0 || ui->ui_alive == 0)
			return;
		addr = (struct dmfdevice *)ui->ui_addr;
	}
	tp0 = &dmf_tty[dmf * 8];
	/*
	 * Loop fetching characters from the silo for this
	 * dmf until there are no more in the silo.
	 */
	while ((c = addr->dmfrbuf) < 0) {
		register struct tty *tp;

		unit = (c >> 8) & 07;
		tp = tp0 + unit;
		dev = unit + dmf * 8;
		if (c & DMF_DSC) {
			s = spltty();
			addr->dmfcsr = DMF_IE | DMFIR_TBUF | unit;
			if (addr->dmfrms & DMF_CAR) {
				if ((tp->t_state & TS_CARR_ON) == 0) {
					wakeup((caddr_t)&tp->t_rawq);
					tp->t_state |= TS_CARR_ON;
				}
			} else {
				if (tp->t_state & TS_CARR_ON) {
					gsignal(tp->t_pgrp, SIGHUP);
					gsignal(tp->t_pgrp, SIGCONT);
					addr->dmfcsr = DMF_IE | DMFIR_LCR |
						unit;
					addr->dmftms = 0;
					ttyflush(tp, FREAD|FWRITE);
				}
				tp->t_state &= ~TS_CARR_ON;
			}
			splx(s);
			continue;
		}
		if ((tp->t_state&TS_ISOPEN)==0) {
			wakeup((caddr_t)tp);
			continue;
		}
		if(c & (DMF_PE|DMF_DO|DMF_FE)) {
			if (c & DMF_PE)
				if ((tp->t_flags&(EVENP|ODDP))==EVENP
			 	|| (tp->t_flags&(EVENP|ODDP))==ODDP )
					continue;
			if ((c & DMF_DO) && overrun == 0) {
				log(KERN_RECOV, "dmf%d: silo overflow\n", dmf);
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
					c = tp->t_intrc;
		}
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
dmfioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register struct tty *tp;
	register int unit = minor(dev);
	int error;
 
	if(unit & 0200)
		return (ENOTTY);
	tp = &dmf_tty[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0) {
		if (cmd == TIOCSETP || cmd == TIOCSETN || cmd == TIOCLBIS ||
		    cmd == TIOCLBIC || cmd == TIOCLSET)
			dmfparam(unit);
		return (error);
	}
	switch (cmd) {

	case TIOCSBRK:
		(void) dmfmctl(dev, DMF_BRK, DMBIS);
		break;

	case TIOCCBRK:
		(void) dmfmctl(dev, DMF_BRK, DMBIC);
		break;

	case TIOCSDTR:
		(void) dmfmctl(dev, DMF_DTR|DMF_RTS, DMBIS);
		break;

	case TIOCCDTR:
		(void) dmfmctl(dev, DMF_DTR|DMF_RTS, DMBIC);
		break;

	case TIOCMSET:
		(void) dmfmctl(dev, dmtodmf(*(int *)data), DMSET);
		break;

	case TIOCMBIS:
		(void) dmfmctl(dev, dmtodmf(*(int *)data), DMBIS);
		break;

	case TIOCMBIC:
		(void) dmfmctl(dev, dmtodmf(*(int *)data), DMBIC);
		break;

	case TIOCMGET:
		*(int *)data = dmftodm(dmfmctl(dev, 0, DMGET));
		break;

	default:
		return (ENOTTY);
	}
	return (0);
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
	s = spltty();
	addr->dmfcsr = (unit&07) | DMFIR_LCR | DMF_IE;
	if ((tp->t_ispeed)==0) {
		tp->t_state |= TS_HUPCLS;
		(void) dmfmctl(unit, DMF_OFF, DMSET);
		return;
	}
	lpar = (dmf_speeds[tp->t_ospeed]<<12) | (dmf_speeds[tp->t_ispeed]<<8);
	lcr = DMFLCR_ENA;
	if ((tp->t_ispeed) == B134)
		lpar |= BITS6|PENABLE;
	else if (tp->t_flags & (RAW|LITOUT))
		lpar |= BITS8;
	else {
		lpar |= BITS7|PENABLE;
		/* CHECK FOR XON/XOFF AND SET lcr |= DMF_AUTOX; */
	}
	if (tp->t_flags&EVENP)
		lpar |= EPAR;
	if ((tp->t_ospeed) == B110)
		lpar |= TWOSB;
	lpar |= (unit&07);
	addr->dmflpr = lpar;
	SETLCR(addr, lcr);
	splx(s);
}

/*
 * DMF32 transmitter interrupt.
 * Restart the idle line.
 */
dmfxint(dmf)
	int dmf;
{
	int u = dmf * 8;
	struct tty *tp0 = &dmf_tty[u];
	register struct tty *tp;
	register struct dmfdevice *addr;
	register struct uba_device *ui;
	register int t;
	short cntr;

	ui = dmfinfo[dmf];
	addr = (struct dmfdevice *)ui->ui_addr;
	while ((t = addr->dmfcsr) & DMF_TI) {
		if (t & DMF_NXM)
			/* SHOULD RESTART OR SOMETHING... */
			printf("dmf%d: NXM line %d\n", dmf, t >> 8 & 7);
		t = t >> 8 & 7;
		tp = tp0 + t;
		tp->t_state &= ~TS_BUSY;
		if (tp->t_state&TS_FLUSH)
			tp->t_state &= ~TS_FLUSH;
		else if (dmf_dma[u + t]) {
			/*
			 * Do arithmetic in a short to make up
			 * for lost 16&17 bits.
			 */
			addr->dmfcsr = DMFIR_TBA | DMF_IE | t;
			cntr = addr->dmftba -
			    UBACVT(tp->t_outq.c_cf, ui->ui_ubanum);
			ndflush(&tp->t_outq, (int)cntr);
		}
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
	register int unit, nch;
	int s;
	register int dmf;

	unit = minor(tp->t_dev);
	dmf = unit >> 3;
	unit &= 07;
	addr = (struct dmfdevice *)tp->t_addr;

	/*
	 * Must hold interrupts in following code to prevent
	 * state of the tp from changing.
	 */
	s = spltty();
	/*
	 * If it's currently active, or delaying, no need to do anything.
	 */
	if (tp->t_state&(TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;
	/*
	 * If there are still characters in the silo,
	 * just reenable the transmitter.
	 */
	addr->dmfcsr = DMF_IE | DMFIR_TBUF | unit;
	if (addr->dmftsc) {
		addr->dmfcsr = DMF_IE | DMFIR_LCR | unit;
		SETLCR(addr, addr->dmflcr|DMF_TE);
		tp->t_state |= TS_BUSY;
		goto out;
	}
	/*
	 * If there are sleepers, and output has drained below low
	 * water mark, wake up the sleepers.
	 */
	if (tp->t_outq.c_cc<=TTLOWAT(tp)) {
		if (tp->t_state&TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		if (tp->t_wsel) {
			selwakeup(tp->t_wsel, tp->t_state & TS_WCOLL);
			tp->t_wsel = 0;
			tp->t_state &= ~TS_WCOLL;
		}
	}
	/*
	 * Now restart transmission unless the output queue is
	 * empty.
	 */
	if (tp->t_outq.c_cc == 0)
		goto out;
	if (tp->t_flags & (RAW|LITOUT))
		nch = ndqb(&tp->t_outq, 0);
	else {
		if ((nch = ndqb(&tp->t_outq, 0200)) == 0) {
			/*
		 	* If first thing on queue is a delay process it.
		 	*/
			nch = getc(&tp->t_outq);
			timeout(ttrstrt, (caddr_t)tp, (nch&0x7f)+6);
			tp->t_state |= TS_TIMEOUT;
			goto out;
		}
	}
	/*
	 * If characters to transmit, restart transmission.
	 */
	if (nch >= dmf_mindma) {
		register car;

		dmf_dma[minor(tp->t_dev)] = 1;
		addr->dmfcsr = DMF_IE | DMFIR_LCR | unit;
		SETLCR(addr, addr->dmflcr|DMF_TE);
		car = UBACVT(tp->t_outq.c_cf, dmfinfo[dmf]->ui_ubanum);
		addr->dmfcsr = DMF_IE | DMFIR_TBA | unit;
		addr->dmftba = car;
		addr->dmftcc = ((car >> 2) & 0xc000) | nch;
		tp->t_state |= TS_BUSY;
	} else if (nch) {
		register char *cp = tp->t_outq.c_cf;
		register int i;

		dmf_dma[minor(tp->t_dev)] = 0;
		nch = MIN(nch, DMF_SILOCNT);
		addr->dmfcsr = DMF_IE | DMFIR_LCR | unit;
		SETLCR(addr, addr->dmflcr|DMF_TE);
		addr->dmfcsr = DMF_IE | DMFIR_TBUF | unit;
		for (i = 0; i < nch; i++)
			addr->dmftbuf = *cp++;
		ndflush(&tp->t_outq, nch);
		tp->t_state |= TS_BUSY;
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
	register unit = minor(tp->t_dev) & 7;
	int s;

	addr = (struct dmfdevice *)tp->t_addr;
	/*
	 * Block input/output interrupts while messing with state.
	 */
	s = spltty();
	if (flag) {
		addr->dmfcsr = DMF_IE | DMFIR_TBUF | unit;
		if (addr->dmftsc) {
			/*
			 * Flush regardless of whether we're transmitting
			 * (TS_BUSY), if the silo contains untransmitted
			 * characters.
			 */
			addr->dmfcsr = DMFIR_LCR | unit | DMF_IE;
			SETLCR(addr, addr->dmflcr | DMF_TE | DMF_FLUSH);
			/* this will interrupt so let dmfxint handle the rest */
			tp->t_state |= TS_FLUSH|TS_BUSY;
		}
	} else {
		if (tp->t_state & TS_BUSY) {
			/*
			 * Stop transmission by disabling
			 * the transmitter.  We'll pick up where we
			 * left off by reenabling in dmfstart.
			 */
			addr->dmfcsr = DMFIR_LCR | unit | DMF_IE;
			SETLCR(addr, addr->dmflcr &~ DMF_TE);
			/* no interrupt here */
			tp->t_state &= ~TS_BUSY;
		}
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
	s = spltty();
	dmfaddr->dmfcsr = DMF_IE | DMFIR_TBUF | unit;
	mbits = dmfaddr->dmfrms << 8;
	dmfaddr->dmfcsr = DMF_IE | DMFIR_LCR | unit;
	mbits |= dmfaddr->dmftms;
	lcr = dmfaddr->dmflcr;
	switch (how) {
	case DMSET:
		mbits = (mbits &0xff00) | bits;
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
	if (mbits & DMF_BRK)
		lcr |= DMF_RBRK;
	else
		lcr &= ~DMF_RBRK;
	lcr = ((mbits & 037) << 8) | (lcr & 0xff);
	dmfaddr->dmfun.dmfirw = lcr;
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

	if (dmf_ubinfo[uban] == 0)
		return;
	dmf_ubinfo[uban] = uballoc(uban, (caddr_t)cfree,
	    nclist*sizeof (struct cblock), 0);
	cbase[uban] = dmf_ubinfo[uban]&0x3ffff;
	for (dmf = 0; dmf < NDMF; dmf++) {
		ui = dmfinfo[dmf];
		if (ui == 0 || ui->ui_alive == 0 || ui->ui_ubanum != uban)
			continue;
		printf(" dmf%d", dmf);
		addr = (struct dmfdevice *)ui->ui_addr;
		addr->dmfcsr = DMF_IE;
		addr->dmfrsp = dmf_timeout;
		unit = dmf * 8;
		for (i = 0; i < 8; i++) {
			tp = &dmf_tty[unit];
			if (tp->t_state & (TS_ISOPEN|TS_WOPEN)) {
				dmfparam(unit);
				(void) dmfmctl(unit, DMF_ON, DMSET);
				tp->t_state &= ~TS_BUSY;
				dmfstart(tp);
			}
			unit++;
		}
	}
}

/* dmflopen -- open the line printer port on a dmf32
 *
 */
dmflopen(dev,flag)
dev_t dev;
int flag;
{
	register int dmf;
	register struct dmfl_softc *sc;
	register struct uba_device *ui;
	register struct dmfdevice *addr;


	dmf = DMFL_UNIT(dev) ;
	if(((sc= &dmfl_softc[dmf])->dmfl_state & OPEN) ||
		((ui=dmfinfo[dmf]) == 0) || ui->ui_alive == 0)
			return(ENXIO);
	addr = (struct dmfdevice *)ui->ui_addr;
	if((addr->dmfl[0] & DMFL_OFFLINE))
	{
		/*printf("dmf: line printer offline/jammed\n");*/
		return(EIO);
	}
	if((addr->dmfl[0]&DMFL_CONV))
	{
		printf("dmf:line printer disconnected\n");
		return(EIO);
	}

	addr->dmfl[0] = 0;
	sc->dmfl_state |= OPEN;
	return 0;
}

dmflclose(dev,flag)
dev_t dev;
int flag;
{
	register int dmf= DMFL_UNIT(dev);
	register struct dmfl_softc *sc = &dmfl_softc[dmf];

	dmflout(dev,"\f",1);
	sc->dmfl_state = 0;
	if(sc->dmfl_info != 0)
		ubarelse((struct dmfdevice *)(dmfinfo[dmf])->ui_ubanum,
			&(sc->dmfl_info));

	((struct dmfdevice *)(dmfinfo[dmf]->ui_addr))->dmfl[0]=0;
	return 0;
}

dmflwrite(dev,uio)
dev_t dev;
struct uio *uio;
{
	register unsigned int n;
	register int error;
	register struct dmfl_softc *sc;

	sc = &dmfl_softc[DMFL_UNIT(dev)];
	if(sc->dmfl_state&ERROR) return(EIO);
	while(n=min(DMFL_BUFSIZ,(unsigned)uio->uio_resid))
	{
		if(error=uiomove(&sc->dmfl_buf[0],(int)n,
			UIO_WRITE,uio))
		{
			printf("uio move error\n");
			return(error);
		}
		if(error=dmflout(dev,&sc->dmfl_buf[0],n))
		{
			return(error);
		}
	}
	return 0;
}


/* dmflout -- start io operation to dmf line printer
 *		cp is addr of buf of n chars to be sent.
 *
 *	-- dmf will be put in formatted output mode, this will
 *		be selectable from an ioctl if the
 *		need ever arises.
 */
dmflout(dev,cp,n)
dev_t dev;
char *cp;
int n;
{
	register struct dmfl_softc *sc;
	register int dmf;
	register struct uba_device *ui;
	register struct dmfdevice *d;
	register unsigned info;
	register unsigned i;

	dmf = DMFL_UNIT(dev) ;
	sc= &dmfl_softc[dmf];
	if(sc->dmfl_state&ERROR) return(EIO);
	ui= dmfinfo[dmf];
	/* allocate unibus resources, will be released when io
	 * operation is done
	 */
	sc->dmfl_info=
	info=
		uballoc(ui->ui_ubanum,cp,n,0);
	d= (struct dmfdevice *)ui->ui_addr;
	d->dmfl[0] = (2<<8) | DMFL_FORMAT; /* indir reg 2 */
	/* indir reg auto increments on r/w */
	/* SO DON'T CHANGE THE ORDER OF THIS CODE */
	d->dmfl[1] = 0; /* prefix chars & num */
	d->dmfl[1] = 0; /* suffix chars & num */
	d->dmfl[1] = info; 	/* dma lo 16 bits addr */

	/* NOT DOCUMENTED !! */
	d->dmfl[1] = -n;		/* number of chars */
	/* ----------^-------- */

	d->dmfl[1] = ((info>>16)&3) /* dma hi 2 bits addr */
		| (1<<8) /* auto cr insert */
		| (1<<9) /* use real ff */
		| (1<<15); /* no u/l conversion */
	d->dmfl[1] = sc->dmfl_lines 	/* lines per page */
		| (sc->dmfl_cols<<8);	/* carriage width */
	sc->dmfl_state |= ASLP;
	i=spltty();
	d->dmfl[0] |= DMFL_PEN|DMFL_IE;
	while(sc->dmfl_state & ASLP)
	{
		sleep(&sc->dmfl_buf[0],(PZERO+8));
		while(sc->dmfl_state&ERROR)
		{
			timeout(dmflint,dmf,10*hz);
			sleep(&sc->dmfl_state,(PZERO+8));
		}
		/*if(sc->dmfl_state&ERROR) return (EIO);*/
	}
	splx(i);
	return(0);
}
/* dmflint -- handle an interrupt from the line printer part of the dmf32
 *
 */

dmflint(dmf)
int dmf;
{

	register struct uba_device *ui;
	register struct dmfl_softc *sc;
	register struct dmfdevice *d;

	ui= dmfinfo[dmf];
	sc= &dmfl_softc[dmf];
	d= (struct dmfdevice *)ui->ui_addr;

	d->dmfl[0] &= ~DMFL_IE;

	if(sc->dmfl_state&ERROR)
	{
		printf("dmfl: intr while in error state \n");
		if((d->dmfl[0]&DMFL_OFFLINE) == 0)
			sc->dmfl_state &= ~ERROR;
		wakeup(&sc->dmfl_state);
		return;
	}
	if(d->dmfl[0]&DMFL_DMAERR)
	{
		printf("dmf:NXM\n");
	}
	if(d->dmfl[0]&DMFL_OFFLINE)
	{
		printf("dmf:printer error\n");
		sc->dmfl_state |= ERROR;
	}
	if(d->dmfl[0]&DMFL_PDONE)
	{
#ifdef notdef
		printf("bytes= %d\n",d->dmfl[1]);
		printf("lines= %d\n",d->dmfl[1]);
#endif
	}
	sc->dmfl_state &= ~ASLP;
	wakeup(&sc->dmfl_buf[0]);
	if(sc->dmfl_info != 0)
		ubarelse(ui->ui_ubanum,&sc->dmfl_info);
	sc->dmfl_info = 0;

}

/* stubs for interrupt routines for devices not yet supported */

dmfsrint() { printf("dmfsrint\n"); }

dmfsxint() { printf("dmfsxint\n"); }

dmfdaint() { printf("dmfdaint\n"); }

dmfdbint() { printf("dmfdbint\n"); }


#endif
