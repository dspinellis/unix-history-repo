/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell and Rick Macklem.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)scc.c	8.2 (Berkeley) 11/30/93
 */

/* 
 * Mach Operating System
 * Copyright (c) 1991,1990,1989 Carnegie Mellon University
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS 
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 * 
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 * 
 * any improvements or extensions that they make and grant Carnegie the
 * rights to redistribute these changes.
 */

#include <scc.h>
#if NSCC > 0
/*
 * Intel 82530 dual usart chip driver. Supports the serial port(s) on the
 * Personal DECstation 5000/xx and DECstation 5000/1xx, plus the keyboard
 * and mouse on the 5000/1xx. (Don't ask me where the A channel signals
 * are on the 5000/xx.)
 *
 * See: Intel MicroCommunications Handbook, Section 2, pg. 155-173, 1992.
 */
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/proc.h>
#include <sys/map.h>
#include <sys/buf.h>
#include <sys/conf.h>
#include <sys/file.h>
#include <sys/uio.h>
#include <sys/kernel.h>
#include <sys/syslog.h>

#include <machine/pmioctl.h>

#include <pmax/dev/device.h>
#include <pmax/dev/pdma.h>
#include <pmax/dev/sccreg.h>
#include <pmax/dev/fbreg.h>

#include <pmax/pmax/cons.h>
#include <pmax/pmax/pmaxtype.h>

extern int pmax_boardtype;
extern struct consdev cn_tab;
extern void ttrstrt	__P((void *));
extern void KBDReset	__P((dev_t, void (*)()));
extern void MouseInit	__P((dev_t, void (*)(), int (*)()));

/*
 * Driver information for auto-configuration stuff.
 */
int	sccprobe(), sccopen(), sccparam(), sccGetc();
void	sccintr(), sccstart(), sccPutc();
struct	driver sccdriver = {
	"scc", sccprobe, 0, 0, sccintr,
};

#define	NSCCLINE 	(NSCC*2)
#define	SCCUNIT(dev)	(minor(dev) >> 1)
#define	SCCLINE(dev)	(minor(dev) & 0x1)

struct	tty scc_tty[NSCCLINE];
void	(*sccDivertXInput)();	/* X windows keyboard input routine */
void	(*sccMouseEvent)();	/* X windows mouse motion event routine */
void	(*sccMouseButtons)();	/* X windows mouse buttons event routine */
#ifdef DEBUG
int	debugChar;
#endif
static void scc_modem_intr(), sccreset();

struct scc_softc {
	struct pdma scc_pdma[2];
	struct {
		u_char	wr1;
		u_char	wr3;
		u_char	wr4;
		u_char	wr5;
		u_char	wr14;
	} scc_wreg[2];
	int	scc_softCAR;
} scc_softc[NSCC];

struct speedtab sccspeedtab[] = {
	0,	0,
	50,	4606,
	75,	3070,
	110,	2093,
	134,	1711,
	150,	1534,
	300,	766,
	600,	382,
	1200,	190,
	1800,	126,
	2400,	94,
	4800,	46,
	9600,	22,
	19200,	10,
	38400,	4,
	-1,	-1
};

#ifndef	PORTSELECTOR
#define	ISPEED	TTYDEF_SPEED
#define	LFLAG	TTYDEF_LFLAG
#else
#define	ISPEED	B4800
#define	LFLAG	(TTYDEF_LFLAG & ~ECHO)
#endif

/*
 * Test to see if device is present.
 * Return true if found and initialized ok.
 */
sccprobe(cp)
	register struct pmax_ctlr *cp;
{
	register struct scc_softc *sc;
	register struct pdma *pdp;
	register struct tty *tp;
	register int cntr;
	struct tty ctty;
	struct termios cterm;
	int s;

	if (cp->pmax_unit >= NSCC)
		return (0);
	if (badaddr(cp->pmax_addr, 2))
		return (0);

	/*
	 * For a remote console, wait a while for previous output to
	 * complete.
	 */
	if (major(cn_tab.cn_dev) == SCCDEV && cn_tab.cn_screen == 0 &&
		SCCUNIT(cn_tab.cn_dev) == cp->pmax_unit)
		DELAY(10000);

	sc = &scc_softc[cp->pmax_unit];
	pdp = &sc->scc_pdma[0];

	/* init pseudo DMA structures */
	tp = &scc_tty[cp->pmax_unit * 2];
	for (cntr = 0; cntr < 2; cntr++) {
		pdp->p_addr = (void *)cp->pmax_addr;
		pdp->p_arg = (int)tp;
		pdp->p_fcn = (void (*)())0;
		tp->t_dev = (dev_t)((cp->pmax_unit << 1) | cntr);
		pdp++, tp++;
	}
	sc->scc_softCAR = cp->pmax_flags | 0x2;

	/* reset chip */
	sccreset(sc);

	/*
	 * Special handling for consoles.
	 */
	if (cn_tab.cn_screen) {
		if (cn_tab.cn_kbdgetc == sccGetc) {
			if (cp->pmax_unit == 1) {
				s = spltty();
				ctty.t_dev = makedev(SCCDEV, SCCKBD_PORT);
				cterm.c_cflag = CS8;
				cterm.c_ospeed = cterm.c_ispeed = 4800;
				(void) sccparam(&ctty, &cterm);
				DELAY(10000);
#ifdef notyet
				/*
				 * For some reason doing this hangs the 3min
				 * during booting. Fortunately the keyboard
				 * works ok without it.
				 */
				KBDReset(ctty.t_dev, sccPutc);
#endif
				DELAY(10000);
				splx(s);
			} else if (cp->pmax_unit == 0) {
				s = spltty();
				ctty.t_dev = makedev(SCCDEV, SCCMOUSE_PORT);
				cterm.c_cflag = CS8 | PARENB | PARODD;
				cterm.c_ospeed = cterm.c_ispeed = 4800;
				(void) sccparam(&ctty, &cterm);
				DELAY(10000);
				MouseInit(ctty.t_dev, sccPutc, sccGetc);
				DELAY(10000);
				splx(s);
			}
		}
	} else if (SCCUNIT(cn_tab.cn_dev) == cp->pmax_unit) {
		s = spltty();
		ctty.t_dev = cn_tab.cn_dev;
		cterm.c_cflag = CS8;
		cterm.c_ospeed = cterm.c_ispeed = 9600;
		(void) sccparam(&ctty, &cterm);
		DELAY(1000);
		cn_tab.cn_disabled = 0;
		splx(s);
	}
	printf("scc%d at nexus0 csr 0x%x priority %d\n",
		cp->pmax_unit, cp->pmax_addr, cp->pmax_pri);
	return (1);
}

/*
 * Reset the chip.
 */
static void
sccreset(sc)
	register struct scc_softc *sc;
{
	register scc_regmap_t *regs;
	register u_char val;

	regs = (scc_regmap_t *)sc->scc_pdma[0].p_addr;
	/*
	 * Chip once-only initialization
	 *
	 * NOTE: The wiring we assume is the one on the 3min:
	 *
	 *	out	A-TxD	-->	TxD	keybd or mouse
	 *	in	A-RxD	-->	RxD	keybd or mouse
	 *	out	A-DTR~	-->	DTR	comm
	 *	out	A-RTS~	-->	RTS	comm
	 *	in	A-CTS~	-->	SI	comm
	 *	in	A-DCD~	-->	RI	comm
	 *	in	A-SYNCH~-->	DSR	comm
	 *	out	B-TxD	-->	TxD	comm
	 *	in	B-RxD	-->	RxD	comm
	 *	in	B-RxC	-->	TRxCB	comm
	 *	in	B-TxC	-->	RTxCB	comm
	 *	out	B-RTS~	-->	SS	comm
	 *	in	B-CTS~	-->	CTS	comm
	 *	in	B-DCD~	-->	CD	comm
	 */
	SCC_INIT_REG(regs, SCC_CHANNEL_A);
	SCC_INIT_REG(regs, SCC_CHANNEL_B);
	SCC_WRITE_REG(regs, SCC_CHANNEL_A, SCC_WR9, SCC_WR9_HW_RESET);
	DELAY(50000);	/*enough ? */
	SCC_WRITE_REG(regs, SCC_CHANNEL_A, SCC_WR9, 0);

	/* program the interrupt vector */
	SCC_WRITE_REG(regs, SCC_CHANNEL_A, SCC_WR2, 0xf0);
	SCC_WRITE_REG(regs, SCC_CHANNEL_B, SCC_WR2, 0xf0);
	SCC_WRITE_REG(regs, SCC_CHANNEL_A, SCC_WR9, SCC_WR9_VIS);

	/* timing base defaults */
	sc->scc_wreg[SCC_CHANNEL_A].wr4 = SCC_WR4_CLK_x16;
	sc->scc_wreg[SCC_CHANNEL_B].wr4 = SCC_WR4_CLK_x16;

	/* enable DTR, RTS and SS */
	sc->scc_wreg[SCC_CHANNEL_B].wr5 = SCC_WR5_RTS;
	sc->scc_wreg[SCC_CHANNEL_A].wr5 = SCC_WR5_RTS | SCC_WR5_DTR;

	/* baud rates */
	val = SCC_WR14_BAUDR_ENABLE|SCC_WR14_BAUDR_SRC;
	sc->scc_wreg[SCC_CHANNEL_B].wr14 = val;
	sc->scc_wreg[SCC_CHANNEL_A].wr14 = val;

	/* interrupt conditions */
	val =	SCC_WR1_RXI_ALL_CHAR | SCC_WR1_PARITY_IE |
		SCC_WR1_EXT_IE;
	sc->scc_wreg[SCC_CHANNEL_A].wr1 = val;
	sc->scc_wreg[SCC_CHANNEL_B].wr1 = val;
}

sccopen(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct scc_softc *sc;
	register struct tty *tp;
	register int unit, line;
	int s, error = 0;

	unit = SCCUNIT(dev);
	if (unit >= NSCC)
		return (ENXIO);
	line = SCCLINE(dev);
	sc = &scc_softc[unit];
	if (sc->scc_pdma[line].p_addr == (void *)0)
		return (ENXIO);
	tp = &scc_tty[minor(dev)];
	tp->t_oproc = sccstart;
	tp->t_param = sccparam;
	tp->t_dev = dev;
	if ((tp->t_state & TS_ISOPEN) == 0) {
		tp->t_state |= TS_WOPEN;
		ttychars(tp);
#ifndef PORTSELECTOR
		if (tp->t_ispeed == 0) {
#endif
			tp->t_iflag = TTYDEF_IFLAG;
			tp->t_oflag = TTYDEF_OFLAG;
			tp->t_cflag = TTYDEF_CFLAG;
			tp->t_lflag = LFLAG;
			tp->t_ispeed = tp->t_ospeed = ISPEED;
#ifdef PORTSELECTOR
			tp->t_cflag |= HUPCL;
#else
		}
#endif
		(void) sccparam(tp, &tp->t_termios);
		ttsetwater(tp);
	} else if ((tp->t_state & TS_XCLUDE) && curproc->p_ucred->cr_uid != 0)
		return (EBUSY);
	(void) sccmctl(dev, DML_DTR, DMSET);
	s = spltty();
	while (!(flag & O_NONBLOCK) && !(tp->t_cflag & CLOCAL) &&
	       !(tp->t_state & TS_CARR_ON)) {
		tp->t_state |= TS_WOPEN;
		if (error = ttysleep(tp, (caddr_t)&tp->t_rawq, TTIPRI | PCATCH,
		    ttopen, 0))
			break;
	}
	splx(s);
	if (error)
		return (error);
	return ((*linesw[tp->t_line].l_open)(dev, tp));
}

/*ARGSUSED*/
sccclose(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct scc_softc *sc = &scc_softc[SCCUNIT(dev)];
	register struct tty *tp;
	register int bit, line;

	tp = &scc_tty[minor(dev)];
	line = SCCLINE(dev);
	if (sc->scc_wreg[line].wr5 & SCC_WR5_SEND_BREAK) {
		sc->scc_wreg[line].wr5 &= ~SCC_WR5_SEND_BREAK;
		ttyoutput(0, tp);
	}
	(*linesw[tp->t_line].l_close)(tp, flag);
	if ((tp->t_cflag & HUPCL) || (tp->t_state & TS_WOPEN) ||
	    !(tp->t_state & TS_ISOPEN))
		(void) sccmctl(dev, 0, DMSET);
	return (ttyclose(tp));
}

sccread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;

	tp = &scc_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}

sccwrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;

	tp = &scc_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}

/*ARGSUSED*/
sccioctl(dev, cmd, data, flag, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
	struct proc *p;
{
	register struct scc_softc *sc;
	register struct tty *tp;
	int error, line;

	tp = &scc_tty[minor(dev)];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag, p);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	line = SCCLINE(dev);
	sc = &scc_softc[SCCUNIT(dev)];
	switch (cmd) {

	case TIOCSBRK:
		sc->scc_wreg[line].wr5 |= SCC_WR5_SEND_BREAK;
		ttyoutput(0, tp);
		break;

	case TIOCCBRK:
		sc->scc_wreg[line].wr5 &= ~SCC_WR5_SEND_BREAK;
		ttyoutput(0, tp);
		break;

	case TIOCSDTR:
		(void) sccmctl(dev, DML_DTR|DML_RTS, DMBIS);
		break;

	case TIOCCDTR:
		(void) sccmctl(dev, DML_DTR|DML_RTS, DMBIC);
		break;

	case TIOCMSET:
		(void) sccmctl(dev, *(int *)data, DMSET);
		break;

	case TIOCMBIS:
		(void) sccmctl(dev, *(int *)data, DMBIS);
		break;

	case TIOCMBIC:
		(void) sccmctl(dev, *(int *)data, DMBIC);
		break;

	case TIOCMGET:
		*(int *)data = sccmctl(dev, 0, DMGET);
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

sccparam(tp, t)
	register struct tty *tp;
	register struct termios *t;
{
	register struct scc_softc *sc;
	register scc_regmap_t *regs;
	register int line;
	register u_char value, wvalue;
	register int cflag = t->c_cflag;
	int ospeed;

        if (t->c_ispeed && t->c_ispeed != t->c_ospeed)
                return (EINVAL);
	sc = &scc_softc[SCCUNIT(tp->t_dev)];
	line = SCCLINE(tp->t_dev);
	regs = (scc_regmap_t *)sc->scc_pdma[line].p_addr;
	ospeed = ttspeedtab(t->c_ospeed, sccspeedtab);
        if (ospeed < 0)
                return (EINVAL);
        /* and copy to tty */
        tp->t_ispeed = t->c_ispeed;
        tp->t_ospeed = t->c_ospeed;
        tp->t_cflag = cflag;

	/*
	 * Handle console specially.
	 */
	if (cn_tab.cn_screen) {
		if (minor(tp->t_dev) == SCCKBD_PORT) {
			cflag = CS8;
			ospeed = ttspeedtab(4800, sccspeedtab);
		} else if (minor(tp->t_dev) == SCCMOUSE_PORT) {
			cflag = CS8 | PARENB | PARODD;
			ospeed = ttspeedtab(4800, sccspeedtab);
		}
	} else if (tp->t_dev == cn_tab.cn_dev) {
		cflag = CS8;
		ospeed = ttspeedtab(9600, sccspeedtab);
	}
	if (ospeed == 0) {
		(void) sccmctl(tp->t_dev, 0, DMSET);	/* hang up line */
		return (0);
	}

	/* reset line */
	if (line == SCC_CHANNEL_A)
		value = SCC_WR9_RESET_CHA_A;
	else
		value = SCC_WR9_RESET_CHA_B;
	SCC_WRITE_REG(regs, line, SCC_WR9, value);
	DELAY(25);

	/* stop bits, normally 1 */
	value = sc->scc_wreg[line].wr4 & 0xf0;
	if (cflag & CSTOPB)
		value |= SCC_WR4_2_STOP;
	else
		value |= SCC_WR4_1_STOP;
	if ((cflag & PARODD) == 0)
		value |= SCC_WR4_EVEN_PARITY;
	if (cflag & PARENB)
		value |= SCC_WR4_PARITY_ENABLE;

	/* set it now, remember it must be first after reset */
	sc->scc_wreg[line].wr4 = value;
	SCC_WRITE_REG(regs, line, SCC_WR4, value);

	/* vector again */
	SCC_WRITE_REG(regs, line, SCC_WR2, 0xf0);

	/* clear break, keep rts dtr */
	wvalue = sc->scc_wreg[line].wr5 & (SCC_WR5_DTR|SCC_WR5_RTS);
	switch (cflag & CSIZE) {
	case CS5:
		value = SCC_WR3_RX_5_BITS;
		wvalue |= SCC_WR5_TX_5_BITS;
		break;
	case CS6:
		value = SCC_WR3_RX_6_BITS;
		wvalue |= SCC_WR5_TX_6_BITS;
		break;
	case CS7:
		value = SCC_WR3_RX_7_BITS;
		wvalue |= SCC_WR5_TX_7_BITS;
		break;
	case CS8:
	default:
		value = SCC_WR3_RX_8_BITS;
		wvalue |= SCC_WR5_TX_8_BITS;
	};
	sc->scc_wreg[line].wr3 = value;
	SCC_WRITE_REG(regs, line, SCC_WR3, value);
	sc->scc_wreg[line].wr5 = wvalue;
	SCC_WRITE_REG(regs, line, SCC_WR5, wvalue);
	SCC_WRITE_REG(regs, line, SCC_WR6, 0);
	SCC_WRITE_REG(regs, line, SCC_WR7, 0);
	SCC_WRITE_REG(regs, line, SCC_WR9, SCC_WR9_VIS);
	SCC_WRITE_REG(regs, line, SCC_WR10, 0);
	value = SCC_WR11_RCLK_BAUDR | SCC_WR11_XTLK_BAUDR |
		SCC_WR11_TRc_OUT | SCC_WR11_TRcOUT_BAUDR;
	SCC_WRITE_REG(regs, line, SCC_WR11, value);
	SCC_SET_TIMING_BASE(regs, line, ospeed);
	value = sc->scc_wreg[line].wr14;
	SCC_WRITE_REG(regs, line, SCC_WR14, value);
	value = SCC_WR15_BREAK_IE | SCC_WR15_CTS_IE | SCC_WR15_DCD_IE;
	SCC_WRITE_REG(regs, line, SCC_WR15, value);

	/* and now the enables */
	value = sc->scc_wreg[line].wr3 | SCC_WR3_RX_ENABLE;
	SCC_WRITE_REG(regs, line, SCC_WR3, value);
	value = sc->scc_wreg[line].wr5 | SCC_WR5_TX_ENABLE;
	sc->scc_wreg[line].wr5 = value;
	SCC_WRITE_REG(regs, line, SCC_WR5, value);

	/* master inter enable */
	value = SCC_WR9_MASTER_IE | SCC_WR9_VIS;
	SCC_WRITE_REG(regs, line, SCC_WR9, value);
	SCC_WRITE_REG(regs, line, SCC_WR1, sc->scc_wreg[line].wr1);
	MachEmptyWriteBuffer();
	return (0);
}

/*
 * Check for interrupts from all devices.
 */
void
sccintr(unit)
	register int unit;
{
	register scc_regmap_t *regs;
	register struct tty *tp;
	register struct pdma *dp;
	register struct scc_softc *sc;
	register int cc, chan, rr1, rr2, rr3;
	int overrun = 0;

	sc = &scc_softc[unit];
	regs = (scc_regmap_t *)sc->scc_pdma[0].p_addr;
	unit <<= 1;
	for (;;) {
	    SCC_READ_REG(regs, SCC_CHANNEL_B, SCC_RR2, rr2);
	    rr2 = SCC_RR2_STATUS(rr2);
	    /* are we done yet ? */
	    if (rr2 == 6) {	/* strange, distinguished value */
		SCC_READ_REG(regs, SCC_CHANNEL_A, SCC_RR3, rr3);
		if (rr3 == 0)
			return;
	    }

	    SCC_WRITE_REG(regs, SCC_CHANNEL_A, SCC_RR0, SCC_RESET_HIGHEST_IUS);
	    if ((rr2 == SCC_RR2_A_XMIT_DONE) || (rr2 == SCC_RR2_B_XMIT_DONE)) {
		chan = (rr2 == SCC_RR2_A_XMIT_DONE) ?
			SCC_CHANNEL_A : SCC_CHANNEL_B;
		tp = &scc_tty[unit | chan];
		dp = &sc->scc_pdma[chan];
		if (dp->p_mem < dp->p_end) {
			SCC_WRITE_DATA(regs, chan, *dp->p_mem++);
			MachEmptyWriteBuffer();
		} else {
			tp->t_state &= ~TS_BUSY;
			if (tp->t_state & TS_FLUSH)
				tp->t_state &= ~TS_FLUSH;
			else {
				ndflush(&tp->t_outq, dp->p_mem-tp->t_outq.c_cf);
				dp->p_end = dp->p_mem = tp->t_outq.c_cf;
			}
			if (tp->t_line)
				(*linesw[tp->t_line].l_start)(tp);
			else
				sccstart(tp);
			if (tp->t_outq.c_cc == 0 || !(tp->t_state & TS_BUSY)) {
				SCC_READ_REG(regs, chan, SCC_RR15, cc);
				cc &= ~SCC_WR15_TX_UNDERRUN_IE;
				SCC_WRITE_REG(regs, chan, SCC_WR15, cc);
				cc = sc->scc_wreg[chan].wr1 & ~SCC_WR1_TX_IE;
				SCC_WRITE_REG(regs, chan, SCC_WR1, cc);
				sc->scc_wreg[chan].wr1 = cc;
				MachEmptyWriteBuffer();
			}
		}
	    } else if (rr2 == SCC_RR2_A_RECV_DONE ||
		rr2 == SCC_RR2_B_RECV_DONE || rr2 == SCC_RR2_A_RECV_SPECIAL ||
		rr2 == SCC_RR2_B_RECV_SPECIAL) {
		if (rr2 == SCC_RR2_A_RECV_DONE || rr2 == SCC_RR2_A_RECV_SPECIAL)
			chan = SCC_CHANNEL_A;
		else
			chan = SCC_CHANNEL_B;
		tp = &scc_tty[unit | chan];
		SCC_READ_DATA(regs, chan, cc);
		if (rr2 == SCC_RR2_A_RECV_SPECIAL ||
			rr2 == SCC_RR2_B_RECV_SPECIAL) {
			SCC_READ_REG(regs, chan, SCC_RR1, rr1);
			SCC_WRITE_REG(regs, chan, SCC_RR0, SCC_RESET_ERROR);
			if ((rr1 & SCC_RR1_RX_OVERRUN) && overrun == 0) {
				log(LOG_WARNING, "scc%d,%d: silo overflow\n",
					unit >> 1, chan);
				overrun = 1;
			}
		}

		/*
		 * Keyboard needs special treatment.
		 */
		if (tp == &scc_tty[SCCKBD_PORT] && cn_tab.cn_screen) {
#ifdef KADB
			if (cc == LK_DO) {
				spl0();
				kdbpanic();
				return;
			}
#endif
#ifdef DEBUG
			debugChar = cc;
#endif
			if (sccDivertXInput) {
				(*sccDivertXInput)(cc);
				continue;
			}
			if ((cc = kbdMapChar(cc)) < 0)
				continue;
		/*
		 * Now for mousey
		 */
		} else if (tp == &scc_tty[SCCMOUSE_PORT] && sccMouseButtons) {
			register MouseReport *mrp;
			static MouseReport currentRep;

			mrp = &currentRep;
			mrp->byteCount++;
			if (cc & MOUSE_START_FRAME) {
				/*
				 * The first mouse report byte (button state).
				 */
				mrp->state = cc;
				if (mrp->byteCount > 1)
					mrp->byteCount = 1;
			} else if (mrp->byteCount == 2) {
				/*
				 * The second mouse report byte (delta x).
				 */
				mrp->dx = cc;
			} else if (mrp->byteCount == 3) {
				/*
				 * The final mouse report byte (delta y).
				 */
				mrp->dy = cc;
				mrp->byteCount = 0;
				if (mrp->dx != 0 || mrp->dy != 0) {
					/*
					 * If the mouse moved,
					 * post a motion event.
					 */
					(*sccMouseEvent)(mrp);
				}
				(*sccMouseButtons)(mrp);
			}
			continue;
		}
		if (!(tp->t_state & TS_ISOPEN)) {
			wakeup((caddr_t)&tp->t_rawq);
#ifdef PORTSELECTOR
			if (!(tp->t_state & TS_WOPEN))
#endif
				continue;
		}
		if (rr2 == SCC_RR2_A_RECV_SPECIAL ||
			rr2 == SCC_RR2_B_RECV_SPECIAL) {
			if (rr1 & SCC_RR1_PARITY_ERR)
				cc |= TTY_PE;
			if (rr1 & SCC_RR1_FRAME_ERR)
				cc |= TTY_FE;
		}
		(*linesw[tp->t_line].l_rint)(cc, tp);
	    } else if ((rr2 == SCC_RR2_A_EXT_STATUS) || (rr2 == SCC_RR2_B_EXT_STATUS)) {
		chan = (rr2 == SCC_RR2_A_EXT_STATUS) ?
			SCC_CHANNEL_A : SCC_CHANNEL_B;
		SCC_WRITE_REG(regs, chan, SCC_RR0, SCC_RESET_EXT_IP);
		scc_modem_intr(unit | chan);
	    }
	}
}

void
sccstart(tp)
	register struct tty *tp;
{
	register struct pdma *dp;
	register scc_regmap_t *regs;
	register struct scc_softc *sc;
	register int cc, chan;
	u_char temp;
	int s, sendone;

	sc = &scc_softc[SCCUNIT(tp->t_dev)];
	dp = &sc->scc_pdma[SCCLINE(tp->t_dev)];
	regs = (scc_regmap_t *)dp->p_addr;
	s = spltty();
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;
	if (tp->t_outq.c_cc <= tp->t_lowat) {
		if (tp->t_state & TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		selwakeup(&tp->t_wsel);
	}
	if (tp->t_outq.c_cc == 0)
		goto out;
	/* handle console specially */
	if (tp == &scc_tty[SCCKBD_PORT] && cn_tab.cn_screen) {
		while (tp->t_outq.c_cc > 0) {
			cc = getc(&tp->t_outq) & 0x7f;
			cnputc(cc);
		}
		/*
		 * After we flush the output queue we may need to wake
		 * up the process that made the output.
		 */
		if (tp->t_outq.c_cc <= tp->t_lowat) {
			if (tp->t_state & TS_ASLEEP) {
				tp->t_state &= ~TS_ASLEEP;
				wakeup((caddr_t)&tp->t_outq);
			}
			selwakeup(&tp->t_wsel);
		}
		goto out;
	}
	if (tp->t_flags & (RAW|LITOUT))
		cc = ndqb(&tp->t_outq, 0);
	else {
		cc = ndqb(&tp->t_outq, 0200);
		if (cc == 0) {
			cc = getc(&tp->t_outq);
			timeout(ttrstrt, (void *)tp, (cc & 0x7f) + 6);
			tp->t_state |= TS_TIMEOUT;
			goto out;
		}
	}
	tp->t_state |= TS_BUSY;
	dp->p_end = dp->p_mem = tp->t_outq.c_cf;
	dp->p_end += cc;

	/*
	 * Enable transmission and send the first char, as required.
	 */
	chan = SCCLINE(tp->t_dev);
	SCC_READ_REG(regs, chan, SCC_RR0, temp);
	sendone = (temp & SCC_RR0_TX_EMPTY);
	SCC_READ_REG(regs, chan, SCC_RR15, temp);
	temp |= SCC_WR15_TX_UNDERRUN_IE;
	SCC_WRITE_REG(regs, chan, SCC_WR15, temp);
	temp = sc->scc_wreg[chan].wr1 | SCC_WR1_TX_IE;
	SCC_WRITE_REG(regs, chan, SCC_WR1, temp);
	sc->scc_wreg[chan].wr1 = temp;
	if (sendone) {
#ifdef DIAGNOSTIC
		if (cc == 0)
			panic("sccstart: No chars");
#endif
		SCC_WRITE_DATA(regs, chan, *dp->p_mem++);
	}
	MachEmptyWriteBuffer();
out:
	splx(s);
}

/*
 * Stop output on a line.
 */
/*ARGSUSED*/
sccstop(tp, flag)
	register struct tty *tp;
{
	register struct pdma *dp;
	register struct scc_softc *sc;
	register int s;

	sc = &scc_softc[SCCUNIT(tp->t_dev)];
	dp = &sc->scc_pdma[SCCLINE(tp->t_dev)];
	s = spltty();
	if (tp->t_state & TS_BUSY) {
		dp->p_end = dp->p_mem;
		if (!(tp->t_state & TS_TTSTOP))
			tp->t_state |= TS_FLUSH;
	}
	splx(s);
}

sccmctl(dev, bits, how)
	dev_t dev;
	int bits, how;
{
	register struct scc_softc *sc;
	register scc_regmap_t *regs;
	register int line, mbits;
	register u_char value;
	int s;

	sc = &scc_softc[SCCUNIT(dev)];
	line = SCCLINE(dev);
	regs = (scc_regmap_t *)sc->scc_pdma[line].p_addr;
	s = spltty();
	/*
	 * only channel B has modem control, however the DTR and RTS
	 * pins on the comm port are wired to the DTR and RTS A channel
	 * signals.
	 */
	mbits = DML_DTR | DML_DSR | DML_CAR;
	if (line == SCC_CHANNEL_B) {
		if (sc->scc_wreg[SCC_CHANNEL_A].wr5 & SCC_WR5_DTR)
			mbits = DML_DTR | DML_DSR;
		else
			mbits = 0;
		SCC_READ_REG_ZERO(regs, SCC_CHANNEL_B, value);
		if (value & SCC_RR0_DCD)
			mbits |= DML_CAR;
	}
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
		return (mbits);
	}
	if (line == SCC_CHANNEL_B) {
		if (mbits & DML_DTR)
			sc->scc_wreg[SCC_CHANNEL_A].wr5 |= SCC_WR5_DTR;
		else
			sc->scc_wreg[SCC_CHANNEL_A].wr5 &= ~SCC_WR5_DTR;
		SCC_WRITE_REG(regs, SCC_CHANNEL_A, SCC_WR5,
			sc->scc_wreg[SCC_CHANNEL_A].wr5);
	}
	if ((mbits & DML_DTR) && (sc->scc_softCAR & (1 << line)))
		scc_tty[minor(dev)].t_state |= TS_CARR_ON;
	(void) splx(s);
	return (mbits);
}

/*
 * Check for carrier transition.
 */
static void
scc_modem_intr(dev)
	dev_t dev;
{
	register scc_regmap_t *regs;
	register struct scc_softc *sc;
	register struct tty *tp;
	register int car, chan;
	register u_char value;
	int s;

	sc = &scc_softc[SCCUNIT(dev)];
	tp = &scc_tty[minor(dev)];
	chan = SCCLINE(dev);
	regs = (scc_regmap_t *)sc->scc_pdma[chan].p_addr;
	if (chan == SCC_CHANNEL_A)
		return;
	s = spltty();
	if (sc->scc_softCAR & (1 << chan))
		car = 1;
	else {
		SCC_READ_REG_ZERO(regs, chan, value);
		car = value & SCC_RR0_DCD;
	}
	if (car) {
		/* carrier present */
		if (!(tp->t_state & TS_CARR_ON))
			(void)(*linesw[tp->t_line].l_modem)(tp, 1);
	} else if (tp->t_state & TS_CARR_ON)
		(void)(*linesw[tp->t_line].l_modem)(tp, 0);
	splx(s);
}

/*
 * Get a char off the appropriate line via. a busy wait loop.
 */
int
sccGetc(dev)
	dev_t dev;
{
	register scc_regmap_t *regs;
	register int c, line;
	register u_char value;
	int s;

	line = SCCLINE(dev);
	regs = (scc_regmap_t *)scc_softc[SCCUNIT(dev)].scc_pdma[line].p_addr;
	if (!regs)
		return (0);
	s = spltty();
	for (;;) {
		SCC_READ_REG(regs, line, SCC_RR0, value);
		if (value & SCC_RR0_RX_AVAIL) {
			SCC_READ_REG(regs, line, SCC_RR1, value);
			SCC_READ_DATA(regs, line, c);
			if (value & (SCC_RR1_PARITY_ERR | SCC_RR1_RX_OVERRUN |
				SCC_RR1_FRAME_ERR)) {
				SCC_WRITE_REG(regs, line, SCC_WR0, SCC_RESET_ERROR);
				SCC_WRITE_REG(regs, SCC_CHANNEL_A, SCC_WR0,
					SCC_RESET_HIGHEST_IUS);
			} else {
				SCC_WRITE_REG(regs, SCC_CHANNEL_A, SCC_WR0,
					SCC_RESET_HIGHEST_IUS);
				splx(s);
				return (c & 0xff);
			}
		} else
			DELAY(10);
	}
}

/*
 * Send a char on a port, via a busy wait loop.
 */
void
sccPutc(dev, c)
	dev_t dev;
	int c;
{
	register scc_regmap_t *regs;
	register int line;
	register u_char value;
	int s;

	s = spltty();
	line = SCCLINE(dev);
	regs = (scc_regmap_t *)scc_softc[SCCUNIT(dev)].scc_pdma[line].p_addr;

	/*
	 * Wait for transmitter to be not busy.
	 */
	do {
		SCC_READ_REG(regs, line, SCC_RR0, value);
		if (value & SCC_RR0_TX_EMPTY)
			break;
		DELAY(100);
	} while (1);

	/*
	 * Send the char.
	 */
	SCC_WRITE_DATA(regs, line, c);
	MachEmptyWriteBuffer();
	splx(s);
}
#endif /* NSCC */
