/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from Utah: $Hdr: dcm.c 1.29 92/01/21$
 *
 *	@(#)dcm.c	7.21 (Berkeley) %G%
 */

/*
 * TODO:
 *	Timeouts
 *	Test console support.
 */

#include "dcm.h"
#if NDCM > 0
/*
 *  98642/MUX
 */
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/ioctl.h>
#include <sys/proc.h>
#include <sys/tty.h>
#include <sys/conf.h>
#include <sys/file.h>
#include <sys/uio.h>
#include <sys/kernel.h>
#include <sys/syslog.h>
#include <sys/time.h>

#include <hp/dev/device.h>

#include <hp300/dev/dcmreg.h>
#include <machine/cpu.h>
#include <hp300/hp300/isr.h>

#ifndef DEFAULT_BAUD_RATE
#define DEFAULT_BAUD_RATE 9600
#endif

int	ttrstrt();
int	dcmprobe(), dcmintr(), dcmparam();
void	dcmstart();
struct	driver dcmdriver = {
	dcmprobe, "dcm",
};

#define NDCMLINE (NDCM*4)

struct	tty dcm_tty[NDCMLINE];
struct	modemreg *dcm_modem[NDCMLINE];
char	mcndlast[NDCMLINE];	/* XXX last modem status for line */
int	ndcm = NDCMLINE;

int	dcm_active;
int	dcmsoftCAR[NDCM];
struct	dcmdevice *dcm_addr[NDCM];
struct	isr dcmisr[NDCM];

struct speedtab dcmspeedtab[] = {
	0,	BR_0,
	50,	BR_50,
	75,	BR_75,
	110,	BR_110,
	134,	BR_134,
	150,	BR_150,
	300,	BR_300,
	600,	BR_600,
	1200,	BR_1200,
	1800,	BR_1800,
	2400,	BR_2400,
	4800,	BR_4800,
	9600,	BR_9600,
	19200,	BR_19200,
	38400,	BR_38400,
	-1,	-1
};

/* u-sec per character based on baudrate (assumes 1 start/8 data/1 stop bit) */
#define	DCM_USPERCH(s)	(10000000 / (s))

/*
 * Per board interrupt scheme.  16.7ms is the polling interrupt rate
 * (16.7ms is about 550 baud, 38.4k is 72 chars in 16.7ms).
 */
#define DIS_TIMER	0
#define DIS_PERCHAR	1
#define DIS_RESET	2

int	dcmistype = -1;		/* -1 == dynamic, 0 == timer, 1 == perchar */
int     dcminterval = 5;	/* interval (secs) between checks */
struct	dcmischeme {
	int	dis_perchar;	/* non-zero if interrupting per char */
	long	dis_time;	/* last time examined */
	int	dis_intr;	/* recv interrupts during last interval */
	int	dis_char;	/* characters read during last interval */
} dcmischeme[NDCM];

/*
 * Console support
 */
#ifdef DCMCONSOLE
int	dcmconsole = DCMCONSOLE;
#else
int	dcmconsole = -1;
#endif
int	dcmconsinit;
int	dcmdefaultrate = DEFAULT_BAUD_RATE;
int	dcmconbrdbusy = 0;
int	dcmmajor;
extern	struct tty *constty;

#ifdef KGDB
/*
 * Kernel GDB support
 */
#include <machine/remote-sl.h>

extern dev_t kgdb_dev;
extern int kgdb_rate;
extern int kgdb_debug_init;
#endif

/* #define DCMSTATS */

#ifdef DEBUG
int	dcmdebug = 0x0;
#define DDB_SIOERR	0x01
#define DDB_PARAM	0x02
#define DDB_INPUT	0x04
#define DDB_OUTPUT	0x08
#define DDB_INTR	0x10
#define DDB_IOCTL	0x20
#define DDB_INTSCHM	0x40
#define DDB_MODEM	0x80
#define DDB_OPENCLOSE	0x100
#endif

#ifdef DCMSTATS
#define	DCMRBSIZE	94
#define DCMXBSIZE	24

struct	dcmstats {
	long	xints;		    /* # of xmit ints */
	long	xchars;		    /* # of xmit chars */
	long	xempty;		    /* times outq is empty in dcmstart */
	long	xrestarts;	    /* times completed while xmitting */
	long	rints;		    /* # of recv ints */
	long	rchars;		    /* # of recv chars */
	long	xsilo[DCMXBSIZE+2]; /* times this many chars xmit on one int */
	long	rsilo[DCMRBSIZE+2]; /* times this many chars read on one int */
} dcmstats[NDCM];
#endif

#define UNIT(x)		minor(x)
#define	BOARD(x)	(((x) >> 2) & 0x3f)
#define PORT(x)		((x) & 3)
#define MKUNIT(b,p)	(((b) << 2) | (p))

/*
 * Conversion from "HP DCE" to almost-normal DCE: on the 638 8-port mux,
 * the distribution panel uses "HP DCE" conventions.  If requested via
 * the device flags, we swap the inputs to something closer to normal DCE,
 * allowing a straight-through cable to a DTE or a reversed cable
 * to a DCE (reversing 2-3, 4-5, 8-20 and leaving 6 unconnected;
 * this gets "DCD" on pin 20 and "CTS" on 4, but doesn't connect
 * DSR or make RTS work, though).  The following gives the full
 * details of a cable from this mux panel to a modem:
 *
 *		     HP		    modem
 *		name	pin	pin	name
 * HP inputs:
 *		"Rx"	 2	 3	Tx
 *		CTS	 4	 5	CTS	(only needed for CCTS_OFLOW)
 *		DCD	20	 8	DCD
 *		"DSR"	 9	 6	DSR	(unneeded)
 *		RI	22	22	RI	(unneeded)
 *
 * HP outputs:
 *		"Tx"	 3	 2	Rx
 *		"DTR"	 6	not connected
 *		"RTS"	 8	20	DTR
 *		"SR"	23	 4	RTS	(often not needed)
 */
#define	FLAG_STDDCE	0x10	/* map inputs if this bit is set in flags */
#define hp2dce_in(ibits)	(iconv[(ibits) & 0xf])
static char iconv[16] = {
	0,		MI_DM,		MI_CTS,		MI_CTS|MI_DM,
	MI_CD,		MI_CD|MI_DM,	MI_CD|MI_CTS,	MI_CD|MI_CTS|MI_DM,
	MI_RI,		MI_RI|MI_DM,	MI_RI|MI_CTS,	MI_RI|MI_CTS|MI_DM,
	MI_RI|MI_CD,	MI_RI|MI_CD|MI_DM, MI_RI|MI_CD|MI_CTS,
	MI_RI|MI_CD|MI_CTS|MI_DM
};

dcmprobe(hd)
	register struct hp_device *hd;
{
	register struct dcmdevice *dcm;
	register int i;
	register int timo = 0;
	int s, brd, isconsole, mbits;

	dcm = (struct dcmdevice *)hd->hp_addr;
	if ((dcm->dcm_rsid & 0x1f) != DCMID)
		return (0);
	brd = hd->hp_unit;
	isconsole = (brd == BOARD(dcmconsole));
	/*
	 * XXX selected console device (CONSUNIT) as determined by
	 * dcmcnprobe does not agree with logical numbering imposed
	 * by the config file (i.e. lowest address DCM is not unit
	 * CONSUNIT).  Don't recognize this card.
	 */
	if (isconsole && dcm != dcm_addr[BOARD(dcmconsole)])
		return (0);

	/*
	 * Empirically derived self-test magic
	 */
	s = spltty();
	dcm->dcm_rsid = DCMRS;
	DELAY(50000);	/* 5000 is not long enough */
	dcm->dcm_rsid = 0; 
	dcm->dcm_ic = IC_IE;
	dcm->dcm_cr = CR_SELFT;
	while ((dcm->dcm_ic & IC_IR) == 0)
		if (++timo == 20000)
			return (0);
	DELAY(50000)	/* XXX why is this needed ???? */
	while ((dcm->dcm_iir & IIR_SELFT) == 0)
		if (++timo == 400000)
			return (0);
	DELAY(50000)	/* XXX why is this needed ???? */
	if (dcm->dcm_stcon != ST_OK) {
		if (!isconsole)
			printf("dcm%d: self test failed: %x\n",
			       brd, dcm->dcm_stcon);
		return (0);
	}
	dcm->dcm_ic = IC_ID;
	splx(s);

	hd->hp_ipl = DCMIPL(dcm->dcm_ic);
	dcm_addr[brd] = dcm;
	dcm_active |= 1 << brd;
	dcmsoftCAR[brd] = hd->hp_flags;
	dcmisr[brd].isr_ipl = hd->hp_ipl;
	dcmisr[brd].isr_arg = brd;
	dcmisr[brd].isr_intr = dcmintr;
	isrlink(&dcmisr[brd]);
#ifdef KGDB
	if (major(kgdb_dev) == dcmmajor && BOARD(kgdb_dev) == brd) {
		if (dcmconsole == UNIT(kgdb_dev))
			kgdb_dev = NODEV; /* can't debug over console port */
#ifndef KGDB_CHEAT
		/*
		 * The following could potentially be replaced
		 * by the corresponding code in dcmcnprobe.
		 */
		else {
			(void) dcminit(kgdb_dev, kgdb_rate);
			if (kgdb_debug_init) {
				printf("dcm%d: ", UNIT(kgdb_dev));
				kgdb_connect(1);
			} else
				printf("dcm%d: kgdb enabled\n", UNIT(kgdb_dev));
		}
		/* end could be replaced */
#endif
	}
#endif
	if (dcmistype == DIS_TIMER)
		dcmsetischeme(brd, DIS_RESET|DIS_TIMER);
	else
		dcmsetischeme(brd, DIS_RESET|DIS_PERCHAR);

	/* load pointers to modem control */
	dcm_modem[MKUNIT(brd, 0)] = &dcm->dcm_modem0;
	dcm_modem[MKUNIT(brd, 1)] = &dcm->dcm_modem1;
	dcm_modem[MKUNIT(brd, 2)] = &dcm->dcm_modem2;
	dcm_modem[MKUNIT(brd, 3)] = &dcm->dcm_modem3;
	/* set DCD (modem) and CTS (flow control) on all ports */
	if (dcmsoftCAR[brd] & FLAG_STDDCE)
		mbits = hp2dce_in(MI_CD|MI_CTS);
	else
		mbits = MI_CD|MI_CTS;
	for (i = 0; i < 4; i++)
		dcm_modem[MKUNIT(brd, i)]->mdmmsk = mbits;

	dcm->dcm_ic = IC_IE;		/* turn all interrupts on */
	/*
	 * Need to reset baud rate, etc. of next print so reset dcmconsole.
	 * Also make sure console is always "hardwired"
	 */
	if (isconsole) {
		dcmconsinit = 0;
		dcmsoftCAR[brd] |= (1 << PORT(dcmconsole));
	}
	return (1);
}

/* ARGSUSED */
#ifdef __STDC__
dcmopen(dev_t dev, int flag, int mode, struct proc *p)
#else
dcmopen(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
#endif
{
	register struct tty *tp;
	register int unit, brd;
	int error = 0, mbits;

	unit = UNIT(dev);
	brd = BOARD(unit);
	if (unit >= NDCMLINE || (dcm_active & (1 << brd)) == 0)
		return (ENXIO);
	tp = &dcm_tty[unit];
	tp->t_oproc = dcmstart;
	tp->t_param = dcmparam;
	tp->t_dev = dev;
	if ((tp->t_state & TS_ISOPEN) == 0) {
		tp->t_state |= TS_WOPEN;
		ttychars(tp);
		if (tp->t_ispeed == 0) {
			tp->t_iflag = TTYDEF_IFLAG;
			tp->t_oflag = TTYDEF_OFLAG;
			tp->t_cflag = TTYDEF_CFLAG;
			tp->t_lflag = TTYDEF_LFLAG;
			tp->t_ispeed = tp->t_ospeed = TTYDEF_SPEED;
		}
		(void) dcmparam(tp, &tp->t_termios);
		ttsetwater(tp);
	} else if (tp->t_state&TS_XCLUDE && p->p_ucred->cr_uid != 0)
		return (EBUSY);
	mbits = MO_ON;
	if (dcmsoftCAR[brd] & FLAG_STDDCE)
		mbits |= MO_SR;		/* pin 23, could be used as RTS */
	(void) dcmmctl(dev, mbits, DMSET);	/* enable port */
	if ((dcmsoftCAR[brd] & (1 << PORT(unit))) ||
	    (dcmmctl(dev, MO_OFF, DMGET) & MI_CD))
		tp->t_state |= TS_CARR_ON;
#ifdef DEBUG
	if (dcmdebug & DDB_MODEM)
		printf("dcm%d: dcmopen port %d softcarr %c\n",
		       brd, unit, (tp->t_state & TS_CARR_ON) ? '1' : '0');
#endif
	(void) spltty();
	while ((flag&O_NONBLOCK) == 0 && (tp->t_cflag&CLOCAL) == 0 &&
	    (tp->t_state & TS_CARR_ON) == 0) {
		tp->t_state |= TS_WOPEN;
		if (error = ttysleep(tp, (caddr_t)&tp->t_rawq, TTIPRI | PCATCH,
		    ttopen, 0))
			break;
	}
	(void) spl0();

#ifdef DEBUG
	if (dcmdebug & DDB_OPENCLOSE)
		printf("dcmopen: u %x st %x fl %x\n",
			unit, tp->t_state, tp->t_flags);
#endif
	if (error == 0)
		error = (*linesw[tp->t_line].l_open)(dev, tp);
	return (error);
}
 
/*ARGSUSED*/
dcmclose(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct tty *tp;
	int unit;
 
	unit = UNIT(dev);
	tp = &dcm_tty[unit];
	(*linesw[tp->t_line].l_close)(tp, flag);
	if (tp->t_cflag&HUPCL || tp->t_state&TS_WOPEN ||
	    (tp->t_state&TS_ISOPEN) == 0)
		(void) dcmmctl(dev, MO_OFF, DMSET);
#ifdef DEBUG
	if (dcmdebug & DDB_OPENCLOSE)
		printf("dcmclose: u %x st %x fl %x\n",
			unit, tp->t_state, tp->t_flags);
#endif
	ttyclose(tp);
	return (0);
}
 
dcmread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;
 
	tp = &dcm_tty[UNIT(dev)];
	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}
 
dcmwrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	int unit = UNIT(dev);
	register struct tty *tp;
 
	tp = &dcm_tty[unit];
	/*
	 * XXX we disallow virtual consoles if the physical console is
	 * a serial port.  This is in case there is a display attached that
	 * is not the console.  In that situation we don't need/want the X
	 * server taking over the console.
	 */
	if (constty && unit == dcmconsole)
		constty = NULL;
	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}
 
dcmintr(brd)
	register int brd;
{
	register struct dcmdevice *dcm = dcm_addr[brd];
	register struct dcmischeme *dis;
	register int unit = MKUNIT(brd, 0);
	register int code, i;
	int pcnd[4], mcode, mcnd[4];

	/*
	 * Do all guarded register accesses right off to minimize
	 * block out of hardware.
	 */
	SEM_LOCK(dcm);
	if ((dcm->dcm_ic & IC_IR) == 0) {
		SEM_UNLOCK(dcm);
		return (0);
	}
	for (i = 0; i < 4; i++) {
		pcnd[i] = dcm->dcm_icrtab[i].dcm_data;
		dcm->dcm_icrtab[i].dcm_data = 0;
		code = dcm_modem[unit+i]->mdmin;
		if (dcmsoftCAR[brd] & FLAG_STDDCE)
			code = hp2dce_in(code);
		mcnd[i] = code;
	}
	code = dcm->dcm_iir & IIR_MASK;
	dcm->dcm_iir = 0;	/* XXX doc claims read clears interrupt?! */
	mcode = dcm->dcm_modemintr;
	dcm->dcm_modemintr = 0;
	SEM_UNLOCK(dcm);

#ifdef DEBUG
	if (dcmdebug & DDB_INTR) {
		printf("dcmintr(%d): iir %x pc %x/%x/%x/%x ",
		       brd, code, pcnd[0], pcnd[1], pcnd[2], pcnd[3]); 
		printf("miir %x mc %x/%x/%x/%x\n",
		       mcode, mcnd[0], mcnd[1], mcnd[2], mcnd[3]);
	}
#endif
	if (code & IIR_TIMEO)
		dcmrint(brd, dcm);
	if (code & IIR_PORT0)
		dcmpint(unit+0, pcnd[0], dcm);
	if (code & IIR_PORT1)
		dcmpint(unit+1, pcnd[1], dcm);
	if (code & IIR_PORT2)
		dcmpint(unit+2, pcnd[2], dcm);
	if (code & IIR_PORT3)
		dcmpint(unit+3, pcnd[3], dcm);
	if (code & IIR_MODM) {
		if (mcode == 0 || mcode & 0x1)	/* mcode==0 -> 98642 board */
			dcmmint(unit+0, mcnd[0], dcm);
		if (mcode & 0x2)
			dcmmint(unit+1, mcnd[1], dcm);
		if (mcode & 0x4)
			dcmmint(unit+2, mcnd[2], dcm);
		if (mcode & 0x8)
			dcmmint(unit+3, mcnd[3], dcm);
	}

	dis = &dcmischeme[brd];
	/*
	 * Chalk up a receiver interrupt if the timer running or one of
	 * the ports reports a special character interrupt.
	 */
	if ((code & IIR_TIMEO) ||
	    ((pcnd[0]|pcnd[1]|pcnd[2]|pcnd[3]) & IT_SPEC))
		dis->dis_intr++;
	/*
	 * See if it is time to check/change the interrupt rate.
	 */
	if (dcmistype < 0 &&
	    (i = time.tv_sec - dis->dis_time) >= dcminterval) {
		/*
		 * If currently per-character and averaged over 70 interrupts
		 * per-second (66 is threshold of 600 baud) in last interval,
		 * switch to timer mode.
		 *
		 * XXX decay counts ala load average to avoid spikes?
		 */
		if (dis->dis_perchar && dis->dis_intr > 70 * i)
			dcmsetischeme(brd, DIS_TIMER);
		/*
		 * If currently using timer and had more interrupts than
		 * received characters in the last interval, switch back
		 * to per-character.  Note that after changing to per-char
		 * we must process any characters already in the queue
		 * since they may have arrived before the bitmap was setup.
		 *
		 * XXX decay counts?
		 */
		else if (!dis->dis_perchar && dis->dis_intr > dis->dis_char) {
			dcmsetischeme(brd, DIS_PERCHAR);
			dcmrint(brd, dcm);
		}
		dis->dis_intr = dis->dis_char = 0;
		dis->dis_time = time.tv_sec;
	}
	return (1);
}

/*
 *  Port interrupt.  Can be two things:
 *	First, it might be a special character (exception interrupt);
 *	Second, it may be a buffer empty (transmit interrupt);
 */
dcmpint(unit, code, dcm)
	int unit, code;
	struct dcmdevice *dcm;
{
	struct tty *tp = &dcm_tty[unit];

	if (code & IT_SPEC)
		dcmreadbuf(unit, dcm, tp);
	if (code & IT_TX)
		dcmxint(unit, dcm, tp);
}

dcmrint(brd, dcm)
	int brd;
	register struct dcmdevice *dcm;
{
	register int i, unit;
	register struct tty *tp;

	unit = MKUNIT(brd, 0);
	tp = &dcm_tty[unit];
	for (i = 0; i < 4; i++, tp++, unit++)
		dcmreadbuf(unit, dcm, tp);
}

dcmreadbuf(unit, dcm, tp)
	int unit;
	register struct dcmdevice *dcm;
	register struct tty *tp;
{
	int port = PORT(unit);
	register struct dcmpreg *pp = dcm_preg(dcm, port);
	register struct dcmrfifo *fifo;
	register int c, stat;
	register unsigned head;
	int nch = 0;
#ifdef DCMSTATS
	struct dcmstats *dsp = &dcmstats[BOARD(unit)];

	dsp->rints++;
#endif
	if ((tp->t_state & TS_ISOPEN) == 0) {
#ifdef KGDB
		if ((makedev(dcmmajor, unit) == kgdb_dev) &&
		    (head = pp->r_head & RX_MASK) != (pp->r_tail & RX_MASK) &&
		    dcm->dcm_rfifos[3-port][head>>1].data_char == FRAME_START) {
			pp->r_head = (head + 2) & RX_MASK;
			kgdb_connect(0);	/* trap into kgdb */
			return;
		}
#endif /* KGDB */
		pp->r_head = pp->r_tail & RX_MASK;
		return;
	}

	head = pp->r_head & RX_MASK;
	fifo = &dcm->dcm_rfifos[3-port][head>>1];
	/*
	 * XXX upper bound on how many chars we will take in one swallow?
	 */
	while (head != (pp->r_tail & RX_MASK)) {
		/*
		 * Get character/status and update head pointer as fast
		 * as possible to make room for more characters.
		 */
		c = fifo->data_char;
		stat = fifo->data_stat;
		head = (head + 2) & RX_MASK;
		pp->r_head = head;
		fifo = head ? fifo+1 : &dcm->dcm_rfifos[3-port][0];
		nch++;

#ifdef DEBUG
		if (dcmdebug & DDB_INPUT)
			printf("dcmreadbuf(%d): c%x('%c') s%x f%x h%x t%x\n",
			       unit, c&0xFF, c, stat&0xFF,
			       tp->t_flags, head, pp->r_tail);
#endif
		/*
		 * Check for and handle errors
		 */
		if (stat & RD_MASK) {
#ifdef DEBUG
			if (dcmdebug & (DDB_INPUT|DDB_SIOERR))
				printf("dcmreadbuf(%d): err: c%x('%c') s%x\n",
				       unit, stat, c&0xFF, c);
#endif
			if (stat & (RD_BD | RD_FE))
				c |= TTY_FE;
			else if (stat & RD_PE)
				c |= TTY_PE;
			else if (stat & RD_OVF)
				log(LOG_WARNING,
				    "dcm%d: silo overflow\n", unit);
			else if (stat & RD_OE)
				log(LOG_WARNING,
				    "dcm%d: uart overflow\n", unit);
		}
		(*linesw[tp->t_line].l_rint)(c, tp);
	}
	dcmischeme[BOARD(unit)].dis_char += nch;
#ifdef DCMSTATS
	dsp->rchars += nch;
	if (nch <= DCMRBSIZE)
		dsp->rsilo[nch]++;
	else
		dsp->rsilo[DCMRBSIZE+1]++;
#endif
}

dcmxint(unit, dcm, tp)
	int unit;
	struct dcmdevice *dcm;
	register struct tty *tp;
{
	tp->t_state &= ~TS_BUSY;
	if (tp->t_state & TS_FLUSH)
		tp->t_state &= ~TS_FLUSH;
	(*linesw[tp->t_line].l_start)(tp);
}

dcmmint(unit, mcnd, dcm)
	register int unit;
	register struct dcmdevice *dcm;
        int mcnd;
{
	register struct tty *tp;
	int delta;

#ifdef DEBUG
	if (dcmdebug & DDB_MODEM)
		printf("dcmmint: port %d mcnd %x mcndlast %x\n",
		       unit, mcnd, mcndlast[unit]);
#endif
	tp = &dcm_tty[unit];
	delta = mcnd ^ mcndlast[unit];
	mcndlast[unit] = mcnd;
	if ((delta & MI_CTS) && (tp->t_state & TS_ISOPEN) &&
	    (tp->t_flags & CCTS_OFLOW)) {
		if (mcnd & MI_CTS) {
			tp->t_state &= ~TS_TTSTOP;
			ttstart(tp);
		} else
			tp->t_state |= TS_TTSTOP;	/* inline dcmstop */
	}
	if (delta & MI_CD) {
		if (mcnd & MI_CD)
			(void)(*linesw[tp->t_line].l_modem)(tp, 1);
		else if ((dcmsoftCAR[BOARD(unit)] & (1 << PORT(unit))) == 0 &&
		    (*linesw[tp->t_line].l_modem)(tp, 0) == 0) {
			dcm_modem[unit]->mdmout = MO_OFF;
			SEM_LOCK(dcm);
			dcm->dcm_modemchng |= 1<<(unit & 3);
			dcm->dcm_cr |= CR_MODM;
			SEM_UNLOCK(dcm);
			DELAY(10); /* time to change lines */
		}
	}
}

dcmioctl(dev, cmd, data, flag, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
	struct proc *p;
{
	register struct tty *tp;
	register int unit = UNIT(dev);
	register struct dcmdevice *dcm;
	register int port;
	int error, s;
 
#ifdef DEBUG
	if (dcmdebug & DDB_IOCTL)
		printf("dcmioctl: unit %d cmd %x data %x flag %x\n",
		       unit, cmd, *data, flag);
#endif
	tp = &dcm_tty[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag, p);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	port = PORT(unit);
	dcm = dcm_addr[BOARD(unit)];
	switch (cmd) {
	case TIOCSBRK:
		/*
		 * Wait for transmitter buffer to empty
		 */
		s = spltty();
		while (dcm->dcm_thead[port].ptr != dcm->dcm_ttail[port].ptr)
			DELAY(DCM_USPERCH(tp->t_ospeed));
		SEM_LOCK(dcm);
		dcm->dcm_cmdtab[port].dcm_data |= CT_BRK;
		dcm->dcm_cr |= (1 << port);	/* start break */
		SEM_UNLOCK(dcm);
		splx(s);
		break;

	case TIOCCBRK:
		SEM_LOCK(dcm);
		dcm->dcm_cmdtab[port].dcm_data |= CT_BRK;
		dcm->dcm_cr |= (1 << port);	/* end break */
		SEM_UNLOCK(dcm);
		break;

	case TIOCSDTR:
		(void) dcmmctl(dev, MO_ON, DMBIS);
		break;

	case TIOCCDTR:
		(void) dcmmctl(dev, MO_ON, DMBIC);
		break;

	case TIOCMSET:
		(void) dcmmctl(dev, *(int *)data, DMSET);
		break;

	case TIOCMBIS:
		(void) dcmmctl(dev, *(int *)data, DMBIS);
		break;

	case TIOCMBIC:
		(void) dcmmctl(dev, *(int *)data, DMBIC);
		break;

	case TIOCMGET:
		*(int *)data = dcmmctl(dev, 0, DMGET);
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

dcmparam(tp, t)
	register struct tty *tp;
	register struct termios *t;
{
	register struct dcmdevice *dcm;
	register int port, mode, cflag = t->c_cflag;
	int ospeed = ttspeedtab(t->c_ospeed, dcmspeedtab);

	/* check requested parameters */
        if (ospeed < 0 || (t->c_ispeed && t->c_ispeed != t->c_ospeed))
                return (EINVAL);
        /* and copy to tty */
        tp->t_ispeed = t->c_ispeed;
        tp->t_ospeed = t->c_ospeed;
        tp->t_cflag = cflag;
	if (ospeed == 0) {
		(void) dcmmctl(UNIT(tp->t_dev), MO_OFF, DMSET);
		return (0);
	}

	mode = 0;
	switch (cflag&CSIZE) {
	case CS5:
		mode = LC_5BITS; break;
	case CS6:
		mode = LC_6BITS; break;
	case CS7:
		mode = LC_7BITS; break;
	case CS8:
		mode = LC_8BITS; break;
	}
	if (cflag&PARENB) {
		if (cflag&PARODD)
			mode |= LC_PODD;
		else
			mode |= LC_PEVEN;
	}
	if (cflag&CSTOPB)
		mode |= LC_2STOP;
	else
		mode |= LC_1STOP;
#ifdef DEBUG
	if (dcmdebug & DDB_PARAM)
		printf("dcmparam(%d): cflag %x mode %x speed %d uperch %d\n",
		       UNIT(tp->t_dev), cflag, mode, tp->t_ospeed,
		       DCM_USPERCH(tp->t_ospeed));
#endif

	port = PORT(tp->t_dev);
	dcm = dcm_addr[BOARD(tp->t_dev)];
	/*
	 * Wait for transmitter buffer to empty.
	 */
	while (dcm->dcm_thead[port].ptr != dcm->dcm_ttail[port].ptr)
		DELAY(DCM_USPERCH(tp->t_ospeed));
	/*
	 * Make changes known to hardware.
	 */
	dcm->dcm_data[port].dcm_baud = ospeed;
	dcm->dcm_data[port].dcm_conf = mode;
	SEM_LOCK(dcm);
	dcm->dcm_cmdtab[port].dcm_data |= CT_CON;
	dcm->dcm_cr |= (1 << port);
	SEM_UNLOCK(dcm);
	/*
	 * Delay for config change to take place. Weighted by baud.
	 * XXX why do we do this?
	 */
	DELAY(16 * DCM_USPERCH(tp->t_ospeed));
	return (0);
}
 
void
dcmstart(tp)
	register struct tty *tp;
{
	register struct dcmdevice *dcm;
	register struct dcmpreg *pp;
	register struct dcmtfifo *fifo;
	register char *bp;
	register unsigned tail, next;
	register int port, nch;
	unsigned head;
	char buf[16];
	int s;
#ifdef DCMSTATS
	struct dcmstats *dsp = &dcmstats[BOARD(tp->t_dev)];
	int tch = 0;
#endif

	s = spltty();
#ifdef DCMSTATS
	dsp->xints++;
#endif
#ifdef DEBUG
	if (dcmdebug & DDB_OUTPUT)
		printf("dcmstart(%d): state %x flags %x outcc %d\n",
		       UNIT(tp->t_dev), tp->t_state, tp->t_flags,
		       tp->t_outq.c_cc);
#endif
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;
	if (tp->t_outq.c_cc <= tp->t_lowat) {
		if (tp->t_state&TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		selwakeup(&tp->t_wsel);
	}
	if (tp->t_outq.c_cc == 0) {
#ifdef DCMSTATS
		dsp->xempty++;
#endif
		goto out;
	}

	dcm = dcm_addr[BOARD(tp->t_dev)];
	port = PORT(tp->t_dev);
	pp = dcm_preg(dcm, port);
	tail = pp->t_tail & TX_MASK;
	next = (tail + 1) & TX_MASK;
	head = pp->t_head & TX_MASK;
	if (head == next)
		goto out;
	fifo = &dcm->dcm_tfifos[3-port][tail];
again:
	nch = q_to_b(&tp->t_outq, buf, (head - next) & TX_MASK);
#ifdef DCMSTATS
	tch += nch;
#endif
#ifdef DEBUG
	if (dcmdebug & DDB_OUTPUT)
		printf("\thead %x tail %x nch %d\n", head, tail, nch);
#endif
	/*
	 * Loop transmitting all the characters we can.
	 */
	for (bp = buf; --nch >= 0; bp++) {
		fifo->data_char = *bp;
		pp->t_tail = next;
		/*
		 * If this is the first character,
		 * get the hardware moving right now.
		 */
		if (bp == buf) {
			tp->t_state |= TS_BUSY;
			SEM_LOCK(dcm);
			dcm->dcm_cmdtab[port].dcm_data |= CT_TX;
			dcm->dcm_cr |= (1 << port);
			SEM_UNLOCK(dcm);
		}
		tail = next;
		fifo = tail ? fifo+1 : &dcm->dcm_tfifos[3-port][0];
		next = (next + 1) & TX_MASK;
	}
	/*
	 * Head changed while we were loading the buffer,
	 * go back and load some more if we can.
	 */
	if (tp->t_outq.c_cc && head != (pp->t_head & TX_MASK)) {
#ifdef DCMSTATS
		dsp->xrestarts++;
#endif
		head = pp->t_head & TX_MASK;
		goto again;
	}

	/*
	 * Kick it one last time in case it finished while we were
	 * loading the last bunch.
	 */
	if (bp > &buf[1]) {
		tp->t_state |= TS_BUSY;
		SEM_LOCK(dcm);
		dcm->dcm_cmdtab[port].dcm_data |= CT_TX;
		dcm->dcm_cr |= (1 << port);
		SEM_UNLOCK(dcm);
	}
#ifdef DEBUG
	if (dcmdebug & DDB_INTR)
		printf("dcmstart(%d): head %x tail %x outqcc %d\n",
		       UNIT(tp->t_dev), head, tail, tp->t_outq.c_cc);
#endif
out:
#ifdef DCMSTATS
	dsp->xchars += tch;
	if (tch <= DCMXBSIZE)
		dsp->xsilo[tch]++;
	else
		dsp->xsilo[DCMXBSIZE+1]++;
#endif
	splx(s);
}
 
/*
 * Stop output on a line.
 */
dcmstop(tp, flag)
	register struct tty *tp;
{
	int s;

	s = spltty();
	if (tp->t_state & TS_BUSY) {
		/* XXX is there some way to safely stop transmission? */
		if ((tp->t_state&TS_TTSTOP) == 0)
			tp->t_state |= TS_FLUSH;
	}
	splx(s);
}
 
/*
 * Modem control
 */
dcmmctl(dev, bits, how)
	dev_t dev;
	int bits, how;
{
	register struct dcmdevice *dcm;
	int s, unit, brd, hit = 0;

	unit = UNIT(dev);
#ifdef DEBUG
	if (dcmdebug & DDB_MODEM)
		printf("dcmmctl(%d) unit %d  bits 0x%x how %x\n",
		       BOARD(unit), unit, bits, how);
#endif

	brd = BOARD(unit);
	dcm = dcm_addr[brd];
	s = spltty();
	switch (how) {

	case DMSET:
		dcm_modem[unit]->mdmout = bits;
		hit++;
		break;

	case DMBIS:
		dcm_modem[unit]->mdmout |= bits;
		hit++;
		break;

	case DMBIC:
		dcm_modem[unit]->mdmout &= ~bits;
		hit++;
		break;

	case DMGET:
		bits = dcm_modem[unit]->mdmin;
		if (dcmsoftCAR[brd] & FLAG_STDDCE)
			bits = hp2dce_in(bits);
		break;
	}
	if (hit) {
		SEM_LOCK(dcm);
		dcm->dcm_modemchng |= 1<<(unit & 3);
		dcm->dcm_cr |= CR_MODM;
		SEM_UNLOCK(dcm);
		DELAY(10); /* delay until done */
		(void) splx(s);
	}
	return (bits);
}

/*
 * Set board to either interrupt per-character or at a fixed interval.
 */
dcmsetischeme(brd, flags)
	int brd, flags;
{
	register struct dcmdevice *dcm = dcm_addr[brd];
	register struct dcmischeme *dis = &dcmischeme[brd];
	register int i;
	u_char mask;
	int perchar = flags & DIS_PERCHAR;

#ifdef DEBUG
	if (dcmdebug & DDB_INTSCHM)
		printf("dcmsetischeme(%d, %d): cur %d, ints %d, chars %d\n",
		       brd, perchar, dis->dis_perchar,
		       dis->dis_intr, dis->dis_char);
	if ((flags & DIS_RESET) == 0 && perchar == dis->dis_perchar) {
		printf("dcmsetischeme(%d):  redundent request %d\n",
		       brd, perchar);
		return;
	}
#endif
	/*
	 * If perchar is non-zero, we enable interrupts on all characters
	 * otherwise we disable perchar interrupts and use periodic
	 * polling interrupts.
	 */
	dis->dis_perchar = perchar;
	mask = perchar ? 0xf : 0x0;
	for (i = 0; i < 256; i++)
		dcm->dcm_bmap[i].data_data = mask;
	/*
	 * Don't slow down tandem mode, interrupt on flow control
	 * chars for any port on the board.
	 */
	if (!perchar) {
		register struct tty *tp = &dcm_tty[MKUNIT(brd, 0)];
		int c;

		for (i = 0; i < 4; i++, tp++) {
			if ((c = tp->t_cc[VSTART]) != _POSIX_VDISABLE)
				dcm->dcm_bmap[c].data_data |= (1 << i);
			if ((c = tp->t_cc[VSTOP]) != _POSIX_VDISABLE)
				dcm->dcm_bmap[c].data_data |= (1 << i);
		}
	}
	/*
	 * Board starts with timer disabled so if first call is to
	 * set perchar mode then we don't want to toggle the timer.
	 */
	if (flags == (DIS_RESET|DIS_PERCHAR))
		return;
	/*
	 * Toggle card 16.7ms interrupts (we first make sure that card
	 * has cleared the bit so it will see the toggle).
	 */
	while (dcm->dcm_cr & CR_TIMER)
		;
	SEM_LOCK(dcm);
	dcm->dcm_cr |= CR_TIMER;
	SEM_UNLOCK(dcm);
}

/*
 * Following are all routines needed for DCM to act as console
 */
#include <hp/dev/cons.h>

dcmcnprobe(cp)
	struct consdev *cp;
{
	register struct hp_hw *hw;
	int unit;

	/* locate the major number */
	for (dcmmajor = 0; dcmmajor < nchrdev; dcmmajor++)
		if (cdevsw[dcmmajor].d_open == dcmopen)
			break;

	/*
	 * Implicitly assigns the lowest select code DCM card found to be
	 * logical unit 0 (actually CONUNIT).  If your config file does
	 * anything different, you're screwed.
	 */
	for (hw = sc_table; hw->hw_type; hw++)
		if (HW_ISDEV(hw, D_COMMDCM) && !badaddr((short *)hw->hw_kva))
			break;
	if (!HW_ISDEV(hw, D_COMMDCM)) {
		cp->cn_pri = CN_DEAD;
		return;
	}
	unit = CONUNIT;
	dcm_addr[BOARD(CONUNIT)] = (struct dcmdevice *)hw->hw_kva;

	/* initialize required fields */
	cp->cn_dev = makedev(dcmmajor, unit);
	cp->cn_tp = &dcm_tty[unit];
	switch (dcm_addr[BOARD(unit)]->dcm_rsid) {
	case DCMID:
		cp->cn_pri = CN_NORMAL;
		break;
	case DCMID|DCMCON:
		cp->cn_pri = CN_REMOTE;
		break;
	default:
		cp->cn_pri = CN_DEAD;
		return;
	}
	/*
	 * If dcmconsole is initialized, raise our priority.
	 */
	if (dcmconsole == UNIT(unit))
		cp->cn_pri = CN_REMOTE;
#ifdef KGDB_CHEAT
	/*
	 * This doesn't currently work, at least not with ite consoles;
	 * the console hasn't been initialized yet.
	 */
	if (major(kgdb_dev) == dcmmajor && BOARD(kgdb_dev) == BOARD(unit)) {
		(void) dcminit(kgdb_dev, kgdb_rate);
		if (kgdb_debug_init) {
			/*
			 * We assume that console is ready for us...
			 * this assumes that a dca or ite console
			 * has been selected already and will init
			 * on the first putc.
			 */
			printf("dcm%d: ", UNIT(kgdb_dev));
			kgdb_connect(1);
		}
	}
#endif
}

dcmcninit(cp)
	struct consdev *cp;
{
	dcminit(cp->cn_dev, dcmdefaultrate);
	dcmconsinit = 1;
	dcmconsole = UNIT(cp->cn_dev);
}

dcminit(dev, rate)
	dev_t dev;
	int rate;
{
	register struct dcmdevice *dcm = dcm_addr[BOARD(dev)];
	int s, mode, port;

	port = PORT(dev);
	mode = LC_8BITS | LC_1STOP;
	s = splhigh();
	/*
	 * Wait for transmitter buffer to empty.
	 */
	while (dcm->dcm_thead[port].ptr != dcm->dcm_ttail[port].ptr)
		DELAY(DCM_USPERCH(rate));
	/*
	 * Make changes known to hardware.
	 */
	dcm->dcm_data[port].dcm_baud = ttspeedtab(rate, dcmspeedtab);
	dcm->dcm_data[port].dcm_conf = mode;
	SEM_LOCK(dcm);
	dcm->dcm_cmdtab[port].dcm_data |= CT_CON;
	dcm->dcm_cr |= (1 << port);
	SEM_UNLOCK(dcm);
	/*
	 * Delay for config change to take place. Weighted by baud.
	 * XXX why do we do this?
	 */
	DELAY(16 * DCM_USPERCH(rate));
	splx(s);
}

dcmcngetc(dev)
	dev_t dev;
{
	register struct dcmdevice *dcm = dcm_addr[BOARD(dev)];
	register struct dcmrfifo *fifo;
	register struct dcmpreg *pp;
	register unsigned head;
	int s, c, stat, port;

	port = PORT(dev);
	pp = dcm_preg(dcm, port);
	s = splhigh();
	head = pp->r_head & RX_MASK;
	fifo = &dcm->dcm_rfifos[3-port][head>>1];
	while (head == (pp->r_tail & RX_MASK))
		;
	/*
	 * If board interrupts are enabled, just let our received char
	 * interrupt through in case some other port on the board was
	 * busy.  Otherwise we must clear the interrupt.
	 */
	SEM_LOCK(dcm);
	if ((dcm->dcm_ic & IC_IE) == 0)
		stat = dcm->dcm_iir;
	SEM_UNLOCK(dcm);
	c = fifo->data_char;
	stat = fifo->data_stat;
	pp->r_head = (head + 2) & RX_MASK;
	splx(s);
	return (c);
}

/*
 * Console kernel output character routine.
 */
dcmcnputc(dev, c)
	dev_t dev;
	int c;
{
	register struct dcmdevice *dcm = dcm_addr[BOARD(dev)];
	register struct dcmpreg *pp;
	unsigned tail;
	int s, port, stat;

	port = PORT(dev);
	pp = dcm_preg(dcm, port);
	s = splhigh();
#ifdef KGDB
	if (dev != kgdb_dev)
#endif
	if (dcmconsinit == 0) {
		(void) dcminit(dev, dcmdefaultrate);
		dcmconsinit = 1;
	}
	tail = pp->t_tail & TX_MASK;
	while (tail != (pp->t_head & TX_MASK))
		;
	dcm->dcm_tfifos[3-port][tail].data_char = c;
	pp->t_tail = tail = (tail + 1) & TX_MASK;
	SEM_LOCK(dcm);
	dcm->dcm_cmdtab[port].dcm_data |= CT_TX;
	dcm->dcm_cr |= (1 << port);
	SEM_UNLOCK(dcm);
	while (tail != (pp->t_head & TX_MASK))
		;
	/*
	 * If board interrupts are enabled, just let our completion
	 * interrupt through in case some other port on the board
	 * was busy.  Otherwise we must clear the interrupt.
	 */
	if ((dcm->dcm_ic & IC_IE) == 0) {
		SEM_LOCK(dcm);
		stat = dcm->dcm_iir;
		SEM_UNLOCK(dcm);
	}
	splx(s);
}
#endif
