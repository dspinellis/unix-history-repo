/*	pty.c	4.8	81/08/14	*/

/*
 * Pseudo-teletype Driver
 * (Actually two drivers, requiring two entries in 'cdevsw')
 */
#include "pty.h"

#if NPTY > 0

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/tty.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/file.h"

#define NPTY 16			/* Number of pseudo-teletypes */
#define BUFSIZ 100		/* Chunk size iomoved from user */
#define ALLDELAYS (NLDELAY|TBDELAY|XTABS|CRDELAY|VTDELAY)
/*
 * A pseudo-teletype is a special device which is not unlike a pipe.
 * It is used to communicate between two processes.  However, it allows
 * one to simulate a teletype, including mode setting, interrupt, and
 * multiple end of files (all not possible on a pipe).	There are
 * really two drivers here.  One is the device which looks like a TTY
 * and can be thought of as the slave device, and hence its routines
 * are prefixed with 'pts' (PTY Slave).	 The other driver can be
 * thought of as the controlling device, and its routines are prefixed
 * by 'ptc' (PTY Controller).  To type on the simulated keyboard of the
 * PTY, one does a 'write' to the controlling device.  To get the
 * simulated printout from the PTY, one does a 'read' on the controlling
 * device.  Normally, the controlling device is called 'ptyx' and the
 * slave device is called 'ttyx' (to make programs like 'who' happy).
 */

struct tty pt_tty[NPTY];		/* TTY headers for PTYs */

/*ARGSUSED*/
ptsopen(dev, flag)
dev_t dev;
{					/* Open for PTY Slave */
	register struct tty *tp;

	if(minor(dev) >= NPTY) {
		u.u_error = ENXIO;
		return;
	}
	tp = &pt_tty[minor(dev)];
	if((tp->t_state & ISOPEN) == 0) {
		ttychars(tp);		/* Set up default chars */
		tp->t_flags = 0;	/* No features (nor raw mode) */
	} else if(tp->t_state&XCLUDE && u.u_uid != 0) {
		u.u_error = EBUSY;
		return;
	}
	if(tp->t_oproc)			/* Ctrlr still around. */
		tp->t_state |= CARR_ON;
	while((tp->t_state & CARR_ON) == 0) {
		tp->t_state |= WOPEN;
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	}
	(*linesw[tp->t_line].l_open)(dev, tp);
}

ptsclose(dev)
dev_t dev;
{					/* Close slave part of PTY */
	register struct tty *tp;

	tp = &pt_tty[minor(dev)];
	(*linesw[tp->t_line].l_close)(tp);
}

ptsread(dev)
dev_t dev;
{	/* Read from PTY, i.e. from data written by controlling device */
	register struct tty    *tp;

	tp = &pt_tty[minor(dev)];
	if(tp->t_oproc) {
		(*linesw[tp->t_line].l_read)(tp);
				/* Wakeup other half if sleeping */
		wakeup((caddr_t)&tp->t_rawq.c_cf);
	}
}

ptswrite(dev)
dev_t dev;
{			/* Write on PTY, i.e. to be read from
			   controlling device */
	register struct tty *tp;

	tp = &pt_tty[minor(dev)];
			/* Wait for controlling device to be opened */
	if(tp->t_oproc)
		(*linesw[tp->t_line].l_write)(tp);
}

ptsstart(tp)
struct tty *tp;
{			/* Called by 'ttstart' to output a character.
			   Merely wakes up controlling half, which
			   does actual work */
	if(tp->t_state & TTSTOP)
		return;
	wakeup((caddr_t)&tp->t_outq.c_cf);
}

/*ARGSUSED*/
ptcopen(dev, flag)
dev_t dev;
{				/* Open for PTY Controller */
	register struct tty *tp;

	if(minor(dev) >= NPTY) {
		u.u_error = ENXIO;
		return;
	}
	tp = &pt_tty[minor(dev)];
	if(tp->t_oproc) {
		u.u_error = EIO;
		return;
	}
	tp->t_oproc = ptsstart;		/* Set address of start routine */
	tp->t_iproc = 0;
	if(tp->t_state & WOPEN)
		wakeup((caddr_t)&tp->t_rawq);
	tp->t_state |= CARR_ON;
}

ptcclose(dev)
dev_t dev;
{					/* Close controlling part of PTY */
	register struct tty *tp;

	tp = &pt_tty[minor(dev)];
	if(tp->t_state & ISOPEN)
		gsignal(tp->t_pgrp, SIGHUP);
	tp->t_state &= ~CARR_ON;	/* Virtual carrier is gone */
	flushtty(tp, FREAD|FWRITE);		     /* Clean things up */
	tp->t_oproc = 0;		/* Mark as closed */
}

ptcread(dev)
dev_t dev;
{					/* Read from PTY's output buffer */
	register struct tty *tp;

	tp = &pt_tty[minor(dev)];
	if((tp->t_state&(CARR_ON|ISOPEN)) == 0)
		return;
	while(tp->t_outq.c_cc == 0 ||	/* Wait for something to arrive */
	      (tp->t_state&TTSTOP))	/* (Woken by ptsstart) */
		sleep((caddr_t)&tp->t_outq.c_cf, TTIPRI);
	while(tp->t_outq.c_cc && passc(getc(&tp->t_outq)) >= 0);
	if(tp->t_outq.c_cc <= TTLOWAT(tp)  && (tp->t_state&ASLEEP)) {
		tp->t_state &= ~ASLEEP;
		if(tp->t_chan)
			mcstart(tp->t_chan, (caddr_t)&tp->t_outq);
		else
			wakeup((caddr_t)&tp->t_outq);
	}
}

ptcwrite(dev)
dev_t dev;
{			/* Stuff characters into PTY's input buffer */
	register struct tty *tp;
	register char *cp, *ce;
	register int cc;
	char locbuf[BUFSIZ];

	tp = &pt_tty[minor(dev)];
	if((tp->t_state&(CARR_ON|ISOPEN)) == 0)
		return;
	while(u.u_count) {
		cc = MIN(u.u_count, BUFSIZ);
		cp = locbuf;
		iomove(cp, (unsigned)cc, B_WRITE);
		if(u.u_error)
			break;
		ce = cp + cc;
		while(cp < ce) {
			while(tp->t_delct && tp->t_rawq.c_cc >= TTYHOG - 2) {
				wakeup((caddr_t)&tp->t_rawq);
				/* Better than just flushing it! */
				/* Wait for something to be read */
				sleep((caddr_t)&tp->t_rawq.c_cf, TTOPRI);
			}
			(*linesw[tp->t_line].l_rint)(*cp++, tp);
		}
	}
}

/* Note: Both slave and controlling device have the same routine for */
/* 'ioctl' (but note check for controller - 4/12/78:mob)*/
/*ARGSUSED*/
ptyioctl(dev, cmd, addr, flag)
caddr_t addr;
dev_t dev;
{					/* Read and write status bits */
	register struct tty *tp;
	register int tbd;
#ifdef BLAND
	register int nld;
#endif

	tp = &pt_tty[minor(dev)];
		/* if controller stty then must flush to prevent a hang */
	if(cdevsw[major(dev)].d_open == ptcopen && cmd == TIOCSETP)
		while(getc(&tp->t_outq) >= 0);
	if(ttioctl(tp, cmd, addr, dev)) {
		if(cmd == TIOCSETP || cmd == TIOCSETN) {
#ifdef BLAND
			nld = tp->t_flags & NLDELAY;
#endif
			tbd = tp->t_flags & TBDELAY;
			tp->t_flags &= ~ALLDELAYS;
			if(tbd == TBDELAY)	/* Wants tab expansion */
				tp->t_flags |= tbd;
#ifdef BLAND
			if(nld == NLDELAY)	/* Allow ANN ARBOR mode. */
				tp->t_flags |= nld;
#endif
		}
	} else
		u.u_error = ENOTTY;
}
#endif
