/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: ite.c 1.28 92/12/20$
 *
 *	@(#)ite.c	7.15 (Berkeley) %G%
 */

/*
 * Bit-mapped display terminal emulator machine independent code.
 * This is a very rudimentary.  Much more can be abstracted out of
 * the hardware dependent routines.
 */
#include "ite.h"
#if NITE > 0

#include "grf.h"

#undef NITE
#define NITE	NGRF

#include <sys/param.h>
#include <sys/conf.h>
#include <sys/proc.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/systm.h>
#include <sys/malloc.h>

#include <hp/dev/grfioctl.h>
#include <hp/dev/grfvar.h>
#include <hp/dev/itevar.h>
#include <hp/dev/kbdmap.h>

#define set_attr(ip, attr)	((ip)->attribute |= (attr))
#define clr_attr(ip, attr)	((ip)->attribute &= ~(attr))

/*
 * No need to raise SPL above the HIL (the only thing that can
 * affect our state.
 */
#include <hp/dev/hilreg.h>
#define splite()		splhil()

/*
 * # of chars are output in a single itestart() call.
 * If this is too big, user processes will be blocked out for
 * long periods of time while we are emptying the queue in itestart().
 * If it is too small, console output will be very ragged.
 */
int	iteburst = 64;

int	nite = NITE;
struct  tty *kbd_tty = NULL;
struct	tty ite_tty[NITE];
struct  ite_softc ite_softc[NITE];

void	itestart(), iterestart();
extern	struct tty *constty;

/*
 * Primary attribute buffer to be used by the first bitmapped console
 * found. Secondary displays alloc the attribute buffer as needed.
 * Size is based on a 68x128 display, which is currently our largest.
 */
u_char  console_attributes[0x2200];

#define ite_erasecursor(ip, sp)	{ \
	if ((ip)->flags & ITE_CURSORON) \
		(*(sp)->ite_cursor)((ip), ERASE_CURSOR); \
}
#define ite_drawcursor(ip, sp) { \
	if ((ip)->flags & ITE_CURSORON) \
		(*(sp)->ite_cursor)((ip), DRAW_CURSOR); \
}
#define ite_movecursor(ip, sp) { \
	if ((ip)->flags & ITE_CURSORON) \
		(*(sp)->ite_cursor)((ip), MOVE_CURSOR); \
}

/*
 * Perform functions necessary to setup device as a terminal emulator.
 */
iteon(dev, flag)
	dev_t dev;
{
	int unit = UNIT(dev);
	struct tty *tp = &ite_tty[unit];
	struct ite_softc *ip = &ite_softc[unit];

	if (unit < 0 || unit >= NITE || (ip->flags&ITE_ALIVE) == 0)
		return(ENXIO);
	/* force ite active, overriding graphics mode */
	if (flag & 1) {
		ip->flags |= ITE_ACTIVE;
		ip->flags &= ~(ITE_INGRF|ITE_INITED);
	}
	/* leave graphics mode */
	if (flag & 2) {
		ip->flags &= ~ITE_INGRF;
		if ((ip->flags & ITE_ACTIVE) == 0)
			return(0);
	}
	ip->flags |= ITE_ACTIVE;
	if (ip->flags & ITE_INGRF)
		return(0);
	if (kbd_tty == NULL || kbd_tty == tp) {
		kbd_tty = tp;
		kbdenable(unit);
	}
	iteinit(dev);
	return(0);
}

iteinit(dev)
     dev_t dev;
{
	int unit = UNIT(dev);
	struct ite_softc *ip = &ite_softc[unit];

	if (ip->flags & ITE_INITED)
		return;
	
	ip->curx = 0;
	ip->cury = 0;
	ip->cursorx = 0;
	ip->cursory = 0;

	(*ip->isw->ite_init)(ip);
	ip->flags |= ITE_CURSORON;
	ite_drawcursor(ip, ip->isw);

	ip->attribute = 0;
	if (ip->attrbuf == NULL)
		ip->attrbuf = (u_char *)
			malloc(ip->rows * ip->cols, M_DEVBUF, M_WAITOK);
	bzero(ip->attrbuf, (ip->rows * ip->cols));

	ip->imode = 0;
	ip->flags |= ITE_INITED;
}

/*
 * "Shut down" device as terminal emulator.
 * Note that we do not deinit the console device unless forced.
 * Deinit'ing the console every time leads to a very active
 * screen when processing /etc/rc.
 */
iteoff(dev, flag)
	dev_t dev;
{
	register struct ite_softc *ip = &ite_softc[UNIT(dev)];

	if (flag & 2)
		ip->flags |= ITE_INGRF;
	if ((ip->flags & ITE_ACTIVE) == 0)
		return;
	if ((flag & 1) ||
	    (ip->flags & (ITE_INGRF|ITE_ISCONS|ITE_INITED)) == ITE_INITED) {
		(*ip->isw->ite_deinit)(ip);
		ip->flags &= ~ITE_CURSORON;
	}
	if ((flag & 2) == 0)
		ip->flags &= ~ITE_ACTIVE;
}

/* ARGSUSED */
#ifdef __STDC__
iteopen(dev_t dev, int mode, int devtype, struct proc *p)
#else
iteopen(dev, mode, devtype, p)
	dev_t dev;
	int mode, devtype;
	struct proc *p;
#endif
{
	int unit = UNIT(dev);
	register struct tty *tp = &ite_tty[unit];
	register struct ite_softc *ip = &ite_softc[unit];
	register int error;
	int first = 0;

	if ((tp->t_state&(TS_ISOPEN|TS_XCLUDE)) == (TS_ISOPEN|TS_XCLUDE)
	    && p->p_ucred->cr_uid != 0)
		return (EBUSY);
	if ((ip->flags & ITE_ACTIVE) == 0) {
		error = iteon(dev, 0);
		if (error)
			return (error);
		first = 1;
	}
	tp->t_oproc = itestart;
	tp->t_param = NULL;
	tp->t_dev = dev;
	if ((tp->t_state&TS_ISOPEN) == 0) {
		ttychars(tp);
		tp->t_iflag = TTYDEF_IFLAG;
		tp->t_oflag = TTYDEF_OFLAG;
		tp->t_cflag = CS8|CREAD;
		tp->t_lflag = TTYDEF_LFLAG;
		tp->t_ispeed = tp->t_ospeed = TTYDEF_SPEED;
		tp->t_state = TS_ISOPEN|TS_CARR_ON;
		ttsetwater(tp);
	}
	error = (*linesw[tp->t_line].l_open)(dev, tp);
	if (error == 0) {
		tp->t_winsize.ws_row = ip->rows;
		tp->t_winsize.ws_col = ip->cols;
	} else if (first)
		iteoff(dev, 0);
	return (error);
}

/*ARGSUSED*/
iteclose(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct tty *tp = &ite_tty[UNIT(dev)];

	(*linesw[tp->t_line].l_close)(tp, flag);
	ttyclose(tp);
	iteoff(dev, 0);
	return(0);
}

iteread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &ite_tty[UNIT(dev)];

	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}

itewrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	int unit = UNIT(dev);
	register struct tty *tp = &ite_tty[unit];

	if ((ite_softc[unit].flags & ITE_ISCONS) && constty &&
	    (constty->t_state&(TS_CARR_ON|TS_ISOPEN))==(TS_CARR_ON|TS_ISOPEN))
		tp = constty;
	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}

iteioctl(dev, cmd, addr, flag, p)
	dev_t dev;
	int cmd;
	caddr_t addr;
	int flag;
	struct proc *p;
{
	register struct tty *tp = &ite_tty[UNIT(dev)];
	int error;

	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, addr, flag, p);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, addr, flag);
	if (error >= 0)
		return (error);
	return (ENOTTY);
}

void
iterestart(tp)
	register struct tty *tp;
{
	register int s = splite();

	tp->t_state &= ~TS_TIMEOUT;
	itestart(tp);
	splx(s);
}

void
itestart(tp)
	register struct tty *tp;
{
	register int cc, s;
	int hiwat = 0;
	struct ite_softc *ip;

	/*
	 * (Potentially) lower priority.  We only need to protect ourselves
	 * from keyboard interrupts since that is all that can affect the
	 * state of our tty (kernel printf doesn't go through this routine).
	 */
	s = splite();
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP)) {
		splx(s);
		return;
	}
	tp->t_state |= TS_BUSY;
	cc = tp->t_outq.c_cc;
	if (cc <= tp->t_lowat) {
		if (tp->t_state & TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		selwakeup(&tp->t_wsel);
	}
	/*
	 * Handle common (?) case
	 */
	if (cc == 1) {
		iteputchar(getc(&tp->t_outq), tp->t_dev);
	} else if (cc) {
		/*
		 * Limit the amount of output we do in one burst
		 * to prevent hogging the CPU.
		 */
		if (cc > iteburst) {
			hiwat++;
			cc = iteburst;
		}
		/*
		 * Turn off cursor while we output multiple characters.
		 * Saves a lot of expensive window move operations.
		 */
		ip = &ite_softc[UNIT(tp->t_dev)];
		ite_erasecursor(ip, ip->isw);
		ip->flags &= ~ITE_CURSORON;
		while (--cc >= 0)
			iteputchar(getc(&tp->t_outq), tp->t_dev);
		ip->flags |= ITE_CURSORON;
		ite_drawcursor(ip, ip->isw);
		if (hiwat) {
			tp->t_state |= TS_TIMEOUT;
			timeout(iterestart, tp, 1);
		}
	}
	tp->t_state &= ~TS_BUSY;
	splx(s);
}

itefilter(stat, c)
     register char stat, c;
{
	static int capsmode = 0;
	static int metamode = 0;
  	register char code, *str;

	if (kbd_tty == NULL)
		return;

	switch (c & 0xFF) {
	case KBD_CAPSLOCK:
		capsmode = !capsmode;
		return;

	case KBD_EXT_LEFT_DOWN:
	case KBD_EXT_RIGHT_DOWN:
		metamode = 1;
		return;
		
	case KBD_EXT_LEFT_UP:
	case KBD_EXT_RIGHT_UP:
		metamode = 0;
		return;
	}

	c &= KBD_CHARMASK;
	switch ((stat>>KBD_SSHIFT) & KBD_SMASK) {

	case KBD_KEY:
	        if (!capsmode) {
			code = kbd_keymap[c];
			break;
		}
		/* fall into... */

	case KBD_SHIFT:
		code = kbd_shiftmap[c];
		break;

	case KBD_CTRL:
		code = kbd_ctrlmap[c];
		break;
		
	case KBD_CTRLSHIFT:	
		code = kbd_ctrlshiftmap[c];
		break;
        }

	if (code == NULL && (str = kbd_stringmap[c]) != NULL) {
		while (*str)
			(*linesw[kbd_tty->t_line].l_rint)(*str++, kbd_tty);
	} else {
		if (metamode)
			code |= 0x80;
		(*linesw[kbd_tty->t_line].l_rint)(code, kbd_tty);
	}
}

iteputchar(c, dev)
	register int c;
	dev_t dev;  
{
	int unit = UNIT(dev);
	register struct ite_softc *ip = &ite_softc[unit];
	register struct itesw *sp = ip->isw;
	register int n;

	if ((ip->flags & (ITE_ACTIVE|ITE_INGRF)) != ITE_ACTIVE)
	  	return;

	if (ip->escape) {
doesc:
		switch (ip->escape) {

		case '&':			/* Next can be a,d, or s */
			if (ip->fpd++) {
				ip->escape = c;
				ip->fpd = 0;
			}
			return;

		case 'a':				/* cursor change */
			switch (c) {

			case 'Y':			/* Only y coord. */
				ip->cury = min(ip->pos, ip->rows-1);
				ip->pos = 0;
				ip->escape = 0;
				ite_movecursor(ip, sp);
				clr_attr(ip, ATTR_INV);
				break;

			case 'y':			/* y coord first */
				ip->cury = min(ip->pos, ip->rows-1);
				ip->pos = 0;
				ip->fpd = 0;
				break;

			case 'C':			/* x coord */
				ip->curx = min(ip->pos, ip->cols-1);
				ip->pos = 0;
				ip->escape = 0;
				ite_movecursor(ip, sp);
				clr_attr(ip, ATTR_INV);
				break;

			default:	     /* Possibly a 3 digit number. */
				if (c >= '0' && c <= '9' && ip->fpd < 3) {
					ip->pos = ip->pos * 10 + (c - '0');
					ip->fpd++;
				} else {
					ip->pos = 0;
					ip->escape = 0;
				}
				break;
			}
			return;

		case 'd':				/* attribute change */
			switch (c) {

			case 'B':
				set_attr(ip, ATTR_INV);
				break;
		        case 'D':
				/* XXX: we don't do anything for underline */
				set_attr(ip, ATTR_UL);
				break;
		        case '@':
				clr_attr(ip, ATTR_ALL);
				break;
			}
			ip->escape = 0;
			return;

		case 's':				/* keypad control */
			switch (ip->fpd) {

			case 0:
				ip->hold = c;
				ip->fpd++;
				return;

			case 1:
				if (c == 'A') {
					switch (ip->hold) {
	
					case '0':
						clr_attr(ip, ATTR_KPAD);
						break;
					case '1':
						set_attr(ip, ATTR_KPAD);
						break;
					}
				}
				ip->hold = 0;
			}
			ip->escape = 0;
			return;

		case 'i':			/* back tab */
			if (ip->curx > TABSIZE) {
				n = ip->curx - (ip->curx & (TABSIZE - 1));
				ip->curx -= n;
			} else
				ip->curx = 0;
			ite_movecursor(ip, sp);
			ip->escape = 0;
			return;

		case '3':			/* clear all tabs */
			goto ignore;

		case 'K':			/* clear_eol */
			ite_clrtoeol(ip, sp, ip->cury, ip->curx);
			ip->escape = 0;
			return;

		case 'J':			/* clear_eos */
			ite_clrtoeos(ip, sp);
			ip->escape = 0;
			return;

		case 'B':			/* cursor down 1 line */
			if (++ip->cury == ip->rows) {
				--ip->cury;
				ite_erasecursor(ip, sp);
				(*sp->ite_scroll)(ip, 1, 0, 1, SCROLL_UP);
				ite_clrtoeol(ip, sp, ip->cury, 0);
			}
			else
				ite_movecursor(ip, sp);
			clr_attr(ip, ATTR_INV);
			ip->escape = 0;
			return;

		case 'C':			/* cursor forward 1 char */
			ip->escape = 0;
			itecheckwrap(ip, sp);
			return;

		case 'A':			/* cursor up 1 line */
			if (ip->cury > 0) {
				ip->cury--;
				ite_movecursor(ip, sp);
			}
			ip->escape = 0;
			clr_attr(ip, ATTR_INV);
			return;

		case 'P':			/* delete character */
			ite_dchar(ip, sp);
			ip->escape = 0;
			return;

		case 'M':			/* delete line */
			ite_dline(ip, sp);
			ip->escape = 0;
			return;

		case 'Q':			/* enter insert mode */
			ip->imode = 1;
			ip->escape = 0;
			return;

		case 'R':			/* exit insert mode */
			ip->imode = 0;
			ip->escape = 0;
			return;

		case 'L':			/* insert blank line */
			ite_iline(ip, sp);
			ip->escape = 0;
			return;

		case 'h':			/* home key */
			ip->cury = ip->curx = 0;
			ite_movecursor(ip, sp);
			ip->escape = 0;
			return;

		case 'D':			/* left arrow key */
			if (ip->curx > 0) {
				ip->curx--;
				ite_movecursor(ip, sp);
			}
			ip->escape = 0;
			return;

		case '1':			/* set tab in all rows */
			goto ignore;

		case ESC:
			if ((ip->escape = c) == ESC)
				break;
			ip->fpd = 0;
			goto doesc;

		default:
ignore:
			ip->escape = 0;
			return;

		}
	}

	switch (c &= 0x7F) {

	case '\n':

		if (++ip->cury == ip->rows) {
			--ip->cury;
			ite_erasecursor(ip, sp);
			(*sp->ite_scroll)(ip, 1, 0, 1, SCROLL_UP);
			ite_clrtoeol(ip, sp, ip->cury, 0);
		} else
			ite_movecursor(ip, sp);
		clr_attr(ip, ATTR_INV);
		break;

	case '\r':
		if (ip->curx) {
			ip->curx = 0;
			ite_movecursor(ip, sp);
		}
		break;
	
	case '\b':
		if (--ip->curx < 0)
			ip->curx = 0;
		else
			ite_movecursor(ip, sp);
		break;

	case '\t':
		if (ip->curx < TABEND(unit)) {
			n = TABSIZE - (ip->curx & (TABSIZE - 1));
			ip->curx += n;
			ite_movecursor(ip, sp);
		} else
			itecheckwrap(ip, sp);
		break;

	case CTRL('G'):
		if (&ite_tty[unit] == kbd_tty)
			kbdbell(unit);
		break;

	case ESC:
		ip->escape = ESC;
		break;

	default:
		if (c < ' ' || c == DEL)
			break;
		if (ip->imode)
			ite_ichar(ip, sp);
		if ((ip->attribute & ATTR_INV) || attrtest(ip, ATTR_INV)) {
			attrset(ip, ATTR_INV);
			(*sp->ite_putc)(ip, c, ip->cury, ip->curx, ATTR_INV);
		} else
			(*sp->ite_putc)(ip, c, ip->cury, ip->curx, ATTR_NOR);
		ite_drawcursor(ip, sp);
		itecheckwrap(ip, sp);
		break;
	}
}

itecheckwrap(ip, sp)
     register struct ite_softc *ip;
     register struct itesw *sp;
{
	if (++ip->curx == ip->cols) {
		ip->curx = 0;
		clr_attr(ip, ATTR_INV);
		if (++ip->cury == ip->rows) {
			--ip->cury;
			ite_erasecursor(ip, sp);
			(*sp->ite_scroll)(ip, 1, 0, 1, SCROLL_UP);
			ite_clrtoeol(ip, sp, ip->cury, 0);
			return;
		}
	}
	ite_movecursor(ip, sp);
}

ite_dchar(ip, sp)
     register struct ite_softc *ip;
     register struct itesw *sp;
{
	if (ip->curx < ip->cols - 1) {
		ite_erasecursor(ip, sp);
		(*sp->ite_scroll)(ip, ip->cury, ip->curx + 1, 1, SCROLL_LEFT);
		attrmov(ip, ip->cury, ip->curx + 1, ip->cury, ip->curx,
			1, ip->cols - ip->curx - 1);
	}
	attrclr(ip, ip->cury, ip->cols - 1, 1, 1);
	(*sp->ite_putc)(ip, ' ', ip->cury, ip->cols - 1, ATTR_NOR);
	ite_drawcursor(ip, sp);
}

ite_ichar(ip, sp)
     register struct ite_softc *ip;
     register struct itesw *sp;
{
	if (ip->curx < ip->cols - 1) {
		ite_erasecursor(ip, sp);
		(*sp->ite_scroll)(ip, ip->cury, ip->curx, 1, SCROLL_RIGHT);
		attrmov(ip, ip->cury, ip->curx, ip->cury, ip->curx + 1,
			1, ip->cols - ip->curx - 1);
	}
	attrclr(ip, ip->cury, ip->curx, 1, 1);
	(*sp->ite_putc)(ip, ' ', ip->cury, ip->curx, ATTR_NOR);
	ite_drawcursor(ip, sp);
}

ite_dline(ip, sp)
     register struct ite_softc *ip;
     register struct itesw *sp;
{
	if (ip->cury < ip->rows - 1) {
		ite_erasecursor(ip, sp);
		(*sp->ite_scroll)(ip, ip->cury + 1, 0, 1, SCROLL_UP);
		attrmov(ip, ip->cury + 1, 0, ip->cury, 0,
			ip->rows - ip->cury - 1, ip->cols);
	}
	ite_clrtoeol(ip, sp, ip->rows - 1, 0);
}

ite_iline(ip, sp)
     register struct ite_softc *ip;
     register struct itesw *sp;
{
	if (ip->cury < ip->rows - 1) {
		ite_erasecursor(ip, sp);
		(*sp->ite_scroll)(ip, ip->cury, 0, 1, SCROLL_DOWN);
		attrmov(ip, ip->cury, 0, ip->cury + 1, 0,
			ip->rows - ip->cury - 1, ip->cols);
	}
	ite_clrtoeol(ip, sp, ip->cury, 0);
}

ite_clrtoeol(ip, sp, y, x)
     register struct ite_softc *ip;
     register struct itesw *sp;
     register int y, x;
{
	(*sp->ite_clear)(ip, y, x, 1, ip->cols - x);
	attrclr(ip, y, x, 1, ip->cols - x);
	ite_drawcursor(ip, sp);
}

ite_clrtoeos(ip, sp)
     register struct ite_softc *ip;
     register struct itesw *sp;
{
	(*sp->ite_clear)(ip, ip->cury, 0, ip->rows - ip->cury, ip->cols);
	attrclr(ip, ip->cury, 0, ip->rows - ip->cury, ip->cols);
	ite_drawcursor(ip, sp);
}

/*
 * Console functions
 */
#include <hp/dev/cons.h>
#ifdef hp300
#include <hp/dev/grfreg.h>
#endif

#ifdef DEBUG
/*
 * Minimum ITE number at which to start looking for a console.
 * Setting to 0 will do normal search, 1 will skip first ITE device,
 * NITE will skip ITEs and use serial port.
 */
int	whichconsole = 0;
#endif

itecnprobe(cp)
	struct consdev *cp;
{
	register struct ite_softc *ip;
	int i, sw, maj, unit, pri;

	/* locate the major number */
	for (maj = 0; maj < nchrdev; maj++)
		if (cdevsw[maj].d_open == iteopen)
			break;

	/* urk! */
	grfconfig();

	/* check all the individual displays and find the best */
	unit = -1;
	pri = CN_DEAD;
	for (i = 0; i < NITE; i++) {
		struct grf_softc *gp = &grf_softc[i];

		ip = &ite_softc[i];
		if ((gp->g_flags & GF_ALIVE) == 0)
			continue;
		ip->flags = (ITE_ALIVE|ITE_CONSOLE);

		/* locate the proper switch table. */
		for (sw = 0; sw < nitesw; sw++)
			if (itesw[sw].ite_hwid == gp->g_sw->gd_hwid)
				break;

		if (sw == nitesw)
			continue;
#ifdef DEBUG
		if (i < whichconsole)
			continue;
#endif
		ip->isw = &itesw[sw];
		ip->grf = gp;
#ifdef hp300
		if ((int)gp->g_display.gd_regaddr == GRFIADDR) {
			pri = CN_INTERNAL;
			unit = i;
		} else if (unit < 0) {
			pri = CN_NORMAL;
			unit = i;
		}
#endif
#ifdef hp800
		/* XXX use the first one for now */
		if (unit < 0) {
			pri = CN_INTERNAL;
			unit = i;
		}
#endif
	}

	/* initialize required fields */
	cp->cn_dev = makedev(maj, unit);
	cp->cn_tp = &ite_tty[unit];
	cp->cn_pri = pri;
}

itecninit(cp)
	struct consdev *cp;
{
	int unit = UNIT(cp->cn_dev);
	struct ite_softc *ip = &ite_softc[unit];

	ip->attrbuf = console_attributes;
	iteinit(cp->cn_dev);
	ip->flags |= (ITE_ACTIVE|ITE_ISCONS);
	kbd_tty = &ite_tty[unit];
}

/*ARGSUSED*/
itecngetc(dev)
	dev_t dev;
{
	register int c;
	int stat;

	c = kbdgetc(0, &stat);	/* XXX always read from keyboard 0 for now */
	switch ((stat >> KBD_SSHIFT) & KBD_SMASK) {
	case KBD_SHIFT:
		c = kbd_shiftmap[c & KBD_CHARMASK];
		break;
	case KBD_CTRL:
		c = kbd_ctrlmap[c & KBD_CHARMASK];
		break;
	case KBD_KEY:
		c = kbd_keymap[c & KBD_CHARMASK];
		break;
	default:
		c = 0;
		break;
	}
	return(c);
}

itecnputc(dev, c)
	dev_t dev;
	int c;
{
	static int paniced = 0;
	struct ite_softc *ip = &ite_softc[UNIT(dev)];

	if (panicstr && !paniced &&
	    (ip->flags & (ITE_ACTIVE|ITE_INGRF)) != ITE_ACTIVE) {
		(void) iteon(dev, 3);
		paniced = 1;
	}
	iteputchar(c, dev);
}
#endif
