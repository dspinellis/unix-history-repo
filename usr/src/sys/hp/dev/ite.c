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
 * from: Utah $Hdr: ite.c 1.1 90/07/09$
 *
 *	@(#)ite.c	7.4 (Berkeley) %G%
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

#include "sys/param.h"
#include "sys/conf.h"
#include "sys/user.h"
#include "sys/proc.h"
#include "sys/ioctl.h"
#include "sys/tty.h"
#include "sys/systm.h"
#include "sys/uio.h"
#include "sys/malloc.h"

#include "itevar.h"
#include "iteioctl.h"
#include "kbdmap.h"

#include "../include/cpu.h"

#define set_attr(ip, attr)	((ip)->attribute |= (attr))
#define clr_attr(ip, attr)	((ip)->attribute &= ~(attr))

extern  int nodev();

int topcat_scroll(),	topcat_init(),		topcat_deinit();
int topcat_clear(),	topcat_putc(),	 	topcat_cursor();
int gatorbox_scroll(),	gatorbox_init(),	gatorbox_deinit();
int gatorbox_clear(),	gatorbox_putc(), 	gatorbox_cursor();
int rbox_scroll(),	rbox_init(),		rbox_deinit();
int rbox_clear(),	rbox_putc(), 		rbox_cursor();
int dvbox_scroll(),	dvbox_init(),		dvbox_deinit();
int dvbox_clear(),	dvbox_putc(), 		dvbox_cursor();

struct itesw itesw[] =
{
	topcat_init,		topcat_deinit,		topcat_clear,
	topcat_putc,		topcat_cursor,		topcat_scroll,

	gatorbox_init,		gatorbox_deinit,	gatorbox_clear,
	gatorbox_putc,		gatorbox_cursor,	gatorbox_scroll,

	rbox_init,		rbox_deinit,		rbox_clear,
	rbox_putc,		rbox_cursor,		rbox_scroll,

	dvbox_init,		dvbox_deinit,		dvbox_clear,
	dvbox_putc,		dvbox_cursor,		dvbox_scroll,
};

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

int	itestart();
extern	int ttrstrt();
extern	struct tty *constty;

/*
 * Primary attribute buffer to be used by the first bitmapped console
 * found. Secondary displays alloc the attribute buffer as needed.
 * Size is based on a 68x128 display, which is currently our largest.
 */
u_char  console_attributes[0x2200];

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
		kbdenable();
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

	(*itesw[ip->type].ite_init)(ip);
	(*itesw[ip->type].ite_cursor)(ip, DRAW_CURSOR);

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
	    (ip->flags & (ITE_INGRF|ITE_ISCONS|ITE_INITED)) == ITE_INITED)
		(*itesw[ip->type].ite_deinit)(ip);
	if ((flag & 2) == 0)
		ip->flags &= ~ITE_ACTIVE;
}

/*ARGSUSED*/
iteopen(dev, flag)
	dev_t dev;
{
	int unit = UNIT(dev);
	register struct tty *tp = &ite_tty[unit];
	register struct ite_softc *ip = &ite_softc[unit];
	register int error;
	int first = 0;

	if ((tp->t_state&(TS_ISOPEN|TS_XCLUDE)) == (TS_ISOPEN|TS_XCLUDE)
	    && u.u_uid != 0)
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
iteclose(dev, flag)
	dev_t dev;
{
	register struct tty *tp = &ite_tty[UNIT(dev)];

	(*linesw[tp->t_line].l_close)(tp);
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

iteioctl(dev, cmd, addr, flag)
	dev_t dev;
	caddr_t addr;
{
	register struct tty *tp = &ite_tty[UNIT(dev)];
	int error;

	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, addr, flag);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, addr, flag);
	if (error >= 0)
		return (error);
	return (ENOTTY);
}

itestart(tp)
	register struct tty *tp;
{
	register int cc, s;
	int hiwat = 0;

	s = spltty();
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP)) {
		splx(s);
		return;
	}
	tp->t_state |= TS_BUSY;
	cc = tp->t_outq.c_cc;
	if (cc <= tp->t_lowat) {
		if (tp->t_state & TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup(&tp->t_outq);
		}
		if (tp->t_wsel) {
			selwakeup(tp->t_wsel, tp->t_state & TS_WCOLL);
			tp->t_wsel = 0;
			tp->t_state &= ~TS_WCOLL;
		}
	}
	/*
	 * Limit the amount of output we do in one burst
	 * to prevent hogging the CPU.
	 */
	if (cc > iteburst) {
		hiwat++;
		cc = iteburst;
	}
	while (--cc >= 0) {
		register int c;

		c = getc(&tp->t_outq);
		/*
		 * iteputchar() may take a long time and we don't want to
		 * block all interrupts for long periods of time.  Since
		 * there is no need to stay at high priority while outputing
		 * the character (since we don't have to worry about
		 * interrupts), we don't.  We just need to make sure that
		 * we don't reenter iteputchar, which is guarenteed by the
		 * earlier setting of TS_BUSY.
		 */
		splx(s);
		iteputchar(c, tp->t_dev);
		spltty();
	}
	if (hiwat) {
		tp->t_state |= TS_TIMEOUT;
		timeout(ttrstrt, tp, 1);
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
	register struct itesw *sp = &itesw[ip->type];
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
				ip->cury = MIN(ip->pos, ip->rows-1);
				ip->pos = 0;
				ip->escape = 0;
				(*sp->ite_cursor)(ip, MOVE_CURSOR);
				clr_attr(ip, ATTR_INV);
				break;

			case 'y':			/* y coord first */
				ip->cury = MIN(ip->pos, ip->rows-1);
				ip->pos = 0;
				ip->fpd = 0;
				break;

			case 'C':			/* x coord */
				ip->curx = MIN(ip->pos, ip->cols-1);
				ip->pos = 0;
				ip->escape = 0;
				(*sp->ite_cursor)(ip, MOVE_CURSOR);
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
			(*sp->ite_cursor)(ip, MOVE_CURSOR);
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
				(*sp->ite_scroll)(ip, 1, 0, 1, SCROLL_UP);
				ite_clrtoeol(ip, sp, ip->cury, 0);
			}
			else
				(*sp->ite_cursor)(ip, MOVE_CURSOR);
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
				(*sp->ite_cursor)(ip, MOVE_CURSOR);
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
			(*sp->ite_cursor)(ip, MOVE_CURSOR);
			ip->escape = 0;
			return;

		case 'D':			/* left arrow key */
			if (ip->curx > 0) {
				ip->curx--;
				(*sp->ite_cursor)(ip, MOVE_CURSOR);
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
			(*sp->ite_scroll)(ip, 1, 0, 1, SCROLL_UP);
			ite_clrtoeol(ip, sp, ip->cury, 0);
		}
		else
			(*sp->ite_cursor)(ip, MOVE_CURSOR);
		clr_attr(ip, ATTR_INV);
		break;

	case '\r':
		if (ip->curx) {
			ip->curx = 0;
			(*sp->ite_cursor)(ip, MOVE_CURSOR);
		}
		break;
	
	case '\b':
		if (--ip->curx < 0)
			ip->curx = 0;
		else
			(*sp->ite_cursor)(ip, MOVE_CURSOR);
		break;

	case '\t':
		if (ip->curx < TABEND(unit)) {
			n = TABSIZE - (ip->curx & (TABSIZE - 1));
			ip->curx += n;
			(*sp->ite_cursor)(ip, MOVE_CURSOR);
		} else
			itecheckwrap(ip, sp);
		break;

	case CTRL('G'):
		if (&ite_tty[unit] == kbd_tty)
			kbdbell();
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
		}			
		else
			(*sp->ite_putc)(ip, c, ip->cury, ip->curx, ATTR_NOR);
		(*sp->ite_cursor)(ip, DRAW_CURSOR);
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
			(*sp->ite_scroll)(ip, 1, 0, 1, SCROLL_UP);
			ite_clrtoeol(ip, sp, ip->cury, 0);
			return;
		}
	}
	(*sp->ite_cursor)(ip, MOVE_CURSOR);
}

ite_dchar(ip, sp)
     register struct ite_softc *ip;
     register struct itesw *sp;
{
	(*sp->ite_scroll)(ip, ip->cury, ip->curx + 1, 1, SCROLL_LEFT);
	attrmov(ip, ip->cury, ip->curx + 1, ip->cury, ip->curx,
		1, ip->cols - ip->curx - 1);
	attrclr(ip, ip->cury, ip->cols - 1, 1, 1);
	(*sp->ite_putc)(ip, ' ', ip->cury, ip->cols - 1, ATTR_NOR);
	(*sp->ite_cursor)(ip, DRAW_CURSOR);
}

ite_ichar(ip, sp)
     register struct ite_softc *ip;
     register struct itesw *sp;
{
	(*sp->ite_scroll)(ip, ip->cury, ip->curx, 1, SCROLL_RIGHT);
	attrmov(ip, ip->cury, ip->curx, ip->cury, ip->curx + 1,
		1, ip->cols - ip->curx - 1);
	attrclr(ip, ip->cury, ip->curx, 1, 1);
	(*sp->ite_putc)(ip, ' ', ip->cury, ip->curx, ATTR_NOR);
	(*sp->ite_cursor)(ip, DRAW_CURSOR);
}

ite_dline(ip, sp)
     register struct ite_softc *ip;
     register struct itesw *sp;
{
	(*sp->ite_scroll)(ip, ip->cury + 1, 0, 1, SCROLL_UP);
	attrmov(ip, ip->cury + 1, 0, ip->cury, 0,
		ip->rows - ip->cury - 1, ip->cols);
	ite_clrtoeol(ip, sp, ip->rows - 1, 0);
}

ite_iline(ip, sp)
     register struct ite_softc *ip;
     register struct itesw *sp;
{
	(*sp->ite_scroll)(ip, ip->cury, 0, 1, SCROLL_DOWN);
	attrmov(ip, ip->cury, 0, ip->cury + 1, 0,
		ip->rows - ip->cury - 1, ip->cols);
	ite_clrtoeol(ip, sp, ip->cury, 0);
}

ite_clrtoeol(ip, sp, y, x)
     register struct ite_softc *ip;
     register struct itesw *sp;
     register int y, x;
{
	(*sp->ite_clear)(ip, y, x, 1, ip->cols - x);
	attrclr(ip, y, x, 1, ip->cols - x);
	(*sp->ite_cursor)(ip, DRAW_CURSOR);
}

ite_clrtoeos(ip, sp)
     register struct ite_softc *ip;
     register struct itesw *sp;
{
	(*sp->ite_clear)(ip, ip->cury, 0, ip->rows - ip->cury, ip->cols);
	attrclr(ip, ip->cury, 0, ip->rows - ip->cury, ip->cols);
	(*sp->ite_cursor)(ip, DRAW_CURSOR);
}

/*
 * Console functions
 */
#include "../hp300/cons.h"
#include "grfioctl.h"
#include "grfvar.h"

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
	int i, maj, unit, pri;
	extern int iteopen();

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

		/* XXX - we need to do something about mapping these */
		switch (gp->g_type) {

		case GT_TOPCAT:
		case GT_LRCATSEYE:
		case GT_HRCCATSEYE:
		case GT_HRMCATSEYE:
			ip->type = ITE_TOPCAT;
			break;
		case GT_GATORBOX:
			ip->type = ITE_GATORBOX;
			break;
		case GT_RENAISSANCE:
			ip->type = ITE_RENAISSANCE;
			break;
		case GT_DAVINCI:
			ip->type = ITE_DAVINCI;
			break;
		}
#ifdef DEBUG
		if (i < whichconsole)
			continue;
#endif
		if ((int)gp->g_display.gd_regaddr == GRFIADDR) {
			pri = CN_INTERNAL;
			unit = i;
		} else if (unit < 0) {
			pri = CN_NORMAL;
			unit = i;
		}
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

	c = kbdgetc(&stat);
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
	extern char *panicstr;

	if (panicstr && !paniced &&
	    (ip->flags & (ITE_ACTIVE|ITE_INGRF)) != ITE_ACTIVE) {
		(void) iteon(dev, 3);
		paniced = 1;
	}
	iteputchar(c, dev);
}
#endif
