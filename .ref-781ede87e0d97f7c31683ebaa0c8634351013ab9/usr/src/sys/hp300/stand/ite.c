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
 * from: Utah $Hdr: ite.c 1.19 89/08/22$
 *
 *	@(#)ite.c	7.2 (Berkeley) %G%
 */

/*
 * Standalone Internal Terminal Emulator (CRT and keyboard)
 */
#include "samachdep.h"

#ifdef ITECONSOLE

#include "sys/param.h"
#include "../hp300/cons.h"
#include "../dev/device.h"
#include "../dev/itevar.h"
#include "../dev/grfvar.h"

int nodev();

int topcat_init(), topcat_putc();
int topcat_clear(), topcat_cursor(), topcat_scroll();
int gatorbox_init(), gatorbox_clear();
int gatorbox_putc(), gatorbox_cursor(), gatorbox_scroll();
int rbox_init(), rbox_clear();
int rbox_putc(), rbox_cursor(), rbox_scroll();
int dvbox_init(), dvbox_clear();
int dvbox_putc(), dvbox_cursor(), dvbox_scroll();

struct itesw itesw[] = {
	topcat_init,		nodev,			topcat_clear,
	topcat_putc,		topcat_cursor,		topcat_scroll,

	gatorbox_init,		nodev,			gatorbox_clear,
	gatorbox_putc,		gatorbox_cursor,	gatorbox_scroll,

	rbox_init,		nodev,			rbox_clear,
	rbox_putc,		rbox_cursor,		rbox_scroll,

      	dvbox_init,		nodev,			dvbox_clear,
	dvbox_putc,		dvbox_cursor,		dvbox_scroll,
};

/* these guys need to be in initialized data */
int itecons = -1;
struct  ite_softc ite_softc[NITE] = { 0 };

/*
 * Locate all bitmapped displays
 */
iteconfig()
{
	extern struct hp_hw sc_table[];
	int dtype, fboff, i;
	struct hp_hw *hw;
	struct grfreg *gr;
	struct ite_softc *ip;

	i = 0;
	for (hw = sc_table; hw < &sc_table[MAX_CTLR]; hw++) {
		if (hw->hw_type != BITMAP)
			continue;
		gr = (struct grfreg *) hw->hw_addr;
		/* XXX: redundent but safe */
		if (badaddr((caddr_t)gr) || gr->gr_id != GRFHWID)
			continue;
		switch (gr->gr_id2) {
		case GID_GATORBOX:
			dtype = ITE_GATORBOX;
			break;
		case GID_TOPCAT:
		case GID_LRCATSEYE:
		case GID_HRCCATSEYE:
		case GID_HRMCATSEYE:
			dtype = ITE_TOPCAT;
			break;
		case GID_RENAISSANCE:
			dtype = ITE_RENAISSANCE;
			break;
		case GID_DAVINCI:
			dtype = ITE_DAVINCI;
			break;
		default:
			continue;
		}
		if (i >= NITE)
			break;
		ip = &ite_softc[i];
		ip->regbase = (caddr_t) gr;
		fboff = (gr->gr_fbomsb << 8) | gr->gr_fbolsb;
		ip->fbbase = (caddr_t) (*((u_char *)ip->regbase+fboff) << 16);
		/* DIO II: FB offset is relative to select code space */
		if (ip->regbase >= (caddr_t)0x1000000)
			ip->fbbase += (int)ip->regbase;
		ip->flags = ITE_ALIVE|ITE_CONSOLE;
		ip->type = dtype;
		i++;
	}
}

#ifdef CONSDEBUG
/*
 * Allows us to cycle through all possible consoles (NITE ites and serial port)
 * by using SHIFT-RESET on the keyboard.
 */
int	whichconsole = -1;
#endif

iteprobe(cp)
	struct consdev *cp;
{
	register int ite;
	register struct ite_softc *ip;
	int unit, pri;

#ifdef CONSDEBUG
	whichconsole = ++whichconsole % (NITE+1);
#endif

	if (itecons != -1)
		return(1);

	iteconfig();
	unit = -1;
	pri = CN_DEAD;
	for (ite = 0; ite < NITE; ite++) {
#ifdef CONSDEBUG
		if (ite < whichconsole)
			continue;
#endif
		ip = &ite_softc[ite];
		if ((ip->flags & (ITE_ALIVE|ITE_CONSOLE))
		    != (ITE_ALIVE|ITE_CONSOLE))
			continue;
		if ((int)ip->regbase == GRFIADDR) {
			pri = CN_INTERNAL;
			unit = ite;
		} else if (unit < 0) {
			pri = CN_NORMAL;
			unit = ite;
		}
	}
	cp->cn_dev = unit;
	cp->cn_pri = pri;
}

iteinit(cp)
	struct consdev *cp;
{
	int ite = cp->cn_dev;
	struct ite_softc *ip;

	if (itecons != -1)
		return(1);

	ip = &ite_softc[ite];

	ip->curx = 0;
	ip->cury = 0;
	ip->cursorx = 0;
	ip->cursory = 0;

	(*itesw[ip->type].ite_init)(ip);
	(*itesw[ip->type].ite_cursor)(ip, DRAW_CURSOR);

	itecons = ite;
	kbdinit();
}

iteputchar(c)
	register int c;
{
	register struct ite_softc *ip = &ite_softc[itecons];
	register struct itesw *sp = &itesw[ip->type];

	c &= 0x7F;
	switch (c) {

	case '\n':
		if (++ip->cury == ip->rows) {
			ip->cury--;
			(*sp->ite_scroll)(ip, 1, 0, 1, SCROLL_UP);
			ite_clrtoeol(ip, sp, ip->cury, 0);
		}
		else
			(*sp->ite_cursor)(ip, MOVE_CURSOR);
		break;

	case '\r':
		ip->curx = 0;
		(*sp->ite_cursor)(ip, MOVE_CURSOR);
		break;

	case '\b':
		if (--ip->curx < 0)
			ip->curx = 0;
		else
			(*sp->ite_cursor)(ip, MOVE_CURSOR);
		break;

	default:
		if (c < ' ' || c == 0177)
			break;
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
		if (++ip->cury == ip->rows) {
			--ip->cury;
			(*sp->ite_scroll)(ip, 1, 0, 1, SCROLL_UP);
			ite_clrtoeol(ip, sp, ip->cury, 0);
			return;
		}
	}
	(*sp->ite_cursor)(ip, MOVE_CURSOR);
}

ite_clrtoeol(ip, sp, y, x)
     register struct ite_softc *ip;
     register struct itesw *sp;
     register int y, x;
{
	(*sp->ite_clear)(ip, y, x, 1, ip->cols - x);
	(*sp->ite_cursor)(ip, DRAW_CURSOR);
}

itegetchar()
{
#ifdef SMALL
	return (0);
#else
	return (kbdgetc());
#endif
}
#endif
