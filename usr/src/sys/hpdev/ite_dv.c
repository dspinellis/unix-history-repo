/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * from: Utah $Hdr: ite_dv.c 1.5 89/04/11$
 *
 *	@(#)ite_dv.c	7.1 (Berkeley) 5/8/90
 */

#include "ite.h"
#if NITE > 0

#include "param.h"
#include "conf.h"
#include "user.h"
#include "proc.h"
#include "ioctl.h"
#include "tty.h"
#include "systm.h"
#include "uio.h"

#include "itevar.h"
#include "itereg.h"
#include "grf_dvreg.h"

#include "machine/cpu.h"

/* XXX */
#include "grfioctl.h"
#include "grfvar.h"

#define REGBASE		((struct dvboxfb *)(ip->regbase))
#define WINDOWMOVER	dvbox_windowmove

dvbox_init(ip)
	struct ite_softc *ip;
{
	int i;
	
	/* XXX */
	if (ip->regbase == 0) {
		struct grfinfo *gi = &grf_softc[ip - ite_softc].g_display;
		ip->regbase = IOV(gi->gd_regaddr);
		ip->fbbase = IOV(gi->gd_fbaddr);
	}

	dv_reset(REGADDR);

	/*
	 * Turn on frame buffer, turn on overlay planes, set replacement
	 * rule, enable top overlay plane writes for ite, disable all frame
	 * buffer planes, set byte per pixel, and display frame buffer 0.
	 * Lastly, turn on the box.
	 */
	REGBASE->interrupt = 0x04;
	REGBASE->drive     = 0x10;		
 	REGBASE->rep_rule  = RR_COPY << 4 | RR_COPY;
	REGBASE->opwen     = 0x01;
	REGBASE->fbwen     = 0x0;
	REGBASE->fold      = 0x01;
	REGBASE->vdrive    = 0x0;
	REGBASE->dispen    = 0x01;

	/*
	 * Video enable top overlay plane.
	 */
	REGBASE->opvenp = 0x01;
	REGBASE->opvens = 0x01;

	/*
	 * Make sure that overlay planes override frame buffer planes.
	 */
	REGBASE->ovly0p  = 0x0;
	REGBASE->ovly0s  = 0x0;
	REGBASE->ovly1p  = 0x0;
	REGBASE->ovly1s  = 0x0;
	REGBASE->fv_trig = 0x1;
	DELAY(100);

	/*
	 * Setup the overlay colormaps. Need to set the 0,1 (black/white)
	 * color for both banks.
	 */

	for (i = 0; i <= 1; i++) {
		REGBASE->cmapbank = i;
		REGBASE->rgb[0].red   = 0x00;
		REGBASE->rgb[0].green = 0x00;
		REGBASE->rgb[0].blue  = 0x00;
		REGBASE->rgb[1].red   = 0xFF;
		REGBASE->rgb[1].green = 0xFF;
		REGBASE->rgb[1].blue  = 0xFF;
	}
	REGBASE->cmapbank = 0;
	
	db_waitbusy(REGADDR);

	ite_devinfo(ip);
	ite_fontinit(ip);

	/*
	 * Clear the (visible) framebuffer.
	 */
	dvbox_windowmove(ip, 0, 0, 0, 0, ip->dheight, ip->dwidth, RR_CLEAR);
	db_waitbusy(REGADDR);

	/*
	 * Stash the inverted cursor.
	 */
	dvbox_windowmove(ip, charY(ip, ' '), charX(ip, ' '),
			 ip->cblanky, ip->cblankx, ip->ftheight,
			 ip->ftwidth, RR_COPYINVERTED);
}

dvbox_deinit(ip)
	struct ite_softc *ip;
{
	dvbox_windowmove(ip, 0, 0, 0, 0, ip->fbheight, ip->fbwidth, RR_CLEAR);
	db_waitbusy(REGADDR);

   	ip->flags &= ~ITE_INITED;
}

dvbox_putc(ip, c, dy, dx, mode)
	register struct ite_softc *ip;
        register int dy, dx;
	int c, mode;
{
        register int wrr = ((mode == ATTR_INV) ? RR_COPYINVERTED : RR_COPY);
	
	dvbox_windowmove(ip, charY(ip, c), charX(ip, c),
			 dy * ip->ftheight, dx * ip->ftwidth,
			 ip->ftheight, ip->ftwidth, wrr);
}

dvbox_cursor(ip, flag)
	register struct ite_softc *ip;
        register int flag;
{
	if (flag == DRAW_CURSOR)
		draw_cursor(ip)
	else if (flag == MOVE_CURSOR) {
		erase_cursor(ip)
		draw_cursor(ip)
	}
	else
		erase_cursor(ip)
}

dvbox_clear(ip, sy, sx, h, w)
	struct ite_softc *ip;
	register int sy, sx, h, w;
{
	dvbox_windowmove(ip, sy * ip->ftheight, sx * ip->ftwidth,
			 sy * ip->ftheight, sx * ip->ftwidth, 
			 h  * ip->ftheight, w  * ip->ftwidth,
			 RR_CLEAR);
}

dvbox_scroll(ip, sy, sx, count, dir)
        register struct ite_softc *ip;
        register int sy, count;
        int dir, sx;
{
	register int dy;
	register int dx = sx;
	register int height = 1;
	register int width = ip->cols;

	dvbox_cursor(ip, ERASE_CURSOR);

	if (dir == SCROLL_UP) {
		dy = sy - count;
		height = ip->rows - sy;
	}
	else if (dir == SCROLL_DOWN) {
		dy = sy + count;
		height = ip->rows - dy - 1;
	}
	else if (dir == SCROLL_RIGHT) {
		dy = sy;
		dx = sx + count;
		width = ip->cols - dx;
	}
	else {
		dy = sy;
		dx = sx - count;
		width = ip->cols - sx;
	}		

	dvbox_windowmove(ip, sy * ip->ftheight, sx * ip->ftwidth,
			 dy * ip->ftheight, dx * ip->ftwidth,
			 height * ip->ftheight,
			 width  * ip->ftwidth, RR_COPY);
}

dvbox_windowmove(ip, sy, sx, dy, dx, h, w, func)
	struct ite_softc *ip;
	int sy, sx, dy, dx, h, w, func;
{
	register struct dvboxfb *dp = REGBASE;
	if (h == 0 || w == 0)
		return;
	
	db_waitbusy(REGADDR);
	dp->rep_rule = func << 4 | func;
	dp->source_y = sy;
	dp->source_x = sx;
	dp->dest_y   = dy;
	dp->dest_x   = dx;
	dp->wheight  = h;
	dp->wwidth   = w;
	dp->wmove    = 1;
}
#endif
