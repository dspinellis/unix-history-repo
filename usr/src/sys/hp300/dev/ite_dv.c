/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
 * from: Utah $Hdr: ite_dv.c 1.10 93/06/25$
 *
 *	@(#)ite_dv.c	8.1 (Berkeley) 7/8/93
 */

#include "ite.h"
#if NITE > 0

#include <sys/param.h>
#include <sys/conf.h>
#include <sys/proc.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/systm.h>

#include <hp/dev/itevar.h>
#include <hp/dev/itereg.h>
#include <hp300/dev/grf_dvreg.h>

#include <machine/cpu.h>

/* XXX */
#include <hp/dev/grfioctl.h>
#include <hp/dev/grfvar.h>

#define REGBASE		((struct dvboxfb *)(ip->regbase))
#define WINDOWMOVER	dvbox_windowmove

dvbox_init(ip)
	register struct ite_softc *ip;
{
	int i;
	
	/* XXX */
	if (ip->regbase == 0) {
		struct grf_softc *gp = ip->grf;

		ip->regbase = gp->g_regkva;
		ip->fbbase = gp->g_fbkva;
		ip->fbwidth = gp->g_display.gd_fbwidth;
		ip->fbheight = gp->g_display.gd_fbheight;
		ip->dwidth = gp->g_display.gd_dwidth;
		ip->dheight = gp->g_display.gd_dheight;
		/*
		 * XXX some displays (e.g. the davinci) appear
		 * to return a display height greater than the
		 * returned FB height.  Guess we should go back
		 * to getting the display dimensions from the
		 * fontrom...
		 */
		if (ip->dwidth > ip->fbwidth)
			ip->dwidth = ip->fbwidth;
		if (ip->dheight > ip->fbheight)
			ip->dheight = ip->fbheight;
	}

	dv_reset(ip->regbase);

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
	
	db_waitbusy(ip->regbase);

	ite_fontinfo(ip);
	ite_fontinit(ip);

	/*
	 * Clear the (visible) framebuffer.
	 */
	dvbox_windowmove(ip, 0, 0, 0, 0, ip->dheight, ip->dwidth, RR_CLEAR);
	db_waitbusy(ip->regbase);

	/*
	 * Stash the inverted cursor.
	 */
	dvbox_windowmove(ip, charY(ip, ' '), charX(ip, ' '),
			 ip->cblanky, ip->cblankx, ip->ftheight,
			 ip->ftwidth, RR_COPYINVERTED);
}

dvbox_deinit(ip)
	register struct ite_softc *ip;
{
	dvbox_windowmove(ip, 0, 0, 0, 0, ip->fbheight, ip->fbwidth, RR_CLEAR);
	db_waitbusy(ip->regbase);

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
	
	db_waitbusy(ip->regbase);
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
