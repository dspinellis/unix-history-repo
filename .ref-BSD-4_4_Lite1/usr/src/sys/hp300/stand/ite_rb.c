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
 * from: Utah $Hdr: ite_rb.c 1.6 92/01/20$
 *
 *	@(#)ite_rb.c	8.1 (Berkeley) 6/10/93
 */

#include <hp300/stand/samachdep.h>

#ifdef ITECONSOLE

#include <sys/param.h>
#include <hp/dev/itevar.h>
#include <hp/dev/itereg.h>

#include <hp300/dev/grf_rbreg.h>

#define REGBASE		((struct rboxfb *)(ip->regbase))
#define WINDOWMOVER	rbox_windowmove

rbox_init(ip)
	struct ite_softc *ip;
{
	int i;
	
	rb_waitbusy(ip->regbase);
	DELAY(3000);

	REGBASE->interrupt = 0x04;
	REGBASE->display_enable = 0x01;
	REGBASE->video_enable = 0x01;
	REGBASE->drive = 0x01;
	REGBASE->vdrive = 0x0;

	ite_fontinfo(ip);
	
	REGBASE->opwen = 0xFF;

	/*
	 * Clear the framebuffer.
	 */
	rbox_windowmove(ip, 0, 0, 0, 0, ip->fbheight, ip->fbwidth, RR_CLEAR);
	rb_waitbusy(ip->regbase);
	
	for(i = 0; i < 16; i++) {
		*(ip->regbase + 0x63c3 + i*4) = 0x0;
		*(ip->regbase + 0x6403 + i*4) = 0x0;
		*(ip->regbase + 0x6803 + i*4) = 0x0;
		*(ip->regbase + 0x6c03 + i*4) = 0x0;
		*(ip->regbase + 0x73c3 + i*4) = 0x0;
		*(ip->regbase + 0x7403 + i*4) = 0x0;
		*(ip->regbase + 0x7803 + i*4) = 0x0;
		*(ip->regbase + 0x7c03 + i*4) = 0x0;
	}

	REGBASE->rep_rule = 0x33;
	
	/*
	 * I cannot figure out how to make the blink planes stop. So, we
	 * must set both colormaps so that when the planes blink, and
	 * the secondary colormap is active, we still get text.
	 */
	CM1RED[0x00].value = 0x00;
	CM1GRN[0x00].value = 0x00;
	CM1BLU[0x00].value = 0x00;
	CM1RED[0x01].value = 0xFF;
	CM1GRN[0x01].value = 0xFF;
	CM1BLU[0x01].value = 0xFF;

	CM2RED[0x00].value = 0x00;
	CM2GRN[0x00].value = 0x00;
	CM2BLU[0x00].value = 0x00;
	CM2RED[0x01].value = 0xFF;
	CM2GRN[0x01].value = 0xFF;
	CM2BLU[0x01].value = 0xFF;

 	REGBASE->blink = 0x00;
	REGBASE->write_enable = 0x01;
	REGBASE->opwen = 0x00;
	
	ite_fontinit(ip);

	/*
	 * Stash the inverted cursor.
	 */
	rbox_windowmove(ip, charY(ip, ' '), charX(ip, ' '),
			    ip->cblanky, ip->cblankx, ip->ftheight,
			    ip->ftwidth, RR_COPYINVERTED);
}

rbox_putc(ip, c, dy, dx, mode)
	register struct ite_softc *ip;
        register int dy, dx;
	int c, mode;
{
	rbox_windowmove(ip, charY(ip, c), charX(ip, c),
			dy * ip->ftheight, dx * ip->ftwidth,
			ip->ftheight, ip->ftwidth, RR_COPY);
}

rbox_cursor(ip, flag)
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

rbox_clear(ip, sy, sx, h, w)
	struct ite_softc *ip;
	register int sy, sx, h, w;
{
	rbox_windowmove(ip, sy * ip->ftheight, sx * ip->ftwidth,
			sy * ip->ftheight, sx * ip->ftwidth, 
			h  * ip->ftheight, w  * ip->ftwidth,
			RR_CLEAR);
}

rbox_scroll(ip, sy, sx, count, dir)
        register struct ite_softc *ip;
        register int sy, count;
        int dir, sx;
{
	register int dy = sy - count;
	register int height = ip->rows - sy;

	rbox_cursor(ip, ERASE_CURSOR);

	rbox_windowmove(ip, sy * ip->ftheight, sx * ip->ftwidth,
			dy * ip->ftheight, sx * ip->ftwidth,
			height * ip->ftheight,
			ip->cols * ip->ftwidth, RR_COPY);
}

rbox_windowmove(ip, sy, sx, dy, dx, h, w, func)
	struct ite_softc *ip;
	int sy, sx, dy, dx, h, w, func;
{
	register struct rboxfb *rp = REGBASE;
	if (h == 0 || w == 0)
		return;
	
	rb_waitbusy(ip->regbase);
	rp->rep_rule = func << 4 | func;
	rp->source_y = sy;
	rp->source_x = sx;
	rp->dest_y = dy;
	rp->dest_x = dx;
	rp->wheight = h;
	rp->wwidth  = w;
	rp->wmove = 1;
}
#endif
