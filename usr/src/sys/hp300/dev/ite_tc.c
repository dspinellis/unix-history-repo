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
 * from: Utah $Hdr: ite_tc.c 1.27 92/12/20$
 *
 *	@(#)ite_tc.c	7.7 (Berkeley) %G%
 */

#include "ite.h"
#if NITE > 0

#include <sys/param.h>
#include <sys/conf.h>
#include <sys/proc.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/systm.h>

#include <hp300/dev/grf_tcreg.h>
#include <hp/dev/grfreg.h>
#include <hp/dev/itereg.h>
#include <hp/dev/itevar.h>

#include <machine/cpu.h>

/* XXX */
#include <hp/dev/grfioctl.h>
#include <hp/dev/grfvar.h>

#define REGBASE	    	((struct tcboxfb *)(ip->regbase))
#define WINDOWMOVER 	topcat_windowmove

topcat_init(ip)
	register struct ite_softc *ip;
{
	/* XXX */
	if (ip->regbase == NULL) {
		struct grf_softc *gp = ip->grf;

		ip->regbase = gp->g_regkva;
		ip->fbbase = gp->g_fbkva;
		ip->fbwidth = gp->g_display.gd_fbwidth;
		ip->fbheight = gp->g_display.gd_fbheight;
		ip->dwidth = gp->g_display.gd_dwidth;
		ip->dheight = gp->g_display.gd_dheight;
	}

	/*
	 * Catseye looks a lot like a topcat, but not completely.
	 * So, we set some bits to make it work.
	 */
	if (REGBASE->fbid != GID_TOPCAT) {
		while ((REGBASE->catseye_status & 1))
			;
		REGBASE->catseye_status = 0x0;
		REGBASE->vb_select      = 0x0;
		REGBASE->tcntrl         = 0x0;
		REGBASE->acntrl         = 0x0;
		REGBASE->pncntrl        = 0x0;
		REGBASE->rug_cmdstat    = 0x90;
	}

	/*
	 * Determine the number of planes by writing to the first frame
	 * buffer display location, then reading it back. 
	 */
	REGBASE->wen = ~0;
	REGBASE->fben = ~0;
	REGBASE->prr = RR_COPY;
	*FBBASE = 0xFF;
	ip->planemask = *FBBASE;

	/*
	 * Enable reading/writing of all the planes.
	 */
	REGBASE->fben = ip->planemask;
	REGBASE->wen  = ip->planemask;
	REGBASE->ren  = ip->planemask;
	REGBASE->prr  = RR_COPY;

	ite_fontinfo(ip);

	/*
	 * Clear the framebuffer on all planes.
	 */
	topcat_windowmove(ip, 0, 0, 0, 0, ip->fbheight, ip->fbwidth, RR_CLEAR);
	tc_waitbusy(ip->regbase, ip->planemask);

	ite_fontinit(ip);

	/*
	 * Initialize color map for color displays
	 */
	if (ip->planemask != 1) {
	  	tc_waitbusy(ip->regbase, ip->planemask);
		REGBASE->nblank = 0x01;

		tccm_waitbusy(ip->regbase);
		REGBASE->rdata  = 0x0;
		REGBASE->gdata  = 0x0;
		REGBASE->bdata  = 0x0;
		REGBASE->cindex = 0xFF;
		REGBASE->strobe = 0xFF;

		DELAY(100);
		tccm_waitbusy(ip->regbase);
		REGBASE->rdata  = 0x0;
		REGBASE->gdata  = 0x0;
		REGBASE->bdata  = 0x0;
		REGBASE->cindex = 0x0;

		DELAY(100);
		tccm_waitbusy(ip->regbase);
		REGBASE->rdata  = 0xFF;
		REGBASE->gdata  = 0xFF;
		REGBASE->bdata  = 0xFF;
		REGBASE->cindex = 0xFE;
		REGBASE->strobe = 0xFF;

		DELAY(100);
		tccm_waitbusy(ip->regbase);
		REGBASE->rdata  = 0x0;
		REGBASE->gdata  = 0x0;
		REGBASE->bdata  = 0x0;
		REGBASE->cindex = 0x0;
	}

	/*
	 * Stash the inverted cursor.
	 */
	topcat_windowmove(ip, charY(ip, ' '), charX(ip, ' '),
			  ip->cblanky, ip->cblankx, ip->ftheight,
			  ip->ftwidth, RR_COPYINVERTED);
}

topcat_deinit(ip)
	register struct ite_softc *ip;
{
	topcat_windowmove(ip, 0, 0, 0, 0, ip->fbheight, ip->fbwidth, RR_CLEAR);
	tc_waitbusy(ip->regbase, ip->planemask);

	REGBASE->nblank = ~0;
   	ip->flags &= ~ITE_INITED;
}

topcat_putc(ip, c, dy, dx, mode)
	register struct ite_softc *ip;
	int c, dy, dx, mode;
{
        int wmrr = ((mode == ATTR_INV) ? RR_COPYINVERTED : RR_COPY);
	
	topcat_windowmove(ip, charY(ip, c), charX(ip, c),
			  dy * ip->ftheight, dx * ip->ftwidth,
			  ip->ftheight, ip->ftwidth, wmrr);
}

topcat_cursor(ip, flag)
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

topcat_clear(ip, sy, sx, h, w)
	register struct ite_softc *ip;
	register int sy, sx, h, w;
{
	topcat_windowmove(ip, sy * ip->ftheight, sx * ip->ftwidth,
			  sy * ip->ftheight, sx * ip->ftwidth, 
			  h  * ip->ftheight, w  * ip->ftwidth,
			  RR_CLEAR);
}

topcat_scroll(ip, sy, sx, count, dir)
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

	topcat_windowmove(ip, sy * ip->ftheight, sx * ip->ftwidth,
			  dy * ip->ftheight, dx * ip->ftwidth,
			  height * ip->ftheight,
			  width  * ip->ftwidth, RR_COPY);
}

topcat_windowmove(ip, sy, sx, dy, dx, h, w, func)
	struct ite_softc *ip;
	int sy, sx, dy, dx, h, w, func;
{
  	register struct tcboxfb *rp = REGBASE;
	
	if (h == 0 || w == 0)
		return;
	tc_waitbusy(ip->regbase, ip->planemask);
	rp->wmrr     = func;
	rp->source_y = sy;
	rp->source_x = sx;
	rp->dest_y   = dy;
	rp->dest_x   = dx;
	rp->wheight  = h;
	rp->wwidth   = w;
	rp->wmove    = ip->planemask;
}
#endif
