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
 * from: Utah $Hdr: grf_dv.c 1.11 92/01/21$
 *
 *	@(#)grf_dv.c	7.6 (Berkeley) %G%
 */

#include "grf.h"
#if NGRF > 0

/*
 * Graphics routines for the DaVinci, HP98730/98731 Graphics system.
 */
#include <sys/param.h>
#include <sys/errno.h>

#include <hp/dev/grfioctl.h>
#include <hp/dev/grfvar.h>
#include <hp300/dev/grf_dvreg.h>

#include <machine/cpu.h>

/*
 * Initialize hardware.
 * Must point g_display at a grfinfo structure describing the hardware.
 * Returns 0 if hardware not present, non-zero ow.
 */
dv_init(gp, addr)
	struct grf_softc *gp;
	caddr_t addr;
{
	register struct dvboxfb *dbp;
	struct grfinfo *gi = &gp->g_display;
	int fboff;
	extern caddr_t sctopa(), iomap();

	dbp = (struct dvboxfb *) addr;
	if (ISIIOVA(addr))
		gi->gd_regaddr = (caddr_t) IIOP(addr);
	else
		gi->gd_regaddr = sctopa(vatosc(addr));
	gi->gd_regsize = 0x20000;
	gi->gd_fbwidth = (dbp->fbwmsb << 8) | dbp->fbwlsb;
	gi->gd_fbheight = (dbp->fbhmsb << 8) | dbp->fbhlsb;
	gi->gd_fbsize = gi->gd_fbwidth * gi->gd_fbheight;
	fboff = (dbp->fbomsb << 8) | dbp->fbolsb;
	gi->gd_fbaddr = (caddr_t) (*((u_char *)addr + fboff) << 16);
	if (gi->gd_regaddr >= (caddr_t)DIOIIBASE) {
		/*
		 * For DIO II space the fbaddr just computed is the offset
		 * from the select code base (regaddr) of the framebuffer.
		 * Hence it is also implicitly the size of the register set.
		 */
		gi->gd_regsize = (int) gi->gd_fbaddr;
		gi->gd_fbaddr += (int) gi->gd_regaddr;
		gp->g_regkva = addr;
		gp->g_fbkva = addr + gi->gd_regsize;
	} else {
		/*
		 * For DIO space we need to map the seperate framebuffer.
		 */
		gp->g_regkva = addr;
		gp->g_fbkva = iomap(gi->gd_fbaddr, gi->gd_fbsize);
	}
	gi->gd_dwidth = (dbp->dwmsb << 8) | dbp->dwlsb;
	gi->gd_dheight = (dbp->dwmsb << 8) | dbp->dwlsb;
	gi->gd_planes = 0;	/* ?? */
	gi->gd_colors = 256;

	dv_reset(dbp);
	return(1);
}

/*
 *  Magic code herein.
 */
dv_reset(dbp)
	register struct dvboxfb *dbp;
{
  	dbp->reset = 0x80;
	DELAY(100);

	dbp->interrupt = 0x04;
	dbp->en_scan   = 0x01;
	dbp->fbwen     = ~0;
	dbp->opwen     = ~0;
	dbp->fold      = 0x01;
	dbp->drive     = 0x01;
	dbp->rep_rule  = 0x33;
	dbp->alt_rr    = 0x33;
	dbp->zrr       = 0x33;

	dbp->fbvenp    = 0xFF;
	dbp->dispen    = 0x01;
	dbp->fbvens    = 0x0;
	dbp->fv_trig   = 0x01;
	DELAY(100);
	dbp->vdrive    = 0x0;
	dbp->zconfig   = 0x0;

	while (dbp->wbusy & 0x01)
	  DELAY(100);

	dbp->cmapbank = 0;

	dbp->red0   = 0;
	dbp->red1   = 0;
	dbp->green0 = 0;
	dbp->green1 = 0;
	dbp->blue0  = 0;
	dbp->blue1  = 0;

	dbp->panxh   = 0;
	dbp->panxl   = 0;
	dbp->panyh   = 0;
	dbp->panyl   = 0;
	dbp->zoom    = 0;
	dbp->cdwidth = 0x50;
	dbp->chstart = 0x52;
	dbp->cvwidth = 0x22;
	dbp->pz_trig = 1;
}

/*
 * Change the mode of the display.
 * Right now all we can do is grfon/grfoff.
 * Return a UNIX error number or 0 for success.
 */
dv_mode(gp, cmd)
	register struct grf_softc *gp;
{
	register struct dvboxfb *dbp;
	int error = 0;

	dbp = (struct dvboxfb *) gp->g_regkva;
	switch (cmd) {
	case GM_GRFON:
	  	dbp->dispen = 0x01;
	  	break;
	case GM_GRFOFF:
		break;
	case GM_GRFOVON:
		dbp->opwen = 0xF;
		dbp->drive = 0x10;
		break;
	case GM_GRFOVOFF:
		dbp->opwen = 0;
		dbp->drive = 0x01;
		break;
	default:
		error = EINVAL;
		break;
	}
	return(error);
}

#endif
