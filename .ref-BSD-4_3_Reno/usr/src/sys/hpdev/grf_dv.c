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
 * from: Utah $Hdr: grf_dv.c 1.6 89/04/11$
 *
 *	@(#)grf_dv.c	7.1 (Berkeley) 5/8/90
 */

#include "grf.h"
#if NGRF > 0

/*
 * Graphics routines for the DaVinci, HP98730/98731 Graphics system.
 */
#include "param.h"
#include "errno.h"

#include "grfioctl.h"
#include "grfvar.h"
#include "grf_dvreg.h"

#include "machine/cpu.h"

/*
 * Initialize hardware.
 * Must point g_display at a grfinfo structure describing the hardware.
 * Returns 0 if hardware not present, non-zero ow.
 */
dv_init(gp, addr)
	struct grf_softc *gp;
	u_char *addr;
{
	register struct dvboxfb *dbp;
	struct grfinfo *gi = &gp->g_display;
	int fboff;

	dbp = (struct dvboxfb *) addr;
	gi->gd_regaddr = (caddr_t) UNIOV(addr);
	gi->gd_regsize = 0x20000;
	gi->gd_fbwidth = (dbp->fbwmsb << 8) | dbp->fbwlsb;
	gi->gd_fbheight = (dbp->fbhmsb << 8) | dbp->fbhlsb;
	fboff = (dbp->fbomsb << 8) | dbp->fbolsb;
	gi->gd_fbaddr = (caddr_t) (*(addr + fboff) << 16);
	gi->gd_fbsize = gi->gd_fbwidth * gi->gd_fbheight;
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

	dbp = (struct dvboxfb *) IOV(gp->g_display.gd_regaddr);
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
