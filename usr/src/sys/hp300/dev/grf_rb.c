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
 * from: Utah $Hdr: grf_rb.c 1.1 90/07/09$
 *
 *	@(#)grf_rb.c	7.4 (Berkeley) %G%
 */

#include "grf.h"
#if NGRF > 0

/*
 * Graphics routines for the Renaissance, HP98720 Graphics system.
 */
#include "sys/param.h"
#include "sys/errno.h"

#include "grfioctl.h"
#include "grfvar.h"
#include "grf_rbreg.h"

#include "../include/cpu.h"

/*
 * Initialize hardware.
 * Must point g_display at a grfinfo structure describing the hardware.
 * Returns 0 if hardware not present, non-zero ow.
 */
rb_init(gp, addr)
	struct grf_softc *gp;
	u_char *addr;
{
	register struct rboxfb *rbp;
	struct grfinfo *gi = &gp->g_display;
	int fboff;

	rbp = (struct rboxfb *) addr;
	gi->gd_regaddr = (caddr_t) UNIOV(addr);
	gi->gd_regsize = 0x20000;
	gi->gd_fbwidth = (rbp->fbwmsb << 8) | rbp->fbwlsb;
	gi->gd_fbheight = (rbp->fbhmsb << 8) | rbp->fbhlsb;
	fboff = (rbp->fbomsb << 8) | rbp->fbolsb;
	gi->gd_fbaddr = (caddr_t) (*(addr + fboff) << 16);
	gi->gd_fbsize = gi->gd_fbwidth * gi->gd_fbheight;
	gi->gd_dwidth = (rbp->dwmsb << 8) | rbp->dwlsb;
	gi->gd_dheight = (rbp->dwmsb << 8) | rbp->dwlsb;
	gi->gd_planes = 0;	/* ?? */
	gi->gd_colors = 256;
	return(1);
}

/*
 * Change the mode of the display.
 * Right now all we can do is grfon/grfoff.
 * Return a UNIX error number or 0 for success.
 */
rb_mode(gp, cmd)
	register struct grf_softc *gp;
{
	register struct rboxfb *rbp;
	int error = 0;

	rbp = (struct rboxfb *) IOV(gp->g_display.gd_regaddr);
	switch (cmd) {
	/*
	 * The minimal register info here is from the Renaissance X driver.
	 */
	case GM_GRFON:
	case GM_GRFOFF:
		break;
	case GM_GRFOVON:
		rbp->write_enable = 0;
		rbp->opwen = 0xF;
		rbp->drive = 0x10;
		break;
	case GM_GRFOVOFF:
		rbp->opwen = 0;
		rbp->write_enable = 0xffffffff;
		rbp->drive = 0x01;
		break;
	default:
		error = EINVAL;
		break;
	}
	return(error);
}

#endif
