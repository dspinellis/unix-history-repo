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
 * from: Utah $Hdr: grf_tc.c 1.13 89/08/25$
 *
 *	@(#)grf_tc.c	7.3 (Berkeley) %G%
 */

#include "grf.h"
#if NGRF > 0

/*
 * Graphics routines for TOPCAT frame buffer
 */
#include "sys/param.h"
#include "sys/errno.h"

#include "grfioctl.h"
#include "grfvar.h"
#include "grf_tcreg.h"

#include "../include/cpu.h"

/*
 * Initialize hardware.
 * Must fill in the grfinfo structure in g_softc.
 * Returns 0 if hardware not present, non-zero ow.
 */
tc_init(gp, addr)
	struct grf_softc *gp;
	u_char *addr;
{
	register struct tcboxfb *tp = (struct tcboxfb *) addr;
	struct grfinfo *gi = &gp->g_display;
	volatile u_char *fbp;
	u_char save;
	int fboff;

	gi->gd_regaddr = (caddr_t) UNIOV(addr);
	gi->gd_regsize = 0x10000;
	gi->gd_fbwidth = (tp->fbwmsb << 8) | tp->fbwlsb;
	gi->gd_fbheight = (tp->fbhmsb << 8) | tp->fbhlsb;
	fboff = (tp->fbomsb << 8) | tp->fbolsb;
	gi->gd_fbaddr = (caddr_t) (*(addr + fboff) << 16);
	gi->gd_fbsize = gi->gd_fbwidth * gi->gd_fbheight;
	gi->gd_dwidth = (tp->dwmsb << 8) | tp->dwlsb;
	gi->gd_dheight = (tp->dhmsb << 8) | tp->dhlsb;
	gi->gd_planes = tp->num_planes;
	gi->gd_colors = 1 << gi->gd_planes;
	if (gi->gd_colors == 1) {
		fbp = (u_char *) IOV(gi->gd_fbaddr);
		tp->wen = ~0;
		tp->prr = 0x3;
		tp->fben = ~0;
		save = *fbp;
		*fbp = 0xFF;
		gi->gd_colors = *fbp + 1;
		*fbp = save;
	}
	return(1);
}

/*
 * Change the mode of the display.
 * Right now all we can do is grfon/grfoff.
 * Return a UNIX error number or 0 for success.
 * Function may not be needed anymore.
 */
/*ARGSUSED*/
tc_mode(gp, cmd)
	struct grf_softc *gp;
{
	int error = 0;

	switch (cmd) {
	case GM_GRFON:
	case GM_GRFOFF:
		break;
	default:
		error = EINVAL;
		break;
	}
	return(error);
}
#endif
