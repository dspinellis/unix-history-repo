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
 * from: Utah $Hdr: grf_tc.c 1.13 89/08/25$
 *
 *	@(#)grf_tc.c	7.1 (Berkeley) 5/8/90
 */

#include "grf.h"
#if NGRF > 0

/*
 * Graphics routines for TOPCAT frame buffer
 */
#include "param.h"
#include "errno.h"

#include "grfioctl.h"
#include "grfvar.h"
#include "grf_tcreg.h"

#include "machine/cpu.h"

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

#if defined(HP360) || defined(HP370)
	extern char grfregs[];
	if (addr == (u_char *)grfregs)
		gi->gd_regaddr = (caddr_t) DIOIIBASE;
	else
#endif
	gi->gd_regaddr = (caddr_t) UNIOV(addr);
	gi->gd_regsize = 0x10000;
	gi->gd_fbwidth = (tp->fbwmsb << 8) | tp->fbwlsb;
	gi->gd_fbheight = (tp->fbhmsb << 8) | tp->fbhlsb;
	fboff = (tp->fbomsb << 8) | tp->fbolsb;
	gi->gd_fbaddr = (caddr_t) (*(addr + fboff) << 16);
#if defined(HP360) || defined(HP370)
	/*
	 * For DIO II space addresses offset is relative to the DIO II space.
	 * XXX: this should apply to all frame buffer types.
	 */
	if (gi->gd_regaddr >= (caddr_t)DIOIIBASE)
		gi->gd_fbaddr += (int) gi->gd_regaddr;
#endif
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
