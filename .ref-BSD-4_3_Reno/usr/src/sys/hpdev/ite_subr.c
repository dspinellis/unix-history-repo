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
 * from: Utah $Hdr: ite_subr.c 1.3 89/04/11$
 *
 *	@(#)ite_subr.c	7.1 (Berkeley) 5/8/90
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

#include "machine/cpu.h"

ite_devinfo(ip)
	struct ite_softc *ip;
{
	struct fontinfo *fi;
	struct font *fd;

	fi = (struct fontinfo *) ((*FONTROM << 8 | *(FONTROM + 2)) + REGADDR);
	fd = (struct font *) ((fi->haddr << 8 | fi->laddr) + REGADDR);

	ip->ftheight = fd->fh;
	ip->ftwidth  = fd->fw;
	ip->fbwidth  = ITEREGS->fbwidth_h << 8 | ITEREGS->fbwidth_l;
	ip->fbheight = ITEREGS->fbheight_h << 8 | ITEREGS->fbheight_l;
	ip->dwidth   = ITEREGS->dispwidth_h << 8 | ITEREGS->dispwidth_l;
	ip->dheight  = ITEREGS->dispheight_h << 8 | ITEREGS->dispheight_l;
	ip->rows     = ip->dheight / ip->ftheight;
	ip->cols     = ip->dwidth / ip->ftwidth;

	if (ip->fbwidth > ip->dwidth) {
		/*
		 * Stuff goes to right of display.
		 */
		ip->fontx    = ip->dwidth;
		ip->fonty    = 0;
		ip->cpl      = (ip->fbwidth - ip->dwidth) / ip->ftwidth;
		ip->cblankx  = ip->dwidth;
		ip->cblanky  = ip->fonty + ((128 / ip->cpl) +1) * ip->ftheight;
	}
	else {
		/*
		 * Stuff goes below the display.
		 */
		ip->fontx   = 0;
		ip->fonty   = ip->dheight;
		ip->cpl     = ip->fbwidth / ip->ftwidth;
		ip->cblankx = 0;
		ip->cblanky = ip->fonty + ((128 / ip->cpl) + 1) * ip->ftheight;
	}
}

ite_fontinit(ip)
	register struct ite_softc *ip;
{
	struct fontinfo *fi;
	struct font *fd;
	register u_char *fbmem, *dp;
	register int bn;
	int c, l, b;

	fi = (struct fontinfo *) ((*FONTROM << 8 | *(FONTROM + 2)) + REGADDR);
	fd = (struct font *) ((fi->haddr << 8 | fi->laddr) + REGADDR);

	dp = fd->data;

	for (c = 0; c < 128; c++) {
		fbmem = (u_char *) FBBASE +
			(ip->fonty + (c / ip->cpl) * ip->ftheight) *
			ip->fbwidth;
		fbmem += ip->fontx + (c % ip->cpl) * ip->ftwidth;
		for (l = 0; l < ip->ftheight; l++) {
			bn = 7;
			for (b = 0; b < ip->ftwidth; b++) {
				if ((1 << bn) & *dp)
					*fbmem++ = 1;
				else
					*fbmem++ = 0;
				if (--bn < 0) {
					bn = 7;
					dp += 2;
				}
			}
			if (bn < 7)
				dp += 2;
			fbmem -= ip->ftwidth;
			fbmem += ip->fbwidth;
		}
	}

}
#endif
