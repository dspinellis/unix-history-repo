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
 * from: Utah $Hdr: grf_rb.c 1.10 89/04/11$
 *
 *	@(#)grf_rb.c	7.1 (Berkeley) %G%
 */

#include "grf.h"
#if NGRF > 0

/*
 * Graphics routines for the Renaissance, HP98720 Graphics system.
 */
#include "param.h"
#include "errno.h"

#include "grfioctl.h"
#include "grfvar.h"
#include "grf_rbreg.h"

#include "machine/cpu.h"

#ifdef notyet
static short rb_microcode[] = {
	0x5efe, 0x8a38, 0x0000, 0x0000, 0x0f00, 0, 0, 0,
	0x3efe, 0x8a38, 0x0000, 0x7003, 0xf706, 0, 0, 0,
	0x1efe, 0x8a38, 0x0000, 0x0000, 0x0000, 0, 0, 0,
	0x3efe, 0x8a38, 0x0000, 0x7003, 0xfc06, 0, 0, 0,
	0x1efe, 0x8a38, 0x0000, 0x0000, 0x0000, 0, 0, 0,
	0x3efe, 0x8a38, 0x0004, 0x40f7, 0xa006, 0, 0, 0,
	0x9efe, 0x8a38, 0x0000, 0x0000, 0x0000, 0, 0, 0,
	0x1efe, 0x8a38, 0x0000, 0x0000, 0x0000
};
#endif

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

#ifdef notyet
/*
 * Download the microcode to the rbox.
 */
rb_download_microcode(regbase)
     caddr_t regbase;
{
	register short *rb_microcode_ptr = rb_microcode;
	register short *rb_cntlstore_ptr = (short *)((char *)regbase + 0xC000);
	register int i;

	/*
	 * Reset and halt transform engine
	 */
	*((char *)regbase + 0x8002) = 0x00a0;
	*((char *)regbase + 0x8002) = 0x0020;
	*((char *)regbase + 0x8002) = 0x0080;

	/*
	 * Select the first bank of control store.
	 */
	*((char *)regbase + 0x8007) = 0x0;

	/*
	 * Write the microcode into the control store address space.
	 */
	for (i = 0; i < sizeof(rb_microcode) / sizeof(rb_microcode[0]); i++)
		*rb_cntlstore_ptr++ = *rb_microcode_ptr++;

	/*
	 * Start microcode execution.
	 */
	*(short *)((char *)regbase + 0x8002) = 0x2000;
	*(short *)((char *)regbase + 0x8002) = 0x0;
	
	/*
	 * wait for renaissance to finish up.
	 */
	for (i = 0; i < 1000; i++) {
		if (*((char *)regbase + 0x8012) < 0)
			continue;
	}
}
#endif

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
