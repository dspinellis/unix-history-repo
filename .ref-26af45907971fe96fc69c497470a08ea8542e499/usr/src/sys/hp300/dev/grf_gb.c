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
 * from: Utah $Hdr: grf_gb.c 1.17 92/01/21$
 *
 *	@(#)grf_gb.c	7.6 (Berkeley) %G%
 */

#include "grf.h"
#if NGRF > 0

/*
 * Graphics routines for the Gatorbox.
 *
 * Note: In the context of this system, "gator" and "gatorbox" both refer to
 *       HP 987x0 graphics systems.  "Gator" is not used for high res mono.
 *       (as in 9837 Gator systems)
 */
#include <sys/param.h>
#include <sys/errno.h>

#include <hp/dev/grfioctl.h>
#include <hp/dev/grfvar.h>

#include <hp300/dev/grf_gbreg.h>
#include <machine/cpu.h>

#define CRTC_DATA_LENGTH  0x0e
u_char crtc_init_data[CRTC_DATA_LENGTH] = {
    0x29, 0x20, 0x23, 0x04, 0x30, 0x0b, 0x30,
    0x30, 0x00, 0x0f, 0x00, 0x00, 0x00, 0x00
};

/*
 * Initialize hardware.
 * Must point g_display at a grfinfo structure describing the hardware.
 * Returns 0 if hardware not present, non-zero ow.
 */
gb_init(gp, addr)
	struct grf_softc *gp;
	caddr_t addr;
{
	register struct gboxfb *gbp;
	struct grfinfo *gi = &gp->g_display;
	u_char *fbp, save;
	int fboff;
	extern caddr_t sctopa(), iomap();

	gbp = (struct gboxfb *) addr;
	if (ISIIOVA(addr))
		gi->gd_regaddr = (caddr_t) IIOP(addr);
	else
		gi->gd_regaddr = sctopa(vatosc(addr));
	gi->gd_regsize = 0x10000;
	gi->gd_fbwidth = 1024;		/* XXX */
	gi->gd_fbheight = 1024;		/* XXX */
	gi->gd_fbsize = gi->gd_fbwidth * gi->gd_fbheight;
	fboff = (gbp->fbomsb << 8) | gbp->fbolsb;
	gi->gd_fbaddr = (caddr_t) (*((u_char *)addr + fboff) << 16);
	gp->g_regkva = addr;
	gp->g_fbkva = iomap(gi->gd_fbaddr, gi->gd_fbsize);
	gi->gd_dwidth = 1024;		/* XXX */
	gi->gd_dheight = 768;		/* XXX */
	gi->gd_planes = 0;		/* how do we do this? */
	/*
	 * The minimal register info here is from the Gatorbox X driver.
	 */
	fbp = (u_char *) gp->g_fbkva;
	gbp->write_protect = 0;
	gbp->interrupt = 4;		/** fb_enable ? **/
	gbp->rep_rule = 3;		/* GXcopy */
	gbp->blink1 = 0xff;
	gbp->blink2 = 0xff;

	gb_microcode(gbp);

	/*
	 * Find out how many colors are available by determining
	 * which planes are installed.  That is, write all ones to
	 * a frame buffer location, see how many ones are read back.
	 */
	save = *fbp;
	*fbp = 0xFF;
	gi->gd_colors = *fbp + 1;
	*fbp = save;
	return(1);
}

/*
 * Program the 6845.
 */
gb_microcode(gbp)
	register struct gboxfb *gbp;
{
	register int i;
	
	for (i = 0; i < CRTC_DATA_LENGTH; i++) {
		gbp->crtc_address = i;
		gbp->crtc_data = crtc_init_data[i];
	}
}

/*
 * Change the mode of the display.
 * Right now all we can do is grfon/grfoff.
 * Return a UNIX error number or 0 for success.
 */
gb_mode(gp, cmd)
	register struct grf_softc *gp;
{
	struct gboxfb *gbp;
	int error = 0;

	gbp = (struct gboxfb *) gp->g_regkva;
	switch (cmd) {
	case GM_GRFON:
		gbp->sec_interrupt = 1;
		break;
	case GM_GRFOFF:
		break;
	default:
		error = EINVAL;
		break;
	}
	return(error);
}

#endif
