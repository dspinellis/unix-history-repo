/*-
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1990,1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 * from: hp/dev/grf.c		7.13 (Berkeley) 7/12/92
 *
 *	@(#)fb.c	7.4 (Berkeley) %G%
 */

/*
 * fb.c -- frame-buffer device driver
 *	by A.Fujita, Dec-16-1992
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/ioctl.h>
#include <luna68k/dev/fbio.h>

extern	int hz;

int	fb_erase_screen();

volatile struct fb_rfc *rfcPtr = (struct fb_rfc *) 0xB1000000;
static   struct fb_rfc  rfcVal;

int
fbopen(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{
	fb_erase_screen();

	return(0);
}

int
fbclose(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{
	fb_adjust(7, -27);

	timeout(fb_erase_screen, (caddr_t)0, hz);

	return(0);
}

int
fbioctl(dev, cmd, data, flags, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flags;
	struct proc *p;
{
	struct fb_rfc *rfcp;
	int error;

	error = 0;
	switch (cmd) {

	case FBIOSETRFCT:
		*rfcPtr = rfcVal = *((struct fb_rfc *) data);
		break;

	case FBIOGETRFCT:
                *(struct fb_rfc *)data = rfcVal;
		break;

	default:
		error = EINVAL;
		break;

	}
	return(error);
}

fb_adjust(hcnt, vcnt)
	int hcnt, vcnt;
{
	rfcVal.rfc_hcnt = hcnt;			/* shift left   16 dot */
	rfcVal.rfc_vcnt = vcnt;			/* shift down    1 dot */

	*rfcPtr = rfcVal;
}

#define PL_WIDTH	64				/* Plane Width  (long) */

#define	SB_HIGHT	1024				/* Screen Hight  (Bit) */
#define SL_WIDTH	40				/* Screen Width (Long) */

#define SKIP_NEXT_LINE(addr)			( addr += (PL_WIDTH - SL_WIDTH) )

fb_erase_screen()
{
	volatile register u_long *lp = (u_long *) 0xB1080008;

	register int i, j;

	for (i = 0; i < SB_HIGHT; i++) {
		for (j = 0; j < SL_WIDTH; j++)
			*lp++ = 0;
		SKIP_NEXT_LINE(lp);
	}

	return;
}

int
fbselect(dev, rw)
	dev_t dev;
	int rw;
{
	return(0);
}

int
fbmap(dev, off, prot)
	dev_t dev;
	int off, prot;
{
	return(((u_int) 0xB10C0000 + off) >> PGSHIFT);
}
