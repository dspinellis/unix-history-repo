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
 *	@(#)fb.c	7.1 (Berkeley) %G%
 */

/*
 * fb.c -- frame-buffer device driver
 *	by A.Fujita, Dec-16-1992
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/ioctl.h>
#include <luna68k/dev/fbio.h>

volatile struct fb_rfc *rfcPtr = (struct fb_rfc *) 0xB1000000;
static   struct fb_rfc  rfcVal;

int
fbopen(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{
	return(0);
}

int
fbclose(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{
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

	case FBIOPUTRFCT:

		*rfcPtr = rfcVal = *((struct fb_rfc *) data);
		break;

	case FBIOGETRFCT:
                *(struct fb_rfc *)data = rfcVal;
		break;

/*
	case GRFIOCON:
		error = grfon(dev);
		break;

	case GRFIOCOFF:
		error = grfoff(dev);
		break;

	case GRFIOCMAP:
		error = grfmmap(dev, (caddr_t *)data, p);
		break;

	case GRFIOCUNMAP:
		error = grfunmmap(dev, *(caddr_t *)data, p);
		break;
 */
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
