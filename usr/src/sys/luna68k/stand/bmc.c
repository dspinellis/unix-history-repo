/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)bmc.c	7.1 (Berkeley) %G%
 */

/*
 * bmc.c -- bitmap console driver
 *   by A.Fujita, JUL-06-1992
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <luna68k/stand/rcvbuf.h>
#include <luna68k/stand/preset.h>

extern	int dipsw1;
extern	struct rcvbuf	rcvbuf[];

bmcintr()
{
}

/*
 * Following are all routines needed for SIO to act as console
 */
#include <luna68k/luna68k/cons.h>

bmccnprobe(cp)
	struct consdev *cp;
{
	if ((dipsw1 & PS_BMC_CONS) == 0) {
		cp->cn_pri = CN_DEAD;
		return;
	}

	/* initialize required fields */
	cp->cn_dev = 1;
	cp->cn_tp  = 0;
	cp->cn_pri = CN_NORMAL;
}

bmccninit(cp)
	struct consdev *cp;
{
	sioinit();
	bmdinit();
}

bmccngetc(dev)
	dev_t dev;
{
	register int c;
	register int unit = 1;

	while (RBUF_EMPTY(unit)) {
		DELAY(10);
	}

	POP_RBUF(unit, c);

	return(c);
/*
	return(siocngetc(dev));
 */
}

bmccnputc(dev, c)
	dev_t dev;
	int c;
{
	bmdputc(c);
}
