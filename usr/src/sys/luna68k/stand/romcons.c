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
 *	@(#)romcons.c	7.1 (Berkeley) %G%
 */

/* romcons.c   OCT-21-1991 */

#include <sys/types.h>
#include <luna68k/luna68k/cons.h>
#include <luna68k/stand/romvec.h>

romcnprobe(cp)
	struct consdev *cp;
{
	cp->cn_tp  = 0;
	cp->cn_dev = 0;
	cp->cn_pri = CN_NORMAL;
}

romcninit(cp)
	struct consdev *cp;
{
}

romcngetc(dev)
	dev_t dev;
{
	int c;

	for (;;)
		if ((c = ROM_getchar()) != -1)
			break;

	return(c);
}

romcnputc(dev, c)
	dev_t dev;
	int c;
{
	ROM_putchar(c);
}
