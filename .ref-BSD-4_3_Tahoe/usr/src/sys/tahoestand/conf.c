/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)conf.c	1.7 (Berkeley) 6/29/88
 */

#include "param.h"
#include "inode.h"
#include "fs.h"
#include "saio.h"

extern int	nullsys(), nodev(), noioctl();

int	vdstrategy(), vdopen();
int	hdstrategy(), hdopen();
int	cystrategy(), cyopen(), cyclose();

struct devsw devsw[] = {
	{ "ud",	nodev,		nodev,	nullsys, noioctl },  /* 0 = ud */
	{ "dk",	vdstrategy,	vdopen,	nullsys, noioctl },  /* 1 = ht */
	{ "hd",	hdstrategy,	hdopen,	nullsys, noioctl },  /* 2 = hd */
#ifdef notdef
	{ "xp",	xpstrategy,	xpopen,	nullsys, noioctl },  /* 3 = xp */
#else
	{ "xp",	nodev,		nodev,	nullsys, noioctl },
#endif
	{ "cy",	cystrategy,	cyopen,	cyclose, noioctl },  /* 4 = cy */
	{ 0 }
};
int	ndevs = (sizeof(devsw)/sizeof(devsw[0]));
