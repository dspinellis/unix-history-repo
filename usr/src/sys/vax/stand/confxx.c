/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)confxx.c	7.4 (Berkeley) %G%
 */

int	xxstrategy(), xxopen(), xxioctl();

struct devsw devsw[] = {
	{ "XX",	xxstrategy,	xxopen,		nullsys,	noioctl },
};

int	ndevs = (sizeof(devsw) / sizeof(devsw[0]));
