/*
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)getdev.h	5.1 (Berkeley) %G%
 */

void	getdev __P((u_long alldevs, int (*match)(struct device *),
		    void (*add)(u_long, struct device *)));
