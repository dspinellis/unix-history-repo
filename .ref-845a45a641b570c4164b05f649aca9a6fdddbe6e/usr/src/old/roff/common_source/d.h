/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)d.h	4.3 (Berkeley) %G%
 */

struct d {filep op; int dnl,dimac,ditrap,ditf,alss,blss,nls,mkline,
		maxl,hnl,curd;} d[NDI], *dip;
