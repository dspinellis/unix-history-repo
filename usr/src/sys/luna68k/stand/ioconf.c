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
 *	@(#)ioconf.c	7.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/buf.h>
#include <sys/map.h>
#include <luna68k/stand/device.h>


#define C (caddr_t)
#define D (struct driver *)

extern struct driver scdriver;
extern struct driver sddriver;
extern struct driver stdriver;

struct hp_ctlr hp_cinit[] = {
/*	driver,		unit,	alive,	addr,	flags */
	{ &scdriver,	0,	0,	C 0x0,	0x0 },
	0
};

struct hp_device hp_dinit[] = {
/*driver,	cdriver,	unit,	ctlr,	slave,	addr,	dk,	flags*/
{ &sddriver,	&scdriver,	0,	0,	6,	C 0x0,	1,	0x0 },
{ &sddriver,	&scdriver,	1,	0,	5,	C 0x0,	1,	0x0 },
{ &stdriver,	&scdriver,	0,	0,	4,	C 0x0,	0,	0x0 },
0
};
