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
 * from: Utah $Hdr: cons_conf.c 1.1 92/01/21
 *
 *	@(#)cons_conf.c	7.2 (Berkeley) %G%
 */

/*
 * This entire table could be autoconfig()ed but that would mean that
 * the kernel's idea of the console would be out of sync with that of
 * the standalone boot.  I think it best that they both use the same
 * known algorithm unless we see a pressing need otherwise.
 */
#include <sys/types.h>

#include <hp/dev/cons.h>

#include "ite.h"
#include "dca.h"
#include "dcm.h"

#if NITE > 0
extern int itecnprobe(), itecninit(), itecngetc(), itecnputc();
#endif
#if NDCA > 0
extern	int dcacnprobe(), dcacninit(), dcacngetc(), dcacnputc();
#endif
#if NDCM > 0
extern	int dcmcnprobe(), dcmcninit(), dcmcngetc(), dcmcnputc();
#endif

struct	consdev constab[] = {
#if NITE > 0
	{ itecnprobe,	itecninit,	itecngetc,	itecnputc },
#endif
#if NDCA > 0
	{ dcacnprobe,	dcacninit,	dcacngetc,	dcacnputc },
#endif
#if NDCM > 0
	{ dcmcnprobe,	dcmcninit,	dcmcngetc,	dcmcnputc },
#endif
	{ 0 },
};
