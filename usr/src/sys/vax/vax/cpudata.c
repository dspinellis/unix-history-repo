/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)cpudata.c	7.3 (Berkeley) %G%
 */

#include "pte.h"

#include "param.h"

#include "cpu.h"
#include "nexus.h"
#include "ioa.h"
#include "../vaxbi/bireg.h"
#include "../vaxuba/ubareg.h"

/*
 * Initialization of per-cpu data structures.
 */

/*
 * These are the (fixed) physical addresses of the
 * unibus memory for each of the possible unibus adapters.
 * Note that on some machines the unibus memory addresses
 * are actually indexed by the unibus adapter type code.
 */
#if VAX8600
caddr_t umaddr8600a[4] = {
	(caddr_t) UMEMA8600(0),	(caddr_t) UMEMA8600(1),
	(caddr_t) UMEMA8600(2),	(caddr_t) UMEMA8600(3),
};
caddr_t umaddr8600b[4] = {
	(caddr_t) UMEMB8600(0),	(caddr_t) UMEMB8600(1),
	(caddr_t) UMEMB8600(2),	(caddr_t) UMEMB8600(3),
};
#endif
#if VAX780
caddr_t	umaddr780[4] = {
	(caddr_t) UMEM780(0), (caddr_t) UMEM780(1),
	(caddr_t) UMEM780(2), (caddr_t) UMEM780(3)
};
#endif
#if VAX750
caddr_t	umaddr750[2] = {
	(caddr_t) UMEM750(0), (caddr_t) UMEM750(1),
};
#endif
#if VAX730
caddr_t	umaddr730[1] = {
	(caddr_t) UMEM730
};
#endif

/*
 * Information to patch around the stupidity of configuration
 * registers not returning types on some of the processors.
 */
#if VAX750
short	nexty750[NNEX750] = {
	NEX_MEM16,	NEX_MEM16,	NEX_MEM16,	NEX_MEM16,
	NEX_MBA,	NEX_MBA,	NEX_MBA,	NEX_MBA,
	NEX_UBA0,	NEX_UBA1,	NEX_ANY,	NEX_ANY,
	NEX_ANY,	NEX_ANY,	NEX_ANY,	NEX_ANY
};
#endif
#if VAX730
short	nexty730[NNEX730] = {
	NEX_MEM16,	NEX_ANY,	NEX_ANY,	NEX_ANY,
	NEX_ANY,	NEX_ANY,	NEX_ANY,	NEX_ANY,
};
#endif

#if VAX8800
/* I think this is how the thing is shaped: */
struct bibus bi8800[] = {
	{ BI_BASE(0) },
	{ BI_BASE(1) },
};
struct iobus io8800 = {
	{ IO_BIA, somewhere, somesize, (caddr_t)&bi8800[0] },
	{ IO_BIA, somewhere, somesize, (caddr_t)&bi8800[1] },
};
/* but you will have to make it work yourself (sorry) */
#endif

#if VAX8600
struct nexusconnect sbi8600[] = {
	{ NNEX8600, NEXA8600, DW780, NBDP8600, umaddr8600a, 0 },
	{ NNEX8600, NEXB8600, DW780, NBDP8600, umaddr8600b, 0 },
};
struct iobus io8600[] = {
	{ IO_ABUS, IOA8600(0), IOAMAPSIZ, (caddr_t)&sbi8600[0] },
	{ IO_ABUS, IOA8600(1), IOAMAPSIZ, (caddr_t)&sbi8600[1] },
};
#endif

#if VAX8200
/* BEWARE, this is all limited to one BI for now */
struct bibus bi8200[] = {
	{ BI_BASE(0) },
};
struct iobus io8200[] = {
	{ IO_BI, 0, 0, (caddr_t)&bi8200[0] },
};
#endif

#if VAX780
struct nexusconnect sbi780 = {
	NNEX780, NEX780, DW780, NBDP780, umaddr780, 0,
};
struct iobus io780[] = { IO_SBI780, 0, 0, (caddr_t)&sbi780 };
#endif

#if VAX750
struct nexusconnect cmi750 = {
	NNEX750, NEX750, DW750, NBDP750, umaddr750, nexty750,
};
struct iobus io750[] = { IO_CMI750, 0, 0, (caddr_t)&cmi750 };
#endif

#if VAX730
struct nexusconnect xxx730 = {
	NNEX730, NEX730, DW730, NBDP730, umaddr730, nexty730,
};
struct iobus io730[] = { IO_XXX730, 0, 0, (caddr_t)&xxx730 };
#endif
#if VAX630
struct qbus qbus630 = {
	QBA, QBAPAGES, QBAMAP630, (caddr_t)QMEM630, (caddr_t)QIOPAGE630
};
struct iobus io630[] = { IO_QBUS, 0, 0, (caddr_t)&qbus630 };
#endif

/*
 * Clock info
 */
#if VAX8600 || VAX780 || VAX750 || VAX730
int vaxstd_clkstartrt(), vaxstd_clkread(), vaxstd_clkwrite();
struct clockops vaxstd_clockops = {
	vaxstd_clkstartrt, vaxstd_clkread, vaxstd_clkwrite
};
#endif

#if VAX8200
int vaxstd_clkstartrt(), ka820_clkread(), ka820_clkwrite();
struct clockops ka820_clockops = {
	vaxstd_clkstartrt, ka820_clkread, ka820_clkwrite
};
#endif

#if VAX630
int ka630_clkstartrt(), ka630_clkread(), ka630_clkwrite();
struct clockops ka630_clockops = {
	ka630_clkstartrt, ka630_clkread, ka630_clkwrite
};
#endif

/*
 * CPU dependent routines.
 */
#if VAX8600
int	ka860_memenable(), ka860_memerr(), ka860_mchk();
struct	cpuops ka860_ops = {
	&vaxstd_clockops, ka860_memenable, ka860_memerr, ka860_mchk, NULL
};
#endif

#if VAX8200
int	ka820_memenable(), ka820_memerr(), ka820_mchk(), ka820_init();
struct	cpuops ka820_ops = {
	&ka820_clockops, ka820_memenable, ka820_memerr, ka820_mchk, ka820_init
};
#endif

#if VAX780
int	ka780_memenable(), ka780_memerr(), ka780_mchk();
struct	cpuops ka780_ops = {
	&vaxstd_clockops, ka780_memenable, ka780_memerr, ka780_mchk, NULL
};
#endif

#if VAX750
int	ka750_memenable(), ka750_memerr(), ka750_mchk();
struct	cpuops ka750_ops = {
	&vaxstd_clockops, ka750_memenable, ka750_memerr, ka750_mchk, NULL
};
#endif

#if VAX730
int	ka730_memenable(), ka730_memerr(), ka730_mchk();
struct	cpuops ka730_ops = {
	&vaxstd_clockops, ka730_memenable, ka730_memerr, ka730_mchk, NULL
};
#endif

#if VAX630
int	ka630_memnop(), ka630_mchk(), ka630_init();
struct	cpuops ka630_ops = {
	&ka630_clockops, ka630_memnop, ka630_memnop, ka630_mchk, ka630_init
};
#endif

struct percpu percpu[] = {
#if VAX8600
	{ VAX_8600, 6, 2, io8600, &ka860_ops },
#endif
#if VAX8200
	{ VAX_8200, 2, 1, io8200, &ka820_ops },
#endif
#if VAX780
	{ VAX_780, 2, 1, io780, &ka780_ops },
#endif
#if VAX750
	{ VAX_750, 1, 1, io750, &ka750_ops },
#endif
#if VAX730
	{ VAX_730, 1, 1, io730, &ka730_ops },
#endif
#if VAX630
	{ VAX_630, 2, 1, io630, &ka630_ops },
#endif
	0,
};
