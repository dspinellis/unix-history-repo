/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)cpudata.c	6.3 (Berkeley) %G%
 */

#include "pte.h"

#include "param.h"

#include "cpu.h"
#include "nexus.h"
#include "../vaxuba/ubareg.h"

/*
 * Initialization of per-cpu data structures.
 */

/*
 * These are the (fixed) addresses of the (last 8k bytes of)
 * unibus memory for each of the possible unibus adapters.  Note that the
 * unibus memory addresses are actually indexed by the unibus adapter type code.
 */
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

struct percpu percpu[] = {
#if VAX780
	VAX_780, NNEX780, NEX780, umaddr780, NBDP780, 1, 0,
#endif
#if VAX750
	VAX_750, NNEX750, NEX750, umaddr750, NBDP750, 0, nexty750,
#endif
#if VAX730
	VAX_730, NNEX730, NEX730, umaddr730, NBDP730, 0, nexty730,
#endif
	0,
};
