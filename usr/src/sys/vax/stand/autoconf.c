/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)autoconf.c	6.2 (Berkeley) %G%
 */

#include "../machine/pte.h"

#include "../h/param.h"

#include "../vax/cpu.h"
#include "../vax/nexus.h"
#include "../vaxuba/ubareg.h"
#include "../vaxmba/mbareg.h"
#include "../vax/mtpr.h"

#include "savax.h"

#define	UTR(i)	((struct uba_regs *)(NEX780+(i)))
#define	UMA(i)	((caddr_t)UMEM780(i))
#define	MTR(i)	((struct mba_regs *)(NEX780+(i)))

struct	uba_regs *ubaddr780[] = { UTR(3), UTR(4), UTR(5), UTR(6) };
caddr_t	umaddr780[] = { UMA(0), UMA(1), UMA(2), UMA(3) };
struct	mba_regs *mbaddr780[] = { MTR(8), MTR(9), MTR(10), MTR(11) };

#undef	UTR
#undef	UMA
#undef	MTR

#define	UTR(i)	((struct uba_regs *)(NEX750+(i)))
#define	UMA(i)	((caddr_t)UMEM750(i))
#define	MTR(i)	((struct mba_regs *)(NEX750+(i)))

struct	uba_regs *ubaddr750[] = { UTR(8), UTR(9) };
caddr_t	umaddr750[] = { UMA(0), UMA(1) };
struct	mba_regs *mbaddr750[] = { MTR(4), MTR(5), MTR(6), MTR(7) };

#undef	UTR
#undef	UMA
#undef	MTR

#define	UTR(i)	((struct uba_regs *)(NEX730+(i)))
#define	UMA	((caddr_t)UMEM730)

struct	uba_regs *ubaddr730[] = { UTR(3) };
caddr_t	umaddr730[] = { UMA };

#undef	UTR
#undef	UMA

configure()
{
	union cpusid cpusid;
	int nmba, nuba, i;

	cpusid.cpusid = mfpr(SID);
	cpu = cpusid.cpuany.cp_type;
	switch (cpu) {

	case VAX_780:
		mbaddr = mbaddr780;
		ubaddr = ubaddr780;
		umaddr = umaddr780;
		nmba = sizeof (mbaddr780) / sizeof (mbaddr780[0]);
		nuba = sizeof (ubaddr780) / sizeof (ubaddr780[0]);
		break;

	case VAX_750:
		mbaddr = mbaddr750;
		ubaddr = ubaddr750;
		umaddr = umaddr750;
		nmba = sizeof (mbaddr750) / sizeof (mbaddr750[0]);
		nuba = 0;
		break;

	case VAX_730:
		ubaddr = ubaddr730;
		umaddr = umaddr730;
		nmba = nuba = 0;
		break;
	}
	/*
	 * Forward into the past...
	 */
/*
	for (i = 0; i < nmba; i++)
		if (!badloc(mbaddr[i]))
			mbaddr[i]->mba_cr = MBCR_INIT;
*/
	for (i = 0; i < nuba; i++)
		if (!badloc(ubaddr[i]))
			ubaddr[i]->uba_cr = UBACR_ADINIT;
	if (cpu != VAX_780)
		mtpr(IUR, 0);
	/* give unibus devices a chance to recover... */
	if (nuba > 0)
		DELAY(2000000);
}
