/*	autoconf.c	4.4	81/04/03	*/

#include "../h/param.h"
#include "../h/cpu.h"
#include "../h/nexus.h"
#include "../h/pte.h"
#include "../h/ubareg.h"
#include "../h/mbareg.h"
#include "../h/mtpr.h"
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

#define	UTR(i)	((struct uba_regs *)(NEX7ZZ+(i)))
#define	UMA	((caddr_t)UMEM7ZZ)

struct	uba_regs *ubaddr7ZZ[] = { UTR(3) };
caddr_t	umaddr7ZZ[] = { UMA };

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

	case VAX_7ZZ:
		ubaddr = ubaddr7ZZ;
		umaddr = umaddr7ZZ;
		nmba = nuba = 0;
		break;
	}
	/*
	 * Forward into the past...
	 */
	for (i = 0; i < nmba; i++)
		if (!badloc(mbaddr[i]))
			mbaddr[i]->mba_cr = MBCR_INIT;
	for (i = 0; i < nuba; i++)
		if (!badloc(ubaddr[i]))
			ubaddr[i]->uba_cr = UBACR_ADINIT;
	/* give unibus devices a chance to recover... */
	if (nuba > 0)
		DELAY(2000000);
}
