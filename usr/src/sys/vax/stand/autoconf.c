/*	autoconf.c	4.1	81/03/15	*/

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
struct	mba_regs *mbaddr750[] = { MTR(3), MTR(4), MTR(5), MTR(6) };

#undef	UTR
#undef	UMA
#undef	MTR

configure()
{
	union cpusid cpusid;

	cpusid.cpusid = mfpr(SID);
	cpu = cpusid.cpuany.cp_type;
	switch (cpu) {

	case VAX_780:
		mbaddr = mbaddr780;
		ubaddr = ubaddr780;
/*###47 [cc] warning: illegal pointer combination %%%*/
		umaddr = umaddr780;
		break;

	case VAX_750:
		mbaddr = mbaddr750;
		ubaddr = ubaddr750;
/*###53 [cc] warning: illegal pointer combination %%%*/
		umaddr = umaddr750;
		break;
	}
}
