/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)autoconf.c	7.3 (Berkeley) %G%
 */

#include "../machine/pte.h"

#include "param.h"

#include "../vax/cpu.h"
#include "../vax/nexus.h"
#include "../vaxuba/ubareg.h"
#include "../vaxmba/mbareg.h"
#include "../vax/mtpr.h"

#include "savax.h"

#ifdef VAX8200
#include "../vax/bireg.h"
/*
 * These are found during configuration, rather than being compiled in
 * statically.
 */
struct	uba_regs *ubaddr8200[MAXNUBA];
caddr_t	uioaddr8200[MAXNUBA];
#endif

#if VAX8600 || VAX780
#define	UTR(i)	((struct uba_regs *)(NEX780+(i)))
#define	UMA(i)	((caddr_t)UMEM780(i)+UBAIOADDR)
#define	MTR(i)	((struct mba_regs *)(NEX780+(i)))
#define	UTRB(i)	((struct uba_regs *)(NEXB8600+(i)))
#define	UMAB(i)	((caddr_t)UMEMB8600(i)+UBAIOADDR)
#define	MTRB(i)	((struct mba_regs *)(NEXB8600+(i)))

struct	uba_regs *ubaddr780[] = {
	UTR(3), UTR(4), UTR(5), UTR(6),
#if VAX8600
	UTRB(3), UTRB(4), UTRB(5), UTRB(6),
#endif
};
caddr_t	uioaddr780[] = {
	UMA(0), UMA(1), UMA(2), UMA(3),
#if VAX8600
	UMAB(0), UMAB(1), UMAB(2), UMAB(3),
#endif
};
struct	mba_regs *mbaddr780[] = {
	MTR(8), MTR(9), MTR(10), MTR(11),
#if VAX8600
	MTRB(8), MTRB(9), MTRB(10), MTRB(11),
#endif
};

#undef	UTR
#undef	UMA
#undef	MTR
#endif

#if VAX750
#define	UTR(i)	((struct uba_regs *)(NEX750+(i)))
#define	UMA(i)	((caddr_t)UMEM750(i)+UBAIOADDR)
#define	MTR(i)	((struct mba_regs *)(NEX750+(i)))

struct	uba_regs *ubaddr750[] = { UTR(8), UTR(9) };
caddr_t	uioaddr750[] = { UMA(0), UMA(1) };
struct	mba_regs *mbaddr750[] = { MTR(4), MTR(5), MTR(6), MTR(7) };

#undef	UTR
#undef	UMA
#undef	MTR
#endif

#if VAX730
#define	UTR(i)	((struct uba_regs *)(NEX730+(i)))
#define	UMA	((caddr_t)UMEM730+UBAIOADDR)

struct	uba_regs *ubaddr730[] = { UTR(3) };
caddr_t	uioaddr730[] = { UMA };

#undef	UTR
#undef	UMA
#endif

#if VAX630
/*
 * The map registers start at 20088000 on the ka630, so
 * subtract a 2k offset to make things work.
 *
 * This could stand serious cleanup.
 */
struct	uba_regs *ubaddr630[] =
	{ (struct uba_regs *)((caddr_t)QBAMAP630 - 0x800) };
caddr_t	uioaddr630[] = { (caddr_t)QIOPAGE630 };
#endif

configure()
{
	union cpusid cpusid;
	register int nmba, nuba, i;

	cpusid.cpusid = mfpr(SID);
	cpu = cpusid.cpuany.cp_type;
	switch (cpu) {

#if VAX8600
	case VAX_8600:
		nmba = sizeof (mbaddr780) / sizeof (mbaddr780[0]);
		nuba = sizeof (ubaddr780) / sizeof (ubaddr780[0]);
		mbaddr = mbaddr780;
		ubaddr = ubaddr780;
		uioaddr = uioaddr780;
		break;
#endif

#if VAX780
	case VAX_780:
		nmba = 4;
		nuba = 4;
		mbaddr = mbaddr780;
		ubaddr = ubaddr780;
		uioaddr = uioaddr780;
		break;
#endif

#if VAX8200
	case VAX_8200: {
		register struct bi_node *bi;

		nmba = 0;
		nuba = 0;
		for (i = 0, bi = BI_BASE(0); i < NNODEBI; i++, bi++) {
			if (badaddr((caddr_t)bi, sizeof (long)))
				continue;
#ifdef notdef
			/* clear bus errors */
			bi->biic.bi_ber = ~(BIBER_MBZ|BIBER_NMR|BIBER_UPEN);
#endif
			switch (bi->biic.bi_dtype) {

			case BIDT_DWBUA:
				if (nuba >= MAXNUBA)	/* sorry */
					break;
				ubaddr8200[nuba] = (struct uba_regs *)bi;
				uioaddr8200[nuba] = (caddr_t)UMEM8200(i);
				((struct dwbua_regs *)bi)->bua_csr |=
				    BUACSR_UPI;
				nuba++;
				break;

			case BIDT_KDB50:
				if (nkdb < MAXNKDB)
					kdbaddr[nkdb++] = (caddr_t)bi;
				break;
			}
		}
		ubaddr = ubaddr8200;
		uioaddr = uioaddr8200;
	}
		break;
#endif

#if VAX750
	case VAX_750:
		mbaddr = mbaddr750;
		ubaddr = ubaddr750;
		uioaddr = uioaddr750;
		nmba = sizeof (mbaddr750) / sizeof (mbaddr750[0]);
		nuba = 0;
		break;
#endif

#if VAX730
	case VAX_730:
		ubaddr = ubaddr730;
		uioaddr = uioaddr730;
		nmba = 0;
		nuba = 0;
		break;
#endif

#if VAX630
	case VAX_630:
		ubaddr = ubaddr630;
		uioaddr = uioaddr630;
		nmba = 0;
		nuba = 0;
		break;
#endif
	}

	/*
	 * Forward into the past...
	 */
/*
	for (i = 0; i < nmba; i++)
		if (!badaddr(mbaddr[i], sizeof(long)))
			mbaddr[i]->mba_cr = MBCR_INIT;
*/
	switch (cpu) {

#if VAX8600 || VAX780
	case VAX_8600:
	case VAX_780:
		for (i = 0; i < nuba; i++)
			if (!badaddr(ubaddr[i], sizeof(long)))
				ubaddr[i]->uba_cr = UBACR_ADINIT;
		break;
#endif

#if VAX750 || VAX730
	case VAX_750:
	case VAX_730:
		mtpr(IUR, 0);
		break;
#endif

#if VAX630
	case VAX_630:
		mtpr(IUR, 0);
		*((char *)QIOPAGE630 + QIPCR) = Q_LMEAE;
		break;
#endif
	}

	/* give unibus devices a chance to recover... */
	if (nuba > 0)
		DELAY(2000000);
}
