/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)autoconf.c	7.10 (Berkeley) %G%
 */

#include "param.h"
#include "reboot.h"

#include "../vax/cpu.h"
#include "../vax/nexus.h"
#include "../vax/pte.h"
#include "../vax/mtpr.h"
#include "../vaxuba/ubareg.h"
#include "../vaxmba/mbareg.h"

#include "savax.h"

#ifdef VAX8200
#include "../vaxbi/bireg.h"
#endif

#if VAX8600 || VAX8200 || VAX780
/*
 * These are used on CPU's that do configuration.
 */
struct	uba_regs *ubaddrspace[MAXNUBA];
caddr_t	uioaddrspace[MAXNUBA];
struct	mba_regs *mbaddrspace[MAXNMBA];
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

int (*v_getc)(), (*v_putc)();

#ifndef SMALL
/*
 * Virtual console configuration tables.
 */
extern qv_init(),qd_init();

int (*vcons_init[])() = {
	qd_init,
	qv_init,
	0
};
#endif
#endif

#ifndef SMALL
extern int boothowto;
int debug = 0;
#endif
int cpuspeed = 1;

configure()
{
	union cpusid cpusid;
	register int i;

#ifndef SMALL
	if (boothowto & RB_KDB)		/* XXX */
		debug = 1;
#endif
	cpusid.cpusid = mfpr(SID);
	cpu = cpusid.cpuany.cp_type;
	switch (cpu) {

#if VAX8600
	case VAX_8600:
#ifndef SMALL
		if (debug)
			printf("cpu: 8600\nsbia 0:\n");
#endif
		cpuspeed = 6;
		probenexi(NEXA8600, (caddr_t)UMEMA8600(0)+UBAIOADDR, 0);
		probenexi(NEXB8600, (caddr_t)UMEMB8600(0)+UBAIOADDR, 1);
		break;
#endif

#if VAX780
	case VAX_780:
#ifndef SMALL
		if (debug)
			printf("cpu: 780\n");
#endif
		cpuspeed = 2;
		probenexi(NEX780, (caddr_t)UMEM780(0)+UBAIOADDR, 0);
		break;
#endif

#if VAX8200
	case VAX_8200: {
		register struct bi_node *bi;

		cpuspeed = 2;
		for (i = 0, bi = BI_BASE(0); i < NNODEBI; i++, bi++) {
			if (badaddr((caddr_t)bi, sizeof (long)))
				continue;
#ifdef notdef
			/* clear bus errors */
			bi->biic.bi_ber = ~(BIBER_MBZ|BIBER_NMR|BIBER_UPEN);
#endif
#ifndef SMALL
			if (debug)
				printf("node%d: ", i);
#endif
			switch (bi->biic.bi_dtype) {

			case BIDT_DWBUA:
				if (nuba >= MAXNUBA)	/* sorry */
					break;
#ifndef SMALL
				if (debug)
					printf("uba%d\n", nuba);
#endif
				ubaddrspace[nuba] = (struct uba_regs *)bi;
				uioaddrspace[nuba] = (caddr_t)UMEM8200(i) +
				    UBAIOADDR;
				((struct dwbua_regs *)bi)->bua_csr |=
				    BUACSR_UPI;
				nuba++;
				break;

			case BIDT_KDB50:
				if (nkdb < MAXNKDB) {
					kdbaddr[nkdb++] = (caddr_t)bi;
#ifndef SMALL
					if (debug)
						printf("kdb%d\n", nkdb);
#endif
				}
				break;
#ifndef SMALL
			default:
				if (debug)
					printf("unknown type %x\n",
					    bi->biic.bi_dtype);
				break;
#endif
			}
		}
		ubaddr = ubaddrspace;
		uioaddr = uioaddrspace;
	}
		break;
#endif

#if VAX750
	case VAX_750:
#ifndef SMALL
		if (debug)
			printf("cpu: 750 -- assuming standard config\n");
#endif
		mbaddr = mbaddr750;
		ubaddr = ubaddr750;
		uioaddr = uioaddr750;
		nmba = sizeof (mbaddr750) / sizeof (mbaddr750[0]);
		nuba = 2;
		break;
#endif

#if VAX730
	case VAX_730:
#ifndef SMALL
		if (debug)
			printf("cpu: 730 -- assuming standard config\n");
#endif
		ubaddr = ubaddr730;
		uioaddr = uioaddr730;
		nuba = 1;
		break;
#endif

#if VAX630
	case VAX_630:
#ifndef SMALL
		if (debug)
			printf("cpu: uVAX II\n");
#endif
		ubaddr = ubaddr630;
		uioaddr = uioaddr630;
		nuba = 1;
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
#if !defined(SMALL)
		/*
		 * configure the console
		 */
		for(i = 0; vcons_init[i] && !(*vcons_init[i])(); i++)
			;
#endif
		break;
#endif /* VAX630 */
	}

	/* give unibus devices a chance to recover... */
	if (nuba > 0)
		DELAY(2000000);
}

#if VAX8600 || VAX780
probenexi(nxp, umembase, sbia)
	register struct nexus *nxp;
	caddr_t umembase;
	int sbia;
{
	register int i;
	union nexcsr nexcsr;
	int first = 1;

	for (i = 0; i < 16; i++, nxp++) {
		if (badaddr(nxp, sizeof(long)))
			continue;
		nexcsr = nxp->nexcsr;
		if (nexcsr.nex_csr & NEX_APD)
			continue;
#ifndef SMALL
		if (debug) {
			if (first && sbia != 0)
				printf("sbia %d:\n", sbia);
			printf("tr%d: ", i);
			first = 0;
		}
#endif
		switch (nexcsr.nex_type) {
		default:
#ifndef SMALL
			if (debug)
				printf("nexid %2x\n", nexcsr.nex_type);
#endif
			break;

		case NEX_MEM4:
		case NEX_MEM4I:
		case NEX_MEM16:
		case NEX_MEM16I:
		case NEX_MEM64L:
		case NEX_MEM64LI:
		case NEX_MEM64U:
		case NEX_MEM64UI:
		case NEX_MEM64I:
#ifndef SMALL
			if (debug)
				printf("mem\n");
#endif
			break;

		case NEX_CI:
#ifndef SMALL
			if (debug)
				printf("ci\n");
#endif
			break;

		case NEX_DR32:
#ifndef SMALL
			if (debug)
				printf("dr32\n");
#endif
			break;

		case NEX_MPM0:
		case NEX_MPM1:
		case NEX_MPM2:
		case NEX_MPM3:
#ifndef SMALL
			if (debug)
				printf("mpm\n");
#endif
			break;

		case NEX_MBA:
			if (nmba >= MAXNMBA) {
#ifndef SMALL
				if (debug)
					printf("unsupported mba\n");
#endif
				break;
			}
#ifndef SMALL
			if (debug)
				printf("mba%d\n", nmba);
#endif
			mbaddrspace[nmba] = (struct mba_regs *)nxp;
			nmba++;
			break;

		case NEX_UBA0:
		case NEX_UBA1:
		case NEX_UBA2:
		case NEX_UBA3:
			if (nuba >= MAXNUBA) {
#ifndef SMALL
				if (debug)
					printf("unsupported uba\n");
#endif
				break;
			}
#ifndef SMALL
			if (debug)
				printf("uba%d umem%d", nuba,
				    nexcsr.nex_type&3);
#endif
			ubaddrspace[nuba] = (struct uba_regs *)nxp;
			uioaddrspace[nuba] = umembase +
			    (nexcsr.nex_csr & 3) * (512*NBPG);
#ifndef SMALL
			if (debug)
				printf(" (%x)\n", uioaddrspace[nuba]);
#endif
			nuba++;
			((struct uba_regs *)nxp)->uba_cr = UBACR_ADINIT;
			break;
		}
	}
	mbaddr = mbaddrspace;
	ubaddr = ubaddrspace;
	uioaddr = uioaddrspace;
#undef	UTR
#undef	UMA
#undef	MTR
}
#endif /* VAX780 || VAX8600 */
