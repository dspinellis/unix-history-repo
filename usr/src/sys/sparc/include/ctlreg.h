/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ctlreg.h	7.4 (Berkeley) %G%
 *
 * from: $Header: ctlreg.h,v 1.6 93/04/27 14:29:07 torek Exp $
 */

/*
 * Sun-4 (sort of) and 4c (Campus, i.e., SparcStation) control registers
 * (includes address space definitions and registers in control space).
 *
 * Address space identifiers are 8 bits (0 through 255), but we
 * only use four of them.
 */

/*			0	   unused */
/*			1	   unused */
#define	ASI_CONTROL	2	/* cache enable, context reg, etc */
#define	ASI_SEGMAP	3	/* segment maps (so we can reach each pmeg) */
#define	ASI_PTE		4	/* PTE space (pmegs) */
#define	ASI_HWFLUSHSEG	5	/* hardware assisted version of FLUSHSEG */
#define	ASI_HWFLUSHPG	6	/* hardware assisted version of FLUSHPG */
#define	ASI_HWFLUSHCTX	7	/* hardware assisted version of FLUSHCTX */
#define	ASI_USERI	8	/* I-space (user) */
#define	ASI_KERNELI	9	/* I-space (kernel) */
#define	ASI_USERD	10	/* D-space (user) */
#define	ASI_KERNELD	11	/* D-space (kernel) */
#define	ASI_FLUSHSEG	12	/* causes hardware to flush cache segment */
#define	ASI_FLUSHPG	13	/* causes hardware to flush cache page */
#define	ASI_FLUSHCTX	14	/* causes hardware to flush cache context */
#ifdef SUN4
#define	ASI_DCACHE	15	/* flush data cache; not used on 4c */
#endif
/* 16 through 255 unused (and probably wrap, but who cares?) */

/* registers in the control space */
#define	AC_CONTEXT	0x30000000	/* context register (byte) */
#define	AC_SYSENABLE	0x40000000	/* system enable register (byte) */
#define	AC_CACHETAGS	0x80000000	/* cache tag base address */
#define	AC_SERIAL	0xf0000000	/* special serial port sneakiness */
	/* AC_SERIAL is not used in the kernel (it is for the PROM) */

#ifdef SUN4
#define	AC_DVMA_ENABLE	0x50000000	/* enable user dvma */
#define	AC_BUS_ERR	0x60000000	/* bus error register */
#define	AC_DIAG_REG	0x70000000	/* diagnostic reg */
#define	AC_DVMA_MAP	0xd0000000	/* user dvma map entries */
#endif

#ifdef SUN4C
#define	AC_SYNC_ERR	0x60000000	/* sync (memory) error reg */
#define	AC_SYNC_VA	0x60000004	/* sync error virtual addr */
#define	AC_ASYNC_ERR	0x60000008	/* async error reg */
#define	AC_ASYNC_VA	0x6000000c	/* async error virtual addr */
#define	AC_CACHEDATA	0x90000000	/* cached data */

/*
 * Bits in sync error register.  Reading the register clears these;
 * otherwise they accumulate.  The error(s) occurred at the virtual
 * address stored in the sync error address register, and may have
 * been due to, e.g., what would usually be called a page fault.
 * Worse, the bits accumulate during instruction prefetch, so
 * various bits can be on that should be off.
 */
#define	SER_WRITE	0x8000		/* error occurred during write */
#define	SER_INVAL	0x80		/* PTE had PG_V off */
#define	SER_PROT	0x40		/* operation violated PTE prot */
#define	SER_TIMEOUT	0x20		/* bus timeout (non-existent mem) */
#define	SER_SBUSERR	0x10		/* S-Bus bus error */
#define	SER_MEMERR	0x08		/* memory ecc/parity error */
#define	SER_SZERR	0x02		/* size error, whatever that is */
#define	SER_WATCHDOG	0x01		/* watchdog reset (never see this) */

#define	SER_BITS \
"\20\20WRITE\10INVAL\7PROT\6TIMEOUT\5SBUSERR\4MEMERR\2SZERR\1WATCHDOG"

/*
 * Bits in async error register (errors from DVMA or Sun-4 cache
 * writeback).  The corresponding bit is also set in the sync error reg.
 *
 * A writeback invalid error means there is a bug in the PTE manager.
 *
 * The word is that the async error register does not work right.
 */
#define	AER_WBINVAL	0x80		/* writeback found PTE without PG_V */
#define	AER_TIMEOUT	0x20		/* bus timeout */
#define	AER_DVMAERR	0x10		/* bus error during DVMA */

#define	AER_BITS	"\20\10WBINVAL\6TIMEOUT\5DVMAERR"

#endif /* SUN4C */

/*
 * Bits in system enable register.
 */
#define	SYSEN_DVMA	0x20		/* enable dvma */
#define	SYSEN_CACHE	0x10		/* enable cache */
#define	SYSEN_RESET	0x04		/* reset the hardware */
