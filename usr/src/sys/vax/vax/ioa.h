/*
 * @(#)ioa.h	1.1 (Berkeley) %G%
 */
/************************************************************************
 *									*
 *			Copyright (c) 1983 by				*
 *		Digital Equipment Corporation, Maynard, MA		*
 *			All rights reserved.				*
 *									*
 *   This software is furnished under a license and may be used and	*
 *   copied  only  in accordance with the terms of such license and	*
 *   with the  inclusion  of  the  above  copyright  notice.   This	*
 *   software  or  any  other copies thereof may not be provided or	*
 *   otherwise made available to any other person.  No title to and	*
 *   ownership of the software is hereby transferred.			*
 *									*
 *   The information in this software is subject to change  without	*
 *   notice  and should not be construed as a commitment by Digital	*
 *   Equipment Corporation.						*
 *									*
 *   Digital assumes no responsibility for the use  or  reliability	*
 *   of its software on equipment which is not supplied by Digital.	*
 *									*
 ************************************************************************/

#define	MAXNIOA 4
#define	NIOA8600 2
#define IOASIZE 0x2000000
#define IOAMAPSIZ 512			/* Map one page to get at SBIA regs */
#define	IOA8600(i)	((short *)(0x20080000+IOASIZE*i))

#ifndef LOCORE
struct	sbia_regs
{
	int sbi_cfg;
	int sbi_csr;
	int sbi_errsum;
	int sbi_dctl;
	int sbi_dmaica;
	int sbi_dmaiid;
	int sbi_dmaaca;
	int sbi_dmaaid;
	int sbi_dmabcs;
	int sbi_dmabid;
	int sbi_dmaccs;
	int sbi_dmacid;
	int sbi_silo;
	int sbi_error;
	int sbi_timo;
	int sbi_fltsts;
	int sbi_silcmp;
	int sbi_maint;
	int sbi_unjam;
	int sbi_qclr;
	int sbi_unused[12];
	int sbi_iv10;
	int sbi_iv11;
	int sbi_iv12;
	int sbi_iv13;
	int sbi_iv14;
	int sbi_iv15;
	int sbi_iv16;
	int sbi_iv17;
	int sbi_iv18;
	int sbi_iv19;
	int sbi_iv1a;
	int sbi_iv1b;
	int sbi_iv1c;
	int sbi_iv1d;
	int sbi_iv1e;
};
struct	ioa {
	union ioacsr {
		long	ioa_csr;
		u_char	ioa_type;
	} ioacsr;
	long	ioa_pad[IOAMAPSIZ / sizeof (long) - 1];
};
#ifdef	KERNEL
struct ioa ioa[MAXNIOA];
#endif  KERNEL
#endif	LOCORE

#define IOA_TYPMSK 0xf0
#define IOA_SBIA 0x10
