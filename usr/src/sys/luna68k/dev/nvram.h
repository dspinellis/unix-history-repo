/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *
 *	@(#)nvram.h	7.1 (Berkeley) %G%
 */

/*
 * OMRON: $Id: nvram.h,v 1.1 92/05/27 14:32:51 moti Exp $
 * by Shigeto Mochida
 */

/*
 * Non Volatile RAM
 */

#define	NVRAMSZ		2040	/* Size of NVRAM. (Total 2040 bytes) */
#define NVSYMSZ		16
#define NVVALSZ		16
#define NVSYSSZ		42
#define NVUSRSZ		512

/*
 * Battery back-up memory space.
 */
struct nvram {
	char	nv_testwrite[4];	/* for battery check */
	char	nv_hdr[4];		/* header name */
	long	nv_machtype;		/* machine-type ID */
	long	nv_machno;		/* machine number */
	char	nv_calclock[12];	/* RTC initialize */
	char	nv_checksum[4];		/* check sum for "nv_system" */
	struct nv_system {
		char	nv_symbol[NVSYMSZ];
		char	nv_value[NVVALSZ];
	} nv_system[NVSYSSZ];		/* system define */
	char	nv_reserve[152];	/* reserved */
	char	nv_user[NVUSRSZ];		/* user avail area */
};
