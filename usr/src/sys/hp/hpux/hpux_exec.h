/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: hpux_exec.h 1.6 92/01/20$
 *
 *	@(#)hpux_exec.h	7.4 (Berkeley) %G%
 */

/*
 * HPUX a.out header format
 */
struct hpux_exec {
	long	ha_magic;	/* magic number */
	short	ha_version;	/* version ID */
	short	ha_pad0;	/* doesn't matter */
	long	ha_misc;	/* misc. info */
unsigned long	ha_text;	/* size of text segment */
unsigned long	ha_data;	/* size of initialized data */
unsigned long	ha_bss;		/* size of uninitialized data */
unsigned long	ha_pad2[5];	/* doesn't matter */
unsigned long	ha_entry;	/* entry point */
unsigned long	ha_pad3[4];	/* doesn't matter */
};

#define	HPUXM_VALID	0x00000001
#define HPUXM_STKWT	0x02000000
#define HPUXM_DATAWT	0x04000000
