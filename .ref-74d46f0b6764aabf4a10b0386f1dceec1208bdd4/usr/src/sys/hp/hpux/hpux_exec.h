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
 * from: Utah $Hdr: hpux_exec.h 1.1 90/07/09$
 *
 *	@(#)hpux_exec.h	7.2 (Berkeley) %G%
 */

/*
 * HPUX a.out header format
 */
struct hpux_exec {
	long	ha_magic;	/* magic number */
	short	ha_version;	/* version ID */
	short	ha_pad0;	/* doesn't matter */
	long	ha_pad1;	/* ditto */
unsigned long	ha_text;	/* size of text segment */
unsigned long	ha_data;	/* size of initialized data */
unsigned long	ha_bss;		/* size of uninitialized data */
unsigned long	ha_pad2[5];	/* doesn't matter */
unsigned long	ha_entry;	/* entry point */
unsigned long	ha_pad3[4];	/* doesn't matter */
};

/*
 * If the HPUX object file version number is BSDVNUM the file was built
 * with the HPUX SGS but linked with the BSD libraries.
 */
#define BSDVNUM		0x2BAD
