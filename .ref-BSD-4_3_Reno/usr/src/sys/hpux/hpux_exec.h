/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * from: Utah $Hdr: hpux_exec.h 1.4 88/05/24$
 *
 *	@(#)hpux_exec.h	7.1 (Berkeley) 5/8/90
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
