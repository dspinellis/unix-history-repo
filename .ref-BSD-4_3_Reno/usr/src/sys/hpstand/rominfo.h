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
 * from: Utah $Hdr: rominfo.h 1.2 88/05/24$
 *
 *	@(#)rominfo.h	7.1 (Berkeley) 5/8/90
 */

#define ROMADDR	0xFFFFF000

struct jmpvec {
	short op;	/* jmp instruction */
	long  addr;	/* address */
};

struct rominfo {
	char p1[0xDC0];
	short boottype;		/* ??                           (FFFFFDC0) */
	char  name[10];		/* HP system name, e.g. SYSHPUX (FFFFFDC2) */
	short p2;		/* ??                           (FFFFFDCC) */
	long  lowram;		/* lowest useable RAM location  (FFFFFDCE) */
	char  p3[0x100];	/* ??                           (FFFFFDD2) */
	char  sysflag;		/* HP system flags              (FFFFFED2) */
	char  p4;		/* ??                           (FFFFFED3) */
	long  rambase;		/* physaddr of lowest RAM       (FFFFFED4) */
	char  ndrives;		/* number of drives             (FFFFFED8) */
	char  p5;		/* ??                           (FFFFFED9) */
	char  sysflag2;		/* more system flags            (FFFFFEDA) */
	char  p6;		/* ??                           (FFFFFEDB) */
	long  msus;		/* ??                           (FFFFFEDC) */
	struct jmpvec jvec[48];	/* jump vectors                 (FFFFFEE0) */
};

