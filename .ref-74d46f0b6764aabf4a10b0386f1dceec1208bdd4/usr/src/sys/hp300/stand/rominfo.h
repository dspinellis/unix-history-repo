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
 * from: Utah $Hdr: rominfo.h 1.2 88/05/24$
 *
 *	@(#)rominfo.h	7.1 (Berkeley) %G%
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

