/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)exec.h	7.5 (Berkeley) %G%
 */

#ifndef	_EXEC_H_
#define	_EXEC_H_

/* Header prepended to each a.out file. */
struct exec {
#if !defined(vax) && !defined(tahoe) && !defined(i386)
unsigned short	a_mid;		/* machine ID */
unsigned short	a_magic;	/* magic number */
#else
	 long	a_magic;	/* magic number */
#endif
unsigned long	a_text;		/* text segment size */
unsigned long	a_data;		/* initialized data size */
unsigned long	a_bss;		/* uninitialized data size */
unsigned long	a_syms;		/* symbol table size */
unsigned long	a_entry;	/* entry point */
unsigned long	a_trsize;	/* text relocation size */
unsigned long	a_drsize;	/* data relocation size */
};
#define	a_machtype	a_mid	/* SUN compatibility */

/* a_magic */
#define	OMAGIC		0407	/* old impure format */
#define	NMAGIC		0410	/* read-only text */
#define	ZMAGIC		0413	/* demand load format */

/* a_mid */
#define	MID_ZERO	0	/* unknown - implementation dependent */
#define	MID_SUN010	1	/* sun 68010/68020 binary */
#define	MID_SUN020	2	/* sun 68020-only binary */
#define	MID_HP200	200	/* hp200 (68010) BSD binary */
#define	MID_HP300	300	/* hp300 (68020+68881) BSD binary */
#define	MID_HPUX	0x20C	/* hp200/300 HP-UX binary */
#define	MID_HPUX800     0x20B   /* hp800 HP-UX binary */

#endif /* !_EXEC_H_ */
