/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)exec.h	7.3 (Berkeley) %G%
 */

#include <machine/endian.h>

/* Size of a page in an object file. */
#define	__LDPGSZ	4096

/* Valid magic number check. */
#define	N_BADMAG(ex) \
	((ex).a_magic != NMAGIC && (ex).a_magic != OMAGIC && \
	    (ex).a_magic != ZMAGIC)

/* Address of the bottom of the text segment. */
#define N_TXTADDR(X)	__LDPGSZ

/* Address of the bottom of the data segment. */
#define N_DATADDR(ex) \
	(N_TXTADDR(ex) + ((ex).a_magic == OMAGIC ? (ex).a_text \
	: __LDPGSZ + ((ex).a_text - 1 & ~(__LDPGSZ - 1))))

/* Text segment offset. */
#define	N_TXTOFF(ex) \
	((ex).a_magic == ZMAGIC ? 0 : sizeof(struct exec))

/* Data segment offset. */
#define	N_DATOFF(ex) \
	(N_TXTOFF(ex) + ((ex).a_magic != ZMAGIC ? (ex).a_text : \
	__LDPGSZ + ((ex).a_text - 1 & ~(__LDPGSZ - 1))))

/* Symbol table offset. */
#define N_SYMOFF(ex) \
	(N_TXTOFF(ex) + (ex).a_text + (ex).a_data + (ex).a_trsize + \
	    (ex).a_drsize)

/* String table offset. */
#define	N_STROFF(ex) 	(N_SYMOFF(ex) + (ex).a_syms)

/* Description of the object file header (a.out format). */
struct exec {
#if BYTE_ORDER == BIG_ENDIAN
	u_short	a_mid;		/* machine ID */
	u_short	a_magic;	/* magic number */
#else
	u_short	a_magic;	/* magic number */
	u_short	a_mid;		/* machine ID */
#endif

	u_long	a_text;		/* text segment size */
	u_long	a_data;		/* initialized data size */
	u_long	a_bss;		/* uninitialized data size */
	u_long	a_syms;		/* symbol table size */
	u_long	a_entry;	/* entry point */
	u_long	a_trsize;	/* text relocation size */
	u_long	a_drsize;	/* data relocation size */
};

#define	a_machtype	a_mid	/* SUN compatibility */

#define	MID_ZERO	0	/* unknown - implementation dependent */
#define	MID_SUN010	1	/* sun 68010/68020 binary */
#define	MID_SUN020	2	/* sun 68020-only binary */
#define	MID_SUN_SPARC	3	/* sparc binary */
#define	MID_386		100	/* Intel 80386 binary */
#define	MID_29K		101	/* AMD 29000 binary */
#define	MID_MIPSI	151	/* MIPS R2000/R3000 binary */
#define	MID_MIPSII	152	/* MIPS R4000 binary */
#define	MID_HP200	200	/* hp200 (68010) BSD binary */
#define	MID_HP300	300	/* hp300 (68020+68881) BSD binary */
#define	MID_HPUX	0x20C	/* hp200/300 HP-UX binary */
#define	MID_HPUX800	0x20B	/* hp800 HP-UX binary */

#define	OMAGIC	0407		/* old impure format */
#define	NMAGIC	0410		/* read-only text */
#define	ZMAGIC	0413		/* demand load format */
