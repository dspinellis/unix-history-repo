/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)exec.h	7.1 (Berkeley) %G%
 */

/* Size of a page in an object file. */
#define	__LDPGSZ	4096

/* Valid magic number check. */
#define	N_BADMAG(ex) \
	((ex).a_magic != NMAGIC && (ex).a_magic != OMAGIC && \
	    (ex).a_magic != ZMAGIC)

/* Address of the bottom of the text segment. */
#define N_TXTADDR(X)	0

/* Address of the bottom of the data segment. */
#define N_DATADDR(ex) \
	(N_TXTADDR(ex) + ((ex).a_magic == OMAGIC ? (ex).a_text \
	: __LDPGSZ + ((ex).a_text - 1 & ~(__LDPGSZ - 1))))

/* Text segment offset. */
#define	N_TXTOFF(ex) \
	((ex).a_magic == ZMAGIC ? __LDPGSZ : sizeof(struct exec))

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
#define	OMAGIC	0407		/* old impure format */
#define	NMAGIC	0410		/* read-only text */
#define	ZMAGIC	0413		/* demand load format */
	long	a_magic;	/* magic number */

	u_long	a_text;		/* text segment size */
	u_long	a_data;		/* initialized data size */
	u_long	a_bss;		/* uninitialized data size */
	u_long	a_syms;		/* symbol table size */
	u_long	a_entry;	/* entry point */
	u_long	a_trsize;	/* text relocation size */
	u_long	a_drsize;	/* data relocation size */
};
