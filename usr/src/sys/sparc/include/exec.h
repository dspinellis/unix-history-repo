/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)exec.h	7.4 (Berkeley) %G%
 *
 * from: $Header: exec.h,v 1.9 93/04/20 11:14:45 torek Exp $
 */

/*
 * __LDPGSZ is the page size used by the linker and by exec().
 * It may be some multiple of the ``normal'' page size, so that, e.g.,
 * the same binaries can be run on hardware with different page sizes
 * that otherwise use the same instruction set.  It must be no larger
 * than CLBYTES (in param.h).
 */
#define	__LDPGSZ	8192

/* Valid magic number check. */
#define	N_BADMAG(ex) \
	((ex).a_magic != ZMAGIC && (ex).a_magic != NMAGIC && \
	    (ex).a_magic != OMAGIC)

/*
 * N_TXTADDR is the address of the first executable instruction: that is,
 * the place the pc could begin after an a.out is loaded, in order to run
 * the instructions in that a.out.  The pc will actually be set to ex.a_entry
 * but this is the first place it could possibly reference.
 *
 * On the SPARC, binaries begin at __LDPGSZ, i.e., page 1.
 */
#define N_TXTADDR(ex)	8192

/* Address of the bottom of the data segment. */
#define N_DATADDR(ex) \
	(N_TXTADDR(ex) + ((ex).a_magic == OMAGIC ? (ex).a_text : \
	    (((ex).a_text + __LDPGSZ - 1) & ~(__LDPGSZ - 1))))

/*
 * N_TXTOFF is the offset within an a.out file of the first executable
 * instruction: that is, the offset in the a.out of the byte that winds
 * up at N_TXTADDR.
 *
 * On the SPARC, the a.out header is included in the executable when running
 * a ZMAGIC file (but not for OMAGIC and NMAGIC).
 */
#define	N_TXTOFF(ex)	((ex).a_magic == ZMAGIC ? 0 : sizeof(struct exec))

/* Data segment offset. */
#define	N_DATOFF(ex) \
	(N_TXTOFF(ex) + ((ex).a_magic != ZMAGIC ? (ex).a_text : \
	    (((ex).a_text + __LDPGSZ - 1) & ~(__LDPGSZ - 1))))

/* Symbol table offset. */
#define N_SYMOFF(ex) \
	(N_TXTOFF(ex) + (ex).a_text + (ex).a_data + (ex).a_trsize + \
	    (ex).a_drsize)

/* String table offset. */
#define	N_STROFF(ex) 	(N_SYMOFF(ex) + (ex).a_syms)

/* Description of the object file header (a.out format). */
struct exec {
	u_char	a_dynamic:1;	/* dynamically linked */
	u_char	a_toolversion:7;/* Sun toolset version	XXX */

#define	MID_ZERO	0	/* unknown - implementation dependent */
#define	MID_SUN010	1	/* sun 68010/68020 binary */
#define	MID_SUN020	2	/* sun 68020-only binary */
#define	MID_SUN_SPARC	3	/* sparc binary */
#define	MID_HP200	200	/* hp200 (68010) BSD binary */
#define	MID_HP300	300	/* hp300 (68020+68881) BSD binary */
#define	MID_HPUX	0x20C	/* hp200/300 HP-UX binary */
#define	MID_HPUX800     0x20B   /* hp800 HP-UX binary */
	u_char	a_mid;		/* machine ID */

#define	OMAGIC	0407		/* old impure format */
#define	NMAGIC	0410		/* read-only text */
#define	ZMAGIC	0413		/* demand load format */
	u_short	a_magic;	/* magic number */

	u_long	a_text;		/* text segment size */
	u_long	a_data;		/* initialized data size */
	u_long	a_bss;		/* uninitialized data size */
	u_long	a_syms;		/* symbol table size */
	u_long	a_entry;	/* entry point */
	u_long	a_trsize;	/* text relocation size */
	u_long	a_drsize;	/* data relocation size */
};
#define	a_machtype	a_mid	/* SUN compatibility */
