/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)exec.h	7.5.1.1 (Berkeley) %G%
 */

#ifndef	_EXEC_H_
#define	_EXEC_H_

#ifndef COFF
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
#endif /* COFF */

/* a_magic */
#define	OMAGIC		0407	/* old impure format */
#define	NMAGIC		0410	/* read-only text */
#define	ZMAGIC		0413	/* demand load format */

#if !defined(vax) && !defined(tahoe) && !defined(i386)
/* a_mid */
#define	MID_ZERO	0	/* unknown - implementation dependent */
#define	MID_SUN010	1	/* sun 68010/68020 binary */
#define	MID_SUN020	2	/* sun 68020-only binary */
#define	MID_HP200	200	/* hp200 (68010) BSD binary */
#define	MID_HP300	300	/* hp300 (68020+68881) BSD binary */
#define	MID_HPUX	0x20C	/* hp200/300 HP-UX binary */
#define	MID_HPUX800     0x20B   /* hp800 HP-UX binary */
#endif

#ifdef COFF
/*
 * procAOUT.h --
 *
 *	The a.out format for an object file.
 *
 * Copyright (C) 1989 Digital Equipment Corporation.
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appears in all copies.  
 * Digital Equipment Corporation makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * $Header: /sprite/src/kernel/proc/ds3100.md/RCS/procMach.h,v 9.3 90/02/20 15:35:50 shirriff Exp $ SPRITE (Berkeley)
 */

/*
 * File header magic number.
 */
#define	COFF_MAGIC	0x0162

/*
 * Description of the file.
 */
typedef struct {
    unsigned short	magic;		/* The magic number. */
    unsigned short	numSections;	/* The number of sections. */
    long		timeDateStamp;	/* Time and date stamp. */		
    long		symPtr;		/* File pointer to symbolic header. */	
    long		numSyms;	/* Size of symbolic header. */
    unsigned short	optHeader;	/* Size of optional header. */
    unsigned short	flags;		/* Flags. */
} ProcFileHeader;

/*
 * A.out header.
 */
typedef struct {
    short		magic;		/* Magic number. */
    short		verStamp;	/* Version stamp. */
    long		codeSize;	/* Code size in bytes. */
    long		heapSize;	/* Initialized data size in bytes. */
    long		bssSize;	/* Uninitialized data size in bytes. */
    long		entry;		/* Entry point. */
    long		codeStart;	/* Base of code used for this file. */
    long		heapStart;	/* Base of heap used for this file. */
    long		bssStart;	/* Base of bss used for this file. */
    long		gprMask;	/* General purpose register mask. */
    long		cprMask[4];	/* Co-processor register masks. */
    long		gpValue;	/* The gp value for this object. */
} ProcAOUTHeader;

/*
 * Section header.
 */
typedef struct {
    char		name[8];	/* Section name. */
    long		physAddr;	/* Section physical address. */
    long		virtAddr;	/* Section virtual address. */
    long		size;		/* Section size. */
    long		sectionPtr;	/* File pointer to section data.  */
    long		relocPtr;	/* File pointer to relocation data.  */
    long		lnnoPtr;	/* File pointer to gp tables. */
    unsigned short	numReloc;	/* Number of relocation entries. */
    unsigned short	numLnno;	/* Numberof gp tables. */
    long		flags;		/* Section flags. */
} ProcSectionHeader;

/*
 * The header at the beginning of each file.
 */
struct exec {
    ProcFileHeader	ex_fhdr;
    ProcAOUTHeader	ex_aout;
};
#define a_magic	ex_aout.magic
#define a_text	ex_aout.codeSize
#define a_data	ex_aout.heapSize
#define a_bss	ex_aout.bssSize
#define a_entry	ex_aout.entry

/*
 * Determine the offset of the text segment in the file, given the header.
 * (This is the same function as N_TXTOFF)
 */
#define N_TXTOFF(hdr) \
    ( ((hdr).ex_aout.magic == ZMAGIC) ? 0 : \
	((sizeof(struct exec) + \
	    (hdr).ex_fhdr.numSections*sizeof(ProcSectionHeader) + \
	    ((hdr).ex_aout.verStamp < 23 ? 7 : 15)) & \
		~((long)(((hdr).ex_aout.verStamp < 23 ? 7 : 15))) ) )

#define N_DATAOFF(x) \
    (N_TXTOFF(x) + (x).ex_aout.codeSize)

#endif /* COFF */
#endif /* !_EXEC_H_ */
