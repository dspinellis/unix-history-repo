/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)exec.h	7.2 (Berkeley) %G%
 */

/*
 * Portions of this file are subject to the following copyright notice:
 *
 * Copyright (C) 1989 Digital Equipment Corporation.
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appears in all copies.  
 * Digital Equipment Corporation makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 */

/*
 * /sprite/src/kernel/proc/ds3100.md/RCS/procMach.h,v 9.3 90/02/20 15:35:50
 * shirriff Exp $ SPRITE (Berkeley)
 */

/* Size of a page in an object file. */
#define	__LDPGSZ	4096

/* Valid magic number check. */
#define	N_BADMAG(ex) \
	((ex).a_magic != NMAGIC && (ex).a_magic != OMAGIC && \
	    (ex).a_magic != ZMAGIC)

/* Address of the bottom of the text segment. */
#define N_TXTADDR(ex)	0x400000

/* Address of the bottom of the data segment. */
/* NOT DEFINED FOR THE MIPS. */

/* Text segment offset. */
#define	__N_TXTOFF_ROUND(ex) \
	((ex).ex_aout.verStamp < 23 ? 7 : 15)
#define	N_TXTOFF(ex) \
	((ex).ex_aout.magic == ZMAGIC ? 0 : (sizeof(struct exec) + \
	    (ex).ex_fhdr.numSections * sizeof(ProcSectionHeader) + \
	    __N_TXTOFF_ROUND(ex)) & ~__N_TXTOFF_ROUND(ex))

/* Data segment offset. */
#define N_DATOFF(ex) \
	(N_TXTOFF(ex) + (ex).ex_aout.codeSize)

/* Symbol table offset. */
/* NOT DEFINED FOR THE MIPS. */

/* String table offset. */
/* NOT DEFINED FOR THE MIPS. */

/*
 * XXX
 * The ProcFileHeader structure and the ProcAOUTHeader structure should be
 * folded together into a single struct exec.
 */

/* Description of the COFF section. */
typedef struct {
#define	COFF_MAGIC	0x0162
	u_short	magic;		/* The magic number. */

	u_short	numSections;	/* The number of sections. */
	long	timeDateStamp;	/* Time and date stamp. */		
	long	symPtr;		/* File pointer to symbolic header. */	
	long	numSyms;	/* Size of symbolic header. */
	u_short	optHeader;	/* Size of optional header. */
	u_short	flags;		/* Flags. */
} ProcFileHeader;

/* Description of the a.out section. */
typedef struct {
#define	OMAGIC	0407		/* old impure format */
#define	NMAGIC	0410		/* read-only text */
#define	ZMAGIC	0413		/* demand load format */
	short	magic;		/* Magic number. */

	short	verStamp;	/* Version stamp. */
	long	codeSize;	/* Code size in bytes. */
	long	heapSize;	/* Initialized data size in bytes. */
	long	bssSize;	/* Uninitialized data size in bytes. */
	long	entry;		/* Entry point. */
	long	codeStart;	/* Base of code used for this file. */
	long	heapStart;	/* Base of heap used for this file. */
	long	bssStart;	/* Base of bss used for this file. */
	long	gprMask;	/* General purpose register mask. */
	long	cprMask[4];	/* Co-processor register masks. */
	long	gpValue;	/* The gp value for this object. */
} ProcAOUTHeader;

/* Section header. */
typedef struct {
	char	name[8];	/* Section name. */
	long	physAddr;	/* Section physical address. */
	long	virtAddr;	/* Section virtual address. */
	long	size;		/* Section size. */
	long	sectionPtr;	/* File pointer to section data. */
	long	relocPtr;	/* File pointer to relocation data. */
	long	lnnoPtr;	/* File pointer to gp tables. */
	u_short	numReloc;	/* Number of relocation entries. */
	u_short	numLnno;	/* Numberof gp tables. */
	long	flags;		/* Section flags. */
} ProcSectionHeader;

/* Description of the object file header. */
struct exec {
	ProcFileHeader	ex_fhdr;
	ProcAOUTHeader	ex_aout;
};
#define a_magic	ex_aout.magic
#define a_text	ex_aout.codeSize
#define a_data	ex_aout.heapSize
#define a_bss	ex_aout.bssSize
#define a_entry	ex_aout.entry
