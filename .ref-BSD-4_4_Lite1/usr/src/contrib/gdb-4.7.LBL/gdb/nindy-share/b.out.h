/*  Copyright 1992 Free Software Foundation, Inc.
 *
 * This file is a modified version of 'a.out.h'.  It is to be used in all
 * all GNU tools modified to support the i80960 (or tools that operate on
 * object files created by such tools).
 *
 * All i80960 development is done in a CROSS-DEVELOPMENT environment.  I.e.,
 * object code is generated on, and executed under the direction of a symbolic
 * debugger running on, a host system.  We do not want to be subject to the
 * vagaries of which host it is or whether it supports COFF or a.out format,
 * or anything else.  We DO want to:
 *
 *	o always generate the same format object files, regardless of host.
 *
 *	o have an 'a.out' header that we can modify for our own purposes
 *	  (the 80960 is typically an embedded processor and may require
 *	  enhanced linker support that the normal a.out.h header can't
 *	  accommodate).
 *
 * As for byte-ordering, the following rules apply:
 *
 *	o Text and data that is actually downloaded to the target is always
 *	  in i80960 (little-endian) order.
 *
 *	o All other numbers (in the header, symbols, relocation directives)
 *	  are in host byte-order:  object files CANNOT be lifted from a
 *	  little-end host and used on a big-endian (or vice versa) without
 *	  modification.
 *
 *	o The downloader ('comm960') takes care to generate a pseudo-header
 *	  with correct (i80960) byte-ordering before shipping text and data
 *	  off to the NINDY monitor in the target systems.  Symbols and
 *	  relocation info are never sent to the target.
 */


#define BMAGIC	0415
/* We don't accept the following (see N_BADMAG macro).
 * They're just here so GNU code will compile.
 */
#define	OMAGIC	0407		/* old impure format */
#define	NMAGIC	0410		/* read-only text */
#define	ZMAGIC	0413		/* demand load format */

/* FILE HEADER
 *	All 'lengths' are given as a number of bytes.
 *	All 'alignments' are for relinkable files only;  an alignment of
 *		'n' indicates the corresponding segment must begin at an
 *		address that is a multiple of (2**n).
 */
struct exec {
	/* Standard stuff */
	unsigned long a_magic;	/* Identifies this as a b.out file	*/
	unsigned long a_text;	/* Length of text			*/
	unsigned long a_data;	/* Length of data			*/
	unsigned long a_bss;	/* Length of runtime uninitialized data area */
	unsigned long a_syms;	/* Length of symbol table		*/
	unsigned long a_entry;	/* Runtime start address		*/
	unsigned long a_trsize;	/* Length of text relocation info	*/
	unsigned long a_drsize;	/* Length of data relocation info	*/

	/* Added for i960 */
	unsigned long a_tload;	/* Text runtime load address		*/
	unsigned long a_dload;	/* Data runtime load address		*/
	unsigned char a_talign;	/* Alignment of text segment		*/
	unsigned char a_dalign;	/* Alignment of data segment		*/
	unsigned char a_balign;	/* Alignment of bss segment		*/
	unsigned char unused;	/* (Just to make struct size a multiple of 4) */
};

#define N_BADMAG(x)	(((x).a_magic)!=BMAGIC)
#define N_TXTOFF(x)	( sizeof(struct exec) )
#define N_DATOFF(x)	( N_TXTOFF(x) + (x).a_text )
#define N_TROFF(x)	( N_DATOFF(x) + (x).a_data )
#define N_DROFF(x)	( N_TROFF(x) + (x).a_trsize )
#define N_SYMOFF(x)	( N_DROFF(x) + (x).a_drsize )
#define N_STROFF(x)	( N_SYMOFF(x) + (x).a_syms )

/* A single entry in the symbol table
 */
struct nlist {
	union {
		char	*n_name;
		struct nlist *n_next;
		long	n_strx;		/* Index into string table	*/
	} n_un;
	unsigned char n_type;	/* See below				*/
	char	n_other;	/* Used in i80960 support -- see below	*/
	short	n_desc;
	unsigned long n_value;
};


/* Legal values of n_type
 */
#define N_UNDF	0	/* Undefined symbol	*/
#define N_ABS	2	/* Absolute symbol	*/
#define N_TEXT	4	/* Text symbol		*/
#define N_DATA	6	/* Data symbol		*/
#define N_BSS	8	/* BSS symbol		*/
#define N_FN	31	/* Filename symbol	*/

#define N_EXT	1	/* External symbol (OR'd in with one of above)	*/
#define N_TYPE	036	/* Mask for all the type bits			*/
#define N_STAB	0340	/* Mask for all bits used for SDB entries 	*/

/* MEANING OF 'n_other'
 *
 * If non-zero, the 'n_other' fields indicates either a leaf procedure or
 * a system procedure, as follows:
 *
 *	1 <= n_other <= 32 :
 *		The symbol is the entry point to a system procedure.
 *		'n_value' is the address of the entry, as for any other
 *		procedure.  The system procedure number (which can be used in
 *		a 'calls' instruction) is (n_other-1).  These entries come from
 *		'.sysproc' directives.
 *
 *	n_other == N_CALLNAME
 *		the symbol is the 'call' entry point to a leaf procedure.
 *		The *next* symbol in the symbol table must be the corresponding
 *		'bal' entry point to the procedure (see following).  These
 *		entries come from '.leafproc' directives in which two different
 *		symbols are specified (the first one is represented here).
 *	
 *
 *	n_other == N_BALNAME
 *		the symbol is the 'bal' entry point to a leaf procedure.
 *		These entries result from '.leafproc' directives in which only
 *		one symbol is specified, or in which the same symbol is
 *		specified twice.
 *
 * Note that an N_CALLNAME entry *must* have a corresponding N_BALNAME entry,
 * but not every N_BALNAME entry must have an N_CALLNAME entry.
 */
#define	N_CALLNAME	-1
#define	N_BALNAME	-2


struct relocation_info {
	int	 r_address;	/* File address of item to be relocated	*/
	unsigned
		r_symbolnum:24,/* Index of symbol on which relocation is based*/
		r_pcrel:1,	/* 1 => relocate PC-relative; else absolute
				 *	On i960, pc-relative implies 24-bit
				 *	address, absolute implies 32-bit.
				 */
		r_length:2,	/* Number of bytes to relocate:
				 *	0 => 1 byte
				 *	1 => 2 bytes
				 *	2 => 4 bytes -- only value used for i960
				 */
		r_extern:1,
		r_bsr:1,	/* Something for the GNU NS32K assembler */
		r_disp:1,	/* Something for the GNU NS32K assembler */
		r_callj:1,	/* 1 if relocation target is an i960 'callj' */
		nuthin:1;	/* Unused				*/
};
