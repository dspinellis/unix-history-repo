/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)reloc.h	7.2 (Berkeley) %G%
 *
 * from: $Header: reloc.h,v 1.6 92/06/20 09:59:37 torek Exp $
 */

/*
 * MIPS relocation types.
 */
enum reloc_type {
	MIPS_RELOC_32,		/* 32-bit absolute */
	MIPS_RELOC_JMP,		/* 26-bit absolute << 2 | high 4 bits of pc */
	MIPS_RELOC_WDISP16,	/* 16-bit signed pc-relative << 2 */
	MIPS_RELOC_HI16,	/* 16-bit absolute << 16 */
	MIPS_RELOC_HI16_S,	/* 16-bit absolute << 16 (+1 if needed) */
	MIPS_RELOC_LO16,	/* 16-bit absolute */
};

/*
 * MIPS relocation info.
 *
 * Symbol-relative relocation is done by:
 *	1. start with the value r_addend,
 *	2. locate the appropriate symbol and if defined, add symbol value,
 *	3. if pc relative, subtract pc,
 *	4. if the reloc_type is MIPS_RELOC_HI16_S and the result bit 15 is set,
 *		add 0x00010000,
 *	5. shift down 2 or 16 if necessary.
 * The resulting value is then to be stuffed into the appropriate bits
 * in the object (the low 16, or the low 26 bits).
 */
struct reloc_info_mips {
	u_long	r_address;	/* relocation addr (offset in segment) */
	u_int	r_index:24,	/* segment (r_extern==0) or symbol index */
		r_extern:1,	/* if set, r_index is symbol index */
		:2;		/* unused */
	enum reloc_type r_type:5; /* relocation type, from above */
	long	r_addend;	/* value to add to symbol value */
};
