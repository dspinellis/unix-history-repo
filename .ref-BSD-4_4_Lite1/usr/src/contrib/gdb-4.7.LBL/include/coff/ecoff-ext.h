/* MIPS `ECOFF' symbol table external format.
   Copyright 1992 Free Software Foundation.
   Contributed by Cygnus Support.  Written by John Gilmore.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* ECOFF uses COFF-like section structures, but its own symbol format.
   This file defines the symbol format in fields whose size and alignment
   will not vary on different host systems.  The bfd module coff-msym.c
   converts between external and internal forms of this information,
   deals with byte order issues, etc.  */

/* File header as a set of bytes */

struct hdr_ext {
	unsigned char 	h_magic[2];
	unsigned char	h_vstamp[2];
	unsigned char	h_ilineMax[4];
	unsigned char	h_cbLine[4];
	unsigned char	h_cbLineOffset[4];
	unsigned char	h_idnMax[4];
	unsigned char	h_cbDnOffset[4];
	unsigned char	h_ipdMax[4];
	unsigned char	h_cbPdOffset[4];
	unsigned char	h_isymMax[4];
	unsigned char	h_cbSymOffset[4];
	unsigned char	h_ioptMax[4];
	unsigned char	h_cbOptOffset[4];
	unsigned char	h_iauxMax[4];
	unsigned char	h_cbAuxOffset[4];
	unsigned char	h_issMax[4];
	unsigned char	h_cbSsOffset[4];
	unsigned char	h_issExtMax[4];
	unsigned char	h_cbSsExtOffset[4];
	unsigned char	h_ifdMax[4];
	unsigned char	h_cbFdOffset[4];
	unsigned char	h_crfd[4];
	unsigned char	h_cbRfdOffset[4];
	unsigned char	h_iextMax[4];
	unsigned char	h_cbExtOffset[4];
};

/* File descriptor external record */

struct fdr_ext {
	unsigned char	f_adr[4];
	unsigned char	f_rss[4];
	unsigned char	f_issBase[4];
	unsigned char	f_cbSs[4];
	unsigned char	f_isymBase[4];
	unsigned char	f_csym[4];
	unsigned char	f_ilineBase[4];
	unsigned char	f_cline[4];
	unsigned char	f_ioptBase[4];
	unsigned char	f_copt[4];
	unsigned char	f_ipdFirst[2];
	unsigned char	f_cpd[2];
	unsigned char	f_iauxBase[4];
	unsigned char	f_caux[4];
	unsigned char	f_rfdBase[4];
	unsigned char	f_crfd[4];
	unsigned char	f_bits1[1];
	unsigned char	f_bits2[3];
	unsigned char	f_cbLineOffset[4];
	unsigned char	f_cbLine[4];
};

#define	FDR_BITS1_LANG_BIG		0xF8
#define	FDR_BITS1_LANG_SH_BIG		3
#define	FDR_BITS1_LANG_LITTLE		0x1F
#define	FDR_BITS1_LANG_SH_LITTLE	0

#define	FDR_BITS1_FMERGE_BIG		0x04
#define	FDR_BITS1_FMERGE_LITTLE		0x20

#define	FDR_BITS1_FREADIN_BIG		0x02
#define	FDR_BITS1_FREADIN_LITTLE	0x40

#define	FDR_BITS1_FBIGENDIAN_BIG	0x01
#define	FDR_BITS1_FBIGENDIAN_LITTLE	0x80

#define	FDR_BITS2_GLEVEL_BIG		0xC0
#define	FDR_BITS2_GLEVEL_SH_BIG		6
#define	FDR_BITS2_GLEVEL_LITTLE		0x03
#define	FDR_BITS2_GLEVEL_SH_LITTLE	0

/* We ignore the `reserved' field in bits2. */


/* Procedure descriptor external record */

struct pdr_ext {
	unsigned char	p_adr[4];
	unsigned char	p_isym[4];
	unsigned char	p_iline[4];
	unsigned char	p_regmask[4];
	unsigned char	p_regoffset[4];
	unsigned char	p_iopt[4];
	unsigned char	p_fregmask[4];
	unsigned char	p_fregoffset[4];
	unsigned char	p_frameoffset[4];
	unsigned char	p_framereg[2];
	unsigned char	p_pcreg[2];
	unsigned char	p_lnLow[4];
	unsigned char	p_lnHigh[4];
	unsigned char	p_cbLineOffset[4];
};

/* Line numbers */

struct line_ext {
	unsigned char	l_line[4];
};

/* Symbol external record */

struct sym_ext {
	unsigned char	s_iss[4];
	unsigned char	s_value[4];
	unsigned char	s_bits1[1];
	unsigned char	s_bits2[1];
	unsigned char	s_bits3[1];
	unsigned char	s_bits4[1];
};

#define	SYM_BITS1_ST_BIG		0xFC
#define	SYM_BITS1_ST_SH_BIG		2
#define	SYM_BITS1_ST_LITTLE		0x3F
#define	SYM_BITS1_ST_SH_LITTLE		0

#define	SYM_BITS1_SC_BIG		0x03
#define	SYM_BITS1_SC_SH_LEFT_BIG	3
#define	SYM_BITS1_SC_LITTLE		0xC0
#define	SYM_BITS1_SC_SH_LITTLE		6

#define	SYM_BITS2_SC_BIG		0xE0
#define	SYM_BITS2_SC_SH_BIG		5
#define	SYM_BITS2_SC_LITTLE		0x07
#define	SYM_BITS2_SC_SH_LEFT_LITTLE	2

#define	SYM_BITS2_RESERVED_BIG		0x10
#define	SYM_BITS2_RESERVED_LITTLE	0x08

#define	SYM_BITS2_INDEX_BIG		0x0F
#define	SYM_BITS2_INDEX_SH_LEFT_BIG	16
#define	SYM_BITS2_INDEX_LITTLE		0xF0
#define	SYM_BITS2_INDEX_SH_LITTLE	4

#define	SYM_BITS3_INDEX_SH_LEFT_BIG	8
#define	SYM_BITS3_INDEX_SH_LEFT_LITTLE	4

#define	SYM_BITS4_INDEX_SH_LEFT_BIG	0
#define	SYM_BITS4_INDEX_SH_LEFT_LITTLE	12

/* External symbol external record */

struct ext_ext {
	unsigned char	es_bits1[1];
	unsigned char	es_bits2[1];
	unsigned char	es_ifd[2];
	struct	sym_ext es_asym;
};

#define	EXT_BITS1_JMPTBL_BIG		0x80
#define	EXT_BITS1_JMPTBL_LITTLE		0x01

#define	EXT_BITS1_COBOL_MAIN_BIG	0x40
#define	EXT_BITS1_COBOL_MAIN_LITTLE	0x02

#define	EXT_BITS1_WEAKEXT_BIG		0x20
#define	EXT_BITS1_WEAKEXT_LITTLE	0x04

/* Type information external record */

struct tir_ext {
	unsigned char	t_bits1[1];
	unsigned char	t_tq45[1];
	unsigned char	t_tq01[1];
	unsigned char	t_tq23[1];
};

#define	TIR_BITS1_FBITFIELD_BIG		0x80
#define	TIR_BITS1_FBITFIELD_LITTLE	0x01

#define	TIR_BITS1_CONTINUED_BIG		0x40
#define	TIR_BITS1_CONTINUED_LITTLE	0x02

#define	TIR_BITS1_BT_BIG		0x3F
#define	TIR_BITS1_BT_SH_BIG		0
#define	TIR_BITS1_BT_LITTLE		0xFC
#define	TIR_BITS1_BT_SH_LITTLE		2

#define	TIR_BITS_TQ4_BIG		0xF0
#define	TIR_BITS_TQ4_SH_BIG		4
#define	TIR_BITS_TQ5_BIG		0x0F
#define	TIR_BITS_TQ5_SH_BIG		0
#define	TIR_BITS_TQ4_LITTLE		0x0F
#define	TIR_BITS_TQ4_SH_LITTLE		0
#define	TIR_BITS_TQ5_LITTLE		0xF0
#define	TIR_BITS_TQ5_SH_LITTLE		4

#define	TIR_BITS_TQ0_BIG		0xF0
#define	TIR_BITS_TQ0_SH_BIG		4
#define	TIR_BITS_TQ1_BIG		0x0F
#define	TIR_BITS_TQ1_SH_BIG		0
#define	TIR_BITS_TQ0_LITTLE		0x0F
#define	TIR_BITS_TQ0_SH_LITTLE		0
#define	TIR_BITS_TQ1_LITTLE		0xF0
#define	TIR_BITS_TQ1_SH_LITTLE		4

#define	TIR_BITS_TQ2_BIG		0xF0
#define	TIR_BITS_TQ2_SH_BIG		4
#define	TIR_BITS_TQ3_BIG		0x0F
#define	TIR_BITS_TQ3_SH_BIG		0
#define	TIR_BITS_TQ2_LITTLE		0x0F
#define	TIR_BITS_TQ2_SH_LITTLE		0
#define	TIR_BITS_TQ3_LITTLE		0xF0
#define	TIR_BITS_TQ3_SH_LITTLE		4


/* Relative symbol external record */

struct rndx_ext {
	unsigned char	r_bits[4];
};

#define	RNDX_BITS0_RFD_SH_LEFT_BIG	4
#define	RNDX_BITS1_RFD_BIG		0xF0
#define	RNDX_BITS1_RFD_SH_BIG		4

#define	RNDX_BITS0_RFD_SH_LEFT_LITTLE	0
#define	RNDX_BITS1_RFD_LITTLE		0x0F
#define	RNDX_BITS1_RFD_SH_LEFT_LITTLE	8

#define	RNDX_BITS1_INDEX_BIG		0x0F
#define	RNDX_BITS1_INDEX_SH_LEFT_BIG	16
#define	RNDX_BITS2_INDEX_SH_LEFT_BIG	8
#define	RNDX_BITS3_INDEX_SH_LEFT_BIG	0

#define	RNDX_BITS1_INDEX_LITTLE		0xF0
#define	RNDX_BITS1_INDEX_SH_LITTLE	4
#define	RNDX_BITS2_INDEX_SH_LEFT_LITTLE	4
#define	RNDX_BITS3_INDEX_SH_LEFT_LITTLE	12


/* Dense numbers external record */

struct dnr_ext {
	unsigned char	d_rfd[4];
	unsigned char	d_index[4];
};

/* Auxliliary symbol information external record */

union aux_ext {
	struct tir_ext	a_ti;
	struct rndx_ext	a_rndx;
	unsigned char	a_dnLow[4];
	unsigned char	a_dnHigh[4];
	unsigned char	a_isym[4];
	unsigned char	a_iss[4];
	unsigned char	a_width[4];
	unsigned char	a_count[4];
};

/* FIXME!  These are copied from ../bfd/libbfd.h */
PROTO (bfd_vma, _do_getb32, (unsigned char *addr));
PROTO (bfd_vma, _do_getl32, (unsigned char *addr));

#define	AUX_GET_DNLOW(bigend, ax)	(bigend? _do_getb32 ((ax)->a_dnLow): \
						 _do_getl32 ((ax)->a_dnLow))
#define	AUX_GET_DNHIGH(bigend, ax)	(bigend? _do_getb32 ((ax)->a_dnHigh): \
						 _do_getl32 ((ax)->a_dnHigh))
#define	AUX_GET_ISYM(bigend, ax)	(bigend? _do_getb32 ((ax)->a_isym): \
						 _do_getl32 ((ax)->a_isym))
#define	AUX_GET_ISS(bigend, ax)		(bigend? _do_getb32 ((ax)->a_iss): \
						 _do_getl32 ((ax)->a_iss))
#define	AUX_GET_WIDTH(bigend, ax)	(bigend? _do_getb32 ((ax)->a_width): \
						 _do_getl32 ((ax)->a_width))
#define	AUX_GET_COUNT(bigend, ax)	(bigend? _do_getb32 ((ax)->a_count): \
						 _do_getl32 ((ax)->a_count))

/* Relative file descriptor */

struct rfd_ext {
  unsigned char	rfd[4];
};
