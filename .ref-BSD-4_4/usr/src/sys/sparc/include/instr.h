/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)instr.h	8.1 (Berkeley) 6/11/93
 *
 * from: $Header: instr.h,v 1.6 92/11/26 02:04:37 torek Exp $
 */

/* see also Appendix F of the SPARC version 8 document */
enum IOP { IOP_OP2, IOP_CALL, IOP_reg, IOP_mem };
enum IOP2 { IOP2_UNIMP, IOP2_err1, IOP2_Bicc, IOP2_err3,
	IOP2_SETHI, IOP2_err5, IOP2_FBfcc, IOP2_CBccc };
enum IOP3_reg {
	IOP3_ADD, IOP3_AND, IOP3_OR, IOP3_XOR,
	IOP3_SUB, IOP3_ANDN, IOP3_ORN, IOP3_XNOR,
	IOP3_ADDX, IOP3_rerr09, IOP3_UMUL, IOP3_SMUL,
	IOP3_SUBX, IOP3_rerr0d, IOP3_UDIV, IOP3_SDIV,
	IOP3_ADDcc, IOP3_ANDcc, IOP3_ORcc, IOP3_XORcc,
	IOP3_SUBcc, IOP3_ANDNcc, IOP3_ORNcc, IOP3_XNORcc,
	IOP3_ADDXcc, IOP3_rerr19, IOP3_UMULcc, IOP3_SMULcc,
	IOP3_SUBXcc, IOP3_rerr1d, IOP3_UDIVcc, IOP3_SDIVcc,
	IOP3_TADDcc, IOP3_TSUBcc, IOP3_TADDccTV, IOP3_TSUBccTV,
	IOP3_MULScc, IOP3_SLL, IOP3_SRL, IOP3_SRA,
	IOP3_RDASR_RDY_STBAR, IOP3_RDPSR, IOP3_RDWIM, IOP3_RDTGBR,
	IOP3_rerr2c, IOP3_rerr2d, IOP3_rerr2e, IOP3_rerr2f,
	IOP3_WRASR_WRY, IOP3_WRPSR, IOP3_WRWIM, IOP3_WRTBR,
	IOP3_FPop1, IOP3_FPop2, IOP3_CPop1, IOP3_CPop2,
	IOP3_JMPL, IOP3_RETT, IOP3_Ticc, IOP3_FLUSH,
	IOP3_SAVE, IOP3_RESTORE, IOP3_rerr3e, IOP3_rerr3f
};
enum IOP3_mem {
	IOP3_LD, IOP3_LDUB, IOP3_LDUH, IOP3_LDD,
	IOP3_ST, IOP3_STB, IOP3_STH, IOP3_STD,
	IOP3_merr08, IOP3_LDSB, IOP3_LDSH, IOP3_merr0b,
	IOP3_merr0c, IOP3_LDSTUB, IOP3_merr0f, IOP3_SWAP,
	IOP3_LDA, IOP3_LDUBA, IOP3_LDUHA, IOP3_LDDA,
	IOP3_STA, IOP3_STBA, IOP3_STHA, IOP3_STDA,
	IOP3_merr18, IOP3_LDSBA, IOP3_LDSHA, IOP3_merr1b,
	IOP3_merr1c, IOP3_LDSTUBA, IOP3_merr1f, IOP3_SWAPA,
	IOP3_LDF, IOP3_LDFSR, IOP3_merr22, IOP3_LDDF,
	IOP3_STF, IOP3_STFSR, IOP3_STDFQ, IOP3_STDF,
	IOP3_merr28, IOP3_merr29, IOP3_merr2a, IOP3_merr2b,
	IOP3_merr2c, IOP3_merr2d, IOP3_merr2e, IOP3_merr2f,
	IOP3_LFC, IOP3_LDCSR, IOP3_merr32, IOP3_LDDC,
	IOP3_STC, IOP3_STCSR, IOP3_STDCQ, IOP3_STDC,
	IOP3_merr38, IOP3_merr39, IOP3_merr3a, IOP3_merr3b,
	IOP3_merr3c, IOP3_merr3d, IOP3_merr3e, IOP3_merr3f
};

/*
 * Integer condition codes.
 */
#define	Icc_N	0x0		/* never */
#define	Icc_E	0x1		/* equal (equiv. zero) */
#define	Icc_LE	0x2		/* less or equal */
#define	Icc_L	0x3		/* less */
#define	Icc_LEU	0x4		/* less or equal unsigned */
#define	Icc_CS	0x5		/* carry set (equiv. less unsigned) */
#define	Icc_NEG	0x6		/* negative */
#define	Icc_VS	0x7		/* overflow set */
#define	Icc_A	0x8		/* always */
#define	Icc_NE	0x9		/* not equal (equiv. not zero) */
#define	Icc_G	0xa		/* greater */
#define	Icc_GE	0xb		/* greater or equal */
#define	Icc_GU	0xc		/* greater unsigned */
#define	Icc_CC	0xd		/* carry clear (equiv. gtr or eq unsigned) */
#define	Icc_POS	0xe		/* positive */
#define	Icc_VC	0xf		/* overflow clear */

/*
 * Integer registers.
 */
#define	I_G0	0
#define	I_G1	1
#define	I_G2	2
#define	I_G3	3
#define	I_G4	4
#define	I_G5	5
#define	I_G6	6
#define	I_G7	7
#define	I_O0	8
#define	I_O1	9
#define	I_O2	10
#define	I_O3	11
#define	I_O4	12
#define	I_O5	13
#define	I_O6	14
#define	I_O7	15
#define	I_L0	16
#define	I_L1	17
#define	I_L2	18
#define	I_L3	19
#define	I_L4	20
#define	I_L5	21
#define	I_L6	22
#define	I_L7	23
#define	I_I0	24
#define	I_I1	25
#define	I_I2	26
#define	I_I3	27
#define	I_I4	28
#define	I_I5	29
#define	I_I6	30
#define	I_I7	31

/*
 * An instruction.
 */
union instr {
	int	i_int;			/* as a whole */

	/*
	 * The first level of decoding is to use the top 2 bits.
	 * This gives us one of three `formats', which usually give
	 * a second level of decoding.
	 */
	struct {
		u_int	i_op:2;		/* first-level decode */
		u_int	:30;
	} i_any;

	/*
	 * Format 1 instructions: CALL (undifferentiated).
	 */
	struct {
		u_int	:2;		/* 01 */
		int	i_disp:30;	/* displacement */
	} i_call;

	/*
	 * Format 2 instructions (SETHI, UNIMP, and branches, plus illegal
	 * unused codes).
	 */
	struct {
		u_int	:2;		/* 00 */
		u_int	:5;
		u_int	i_op2:3;	/* second-level decode */
		u_int	:22;
	} i_op2;

	/* UNIMP, SETHI */
	struct {
		u_int	:2;		/* 00 */
		u_int	i_rd:5;		/* destination register */
		u_int	i_op2:3;	/* opcode: UNIMP or SETHI */
		u_int	i_imm:22;	/* immediate value */
	} i_imm22;

	/* branches: Bicc, FBfcc, CBccc */
	struct {
		u_int	:2;		/* 00 */
		u_int	i_annul:1;	/* annul bit */
		u_int	i_cond:4;	/* condition codes */
		u_int	i_op2:3;	/* opcode: {Bi,FBf,CBc}cc */
		int	i_disp:22;	/* branch displacement */
	} i_branch;

	/*
	 * Format 3 instructions (memory reference; arithmetic, logical,
	 * shift, and other miscellaneous operations).  The second-level
	 * decode almost always makes use of an `rd' and `rs1', however
	 * (see also IOP3_reg and IOP3_mem).
	 *
	 * Beyond that, the low 14 bits may be broken up in one of three
	 * different ways, if at all:
	 *	1 bit of imm=0 + 8 bits of asi + 5 bits of rs2 [reg & mem]
	 *	1 bit of imm=1 + 13 bits of signed immediate [reg & mem]
	 *	9 bits of copressor `opf' opcode + 5 bits of rs2 [reg only]
	 */
	struct {
		u_int	:2;		/* 10 or 11 */
		u_int	i_rd:5;		/* destination register */
		u_int	i_op3:6;	/* second-level decode */
		u_int	i_rs1:5;	/* source register 1 */
		u_int	i_low14:14;	/* varies */
	} i_op3;

	/*
	 * Memory forms.  These set i_op=3 and use simm13 or asi layout.
	 * Memory references without an ASI should use 0, but the actual
	 * ASI field is simply ignored.
	 */
	struct {
		u_int	:2;		/* 11 only */
		u_int	i_rd:5;		/* destination register */
		u_int	i_op3:6;	/* second-level decode (see IOP3_mem) */
		u_int	i_i:1;		/* immediate vs asi */
		u_int	i_low13:13;	/* depend on i bit */
	} i_loadstore;

	/*
	 * Memory and register forms.
	 * These come in quite a variety and we do not
	 * attempt to break them down much.
	 */
	struct {
		u_int	:2;		/* 10 or 11 */
		u_int	i_rd:5;		/* destination register */
		u_int	i_op3:6;	/* second-level decode */
		u_int	i_rs1:5;	/* source register 1 */
		u_int	i_i:1;		/* immediate bit (1) */
		int	i_simm13:13;	/* signed immediate */
	} i_simm13;
	struct {
		u_int	:2;		/* 10 or 11 */
		u_int	i_rd:5;		/* destination register */
		u_int	i_op3:6;	/* second-level decode */
		u_int	i_rs1:5;	/* source register 1 */
		u_int	i_asi:8;	/* asi */
		u_int	i_rs2:5;	/* source register 2 */
	} i_asi;
	struct {
		u_int	:2;		/* 10 only (register, no memory) */
		u_int	i_rd:5;		/* destination register */
		u_int	i_op3:6;	/* second-level decode (see IOP3_reg) */
		u_int	i_rs1:5;	/* source register 1 */
		u_int	i_opf:9;	/* coprocessor 3rd-level decode */
		u_int	i_rs2:5;	/* source register 2 */
	} i_opf;

};

/*
 * Internal macros for building instructions.  These correspond 1-to-1 to
 * the names above.  Note that x << y | z == (x << y) | z.
 */
#define	_I_ANY(op, b)	((op) << 30 | (b))

#define	_I_OP2(high, op2, low) \
		_I_ANY(IOP_OP2, (high) << 25 | (op2) << 22 | (low))
#define	_I_IMM22(rd, op2, imm) \
		_I_ANY(IOP_OP2, (rd) << 25 | (op2) << 22 | (imm))
#define	_I_BRANCH(a, c, op2, disp) \
		_I_ANY(IOP_OP2, (a) << 29 | (c) << 25 | (op2) << 22 | (disp))
#define	_I_FBFCC(a, cond, disp) \
		_I_BRANCH(a, cond, IOP2_FBfcc, disp)
#define	_I_CBCCC(a, cond, disp) \
		_I_BRANCH(a, cond, IOP2_CBccc, disp)

#define	_I_SIMM(simm)		(1 << 13 | ((simm) & 0x1fff))

#define	_I_OP3_GEN(form, rd, op3, rs1, low14) \
		_I_ANY(form, (rd) << 25 | (op3) << 19 | (rs1) << 14 | (low14))
#define	_I_OP3_LS_RAR(rd, op3, rs1, asi, rs2) \
		_I_OP3_GEN(IOP_mem, rd, op3, rs1, (asi) << 5 | (rs2))
#define	_I_OP3_LS_RI(rd, op3, rs1, simm13) \
		_I_OP3_GEN(IOP_mem, rd, op3, rs1, _I_SIMM(simm13))
#define	_I_OP3_LS_RR(rd, op3, rs1, rs2) \
		_I_OP3_GEN(IOP_mem, rd, op3, rs1, rs2)
#define	_I_OP3_R_RAR(rd, op3, rs1, asi, rs2) \
		_I_OP3_GEN(IOP_reg, rd, op3, rs1, (asi) << 5 | (rs2))
#define	_I_OP3_R_RI(rd, op3, rs1, simm13) \
		_I_OP3_GEN(IOP_reg, rd, op3, rs1, _I_SIMM(simm13))
#define	_I_OP3_R_RR(rd, op3, rs1, rs2) \
		_I_OP3_GEN(IOP_reg, rd, op3, rs1, rs2)

#define	I_CALL(d)		_I_ANY(IOP_CALL, d)
#define	I_UNIMP(v)		_I_IMM22(0, IOP2_UNIMP, v)
#define	I_BN(a, d)		_I_BRANCH(a, Icc_N, IOP2_Bicc, d)
#define	I_BE(a, d)		_I_BRANCH(a, Icc_E, IOP2_Bicc, d)
#define	I_BZ(a, d)		_I_BRANCH(a, Icc_E, IOP2_Bicc, d)
#define	I_BLE(a, d)		_I_BRANCH(a, Icc_LE, IOP2_Bicc, d)
#define	I_BL(a, d)		_I_BRANCH(a, Icc_L, IOP2_Bicc, d)
#define	I_BLEU(a, d)		_I_BRANCH(a, Icc_LEU, IOP2_Bicc, d)
#define	I_BCS(a, d)		_I_BRANCH(a, Icc_CS, IOP2_Bicc, d)
#define	I_BLU(a, d)		_I_BRANCH(a, Icc_CS, IOP2_Bicc, d)
#define	I_BNEG(a, d)		_I_BRANCH(a, Icc_NEG, IOP2_Bicc, d)
#define	I_BVS(a, d)		_I_BRANCH(a, Icc_VS, IOP2_Bicc, d)
#define	I_BA(a, d)		_I_BRANCH(a, Icc_A, IOP2_Bicc, d)
#define	I_B(a, d)		_I_BRANCH(a, Icc_A, IOP2_Bicc, d)
#define	I_BNE(a, d)		_I_BRANCH(a, Icc_NE, IOP2_Bicc, d)
#define	I_BNZ(a, d)		_I_BRANCH(a, Icc_NE, IOP2_Bicc, d)
#define	I_BG(a, d)		_I_BRANCH(a, Icc_G, IOP2_Bicc, d)
#define	I_BGE(a, d)		_I_BRANCH(a, Icc_GE, IOP2_Bicc, d)
#define	I_BGU(a, d)		_I_BRANCH(a, Icc_GU, IOP2_Bicc, d)
#define	I_BCC(a, d)		_I_BRANCH(a, Icc_CC, IOP2_Bicc, d)
#define	I_BGEU(a, d)		_I_BRANCH(a, Icc_CC, IOP2_Bicc, d)
#define	I_BPOS(a, d)		_I_BRANCH(a, Icc_POS, IOP2_Bicc, d)
#define	I_BVC(a, d)		_I_BRANCH(a, Icc_VC, IOP2_Bicc, d)
#define	I_SETHI(r, v)		_I_IMM22(r, 4, v)

#define	I_ORri(rd, rs1, imm)	_I_OP3_R_RI(rd, IOP3_OR, rs1, imm)
#define	I_ORrr(rd, rs1, rs2)	_I_OP3_R_RR(rd, IOP3_OR, rs1, rs2)

#define	I_MOVi(rd, imm)		_I_OP3_R_RI(rd, IOP3_OR, I_G0, imm)
#define	I_MOVr(rd, rs)		_I_OP3_R_RR(rd, IOP3_OR, I_G0, rs)

#define	I_RDPSR(rd)		_I_OP3_R_RR(rd, IOP3_RDPSR, 0, 0)

#define	I_JMPLri(rd, rs1, imm)	_I_OP3_R_RI(rd, IOP3_JMPL, rs1, imm)
#define	I_JMPLrr(rd, rs1, rs2)	_I_OP3_R_RR(rd, IOP3_JMPL, rs1, rs2)

/*
 * (Since these are sparse, we skip the enumerations for now.)
 * FPop values.  All appear in both FPop1 and FPop2 spaces, but arithmetic
 * ops should happen only with FPop1 and comparison only with FPop2.
 * The type sits in the low two bits; those bits are given as zero here.
 */
#define	FMOV	0x00
#define	FNEG	0x04
#define	FABS	0x08
#define	FSQRT	0x28
#define	FADD	0x40
#define	FSUB	0x44
#define	FMUL	0x48
#define	FDIV	0x4c
#define	FCMP	0x50
#define	FCMPE	0x54
#define	FSMULD	0x68
#define	FDMULX	0x6c
#define	FTOS	0xc4
#define	FTOD	0xc8
#define	FTOX	0xcc
#define	FTOI	0xd0

/*
 * FPU data types.
 */
#define	FTYPE_INT	0	/* data = 32-bit signed integer */
#define	FTYPE_SNG	1	/* data = 32-bit float */
#define	FTYPE_DBL	2	/* data = 64-bit double */
#define	FTYPE_EXT	3	/* data = 128-bit extended (quad-prec) */
