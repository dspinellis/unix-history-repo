/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)pcc.h	5.1 (Berkeley) 5/30/85
 */

/*
 * This file contains definitions for all the constants and structures
 *	needed to use the intermediate code files generated and read by
 *	the Portable C Compiler and related compilers.
 *
 * Rules for changing this code:
 *   1)	All op values must be integer constants -- this permits us to run
 *	a 'sed' script on this file to create %term declarations for yacc.
 *   2)	Because the PCC uses fancy ASG and UNARY macros, assignment
 *	operators must have values 1 greater than corresponding normal
 *	operators, and unary operators must have values 2 greater ditto.
 *   3) Ops used only by f1 must have values >= 150 (PCCF_FORTOPS).
 *   4)	Other language-dependent ops must have values >= 200.
 */

# ifndef	PCC_TOKENS

# define	PCC_TOKENS	0

# define	PCC_ERROR	1	/* an error node */
# define	PCC_FREE	2	/* an unused node */

/*
 * Constants.
 */
# define	PCC_STRING	3	/* a string constant */
# define	PCC_ICON	4	/* an integer constant */
# define	PCC_FCON	5	/* a floating point constant */
# define	PCC_DCON	6	/* a double precision f.p. constant */

/*
 * Leaf types.
 */
# define	PCC_NAME	7	/* an identifier */
# define	PCC_REG		8	/* a register */
# define	PCC_OREG	9	/* register and offset */
# define	PCC_CCODES	10	/* condition codes */
# define	PCC_FLD		11	/* a bit field */

/*
 * Arithmetic operators.
 */
# define	PCC_PLUS	12	/* + */
# define	PCC_PLUSEQ	13	/* += */
# define	PCC_UPLUS	14	/* unary + (for completeness) */
# define	PCC_MINUS	15	/* - */
# define	PCC_MINUSEQ	16	/* -= */
# define	PCC_UMINUS	17	/* unary - */
# define	PCC_MUL		18	/* * */
# define	PCC_MULEQ	19	/* *= */
/* Reserve a slot for 'unary *', which is PCC jargon for PCC_DEREF (yech) */
# define	PCC_DIV		21	/* / */
# define	PCC_DIVEQ	22	/* /= */
# define	PCC_MOD		23	/* % */
# define	PCC_MODEQ	24	/* %= */
# define	PCC_INCR	25	/* ++ */
# define	PCC_DECR	26	/* -- */
# define	PCC_ASSIGN	27	/* = (these last 3 are stretching it) */

/*
 * Bit operators.
 */
# define	PCC_AND		28	/* & */
# define	PCC_ANDEQ	29	/* &= */
/* Reserve a slot for 'unary &', jargon for PCC_ADDROF */
# define	PCC_OR		31	/* | */
# define	PCC_OREQ	32	/* |= */
# define	PCC_ER		33	/* ^ */
# define	PCC_EREQ	34	/* ^= */
# define	PCC_LS		35	/* << */
# define	PCC_LSEQ	36	/* <<= */
# define	PCC_RS		37	/* >> */
# define	PCC_RSEQ	38	/* >>= */
# define	PCC_COMPL	39	/* ~ */

/*
 * Booleans.
 */
# define	PCC_EQ		40	/* == */
# define	PCC_NE		41	/* != */
# define	PCC_LE		42	/* <= */
# define	PCC_LT		43	/* < */
# define	PCC_GE		44	/* >= */
# define	PCC_GT		45	/* > */
# define	PCC_ULE		46	/* unsigned <= */
# define	PCC_ULT		47	/* unsigned < */
# define	PCC_UGE		48	/* unsigned >= */
# define	PCC_UGT		49	/* unsigned > */
# define	PCC_QUEST	50	/* ? (for conditional expressions) */
# define	PCC_COLON	51	/* : (for conditional expressions) */
# define	PCC_ANDAND	52	/* && */
# define	PCC_OROR	53	/* || */
# define	PCC_NOT		54	/* ! */

/*
 * Function calls.
 */
# define	PCC_CALL	55	/* call by value */
/* no ASG */
# define	PCC_UCALL	57	/* call with no arguments */
# define	PCC_FORTCALL	58	/* call by reference? */
/* no ASG */
# define	PCC_UFORTCALL	60	/* ??? */
# ifdef INLINE
# define	PCC_INLINE	61	/* inline function */
/* no ASG */
# define	PCC_UINLINE	63	/* inline with no arguments */
# endif INLINE

/*
 * Referencing and dereferencing.
 */
# define	PCC_DEREF	20	/* * */
# define	PCC_ADDROF	30	/* & */

/*
 * Special structure operators.
 */
# define	PCC_DOT		64	/* . */
# define	PCC_STREF	65	/* -> */
# define	PCC_STASG	66	/* structure assignment */
# define	PCC_STARG	67	/* an argument of type structure */
# define	PCC_STCALL	68	/* a function of type structure */
/* no ASG */
# define	PCC_USTCALL	70	/* unary structure function */

/*
 * Conversions.
 */
# define	PCC_SCONV	71	/* scalar conversion */
# define	PCC_PCONV	72	/* pointer conversion */
# define	PCC_PMCONV	73	/* pointer multiply conversion */
# define	PCC_PVCONV	74	/* pointer divide conversion */
# define	PCC_CAST	75	/* redundant? */

/*
 * Bracket types.
 */
# define	PCC_LB		76	/* [ */
# define	PCC_RB		77	/* ] */

/*
 * Comma nodes.
 */
# define	PCC_COMOP	78	/* , (in expressions) */
# define	PCC_CM		79	/* , (in argument lists) */

/*
 * Miscellaneous.
 */
# define	PCC_FORCE	80	/* result of last expression goes in r0 */
# define	PCC_GOTO	81	/* unconditional goto */
# define	PCC_CBRANCH	82	/* goto label if !test */
# define	PCC_RETURN	83	/* return from function */
# define	PCC_INIT	84	/* initialized data */
# define	PCC_TYPE	85	/* a type */
# define	PCC_CLASS	86	/* a storage class */

# define	PCC_MAXOP	86	/* highest numbered PCC op */

/*
 * Special codes for interfacing to /lib/f1.
 */
# define	PCCF_FORTOPS	150
# define	PCCF_FTEXT	150	/* pass literal assembler text */
# define	PCCF_FEXPR	151	/* a statement */
# define	PCCF_FSWITCH	152	/* not implemented */
# define	PCCF_FLBRAC	153	/* beginning of subroutine */
# define	PCCF_FRBRAC	154	/* end of subroutine */
# define	PCCF_FEOF	155	/* end of file */
# define	PCCF_FARIF	156	/* not implemented */
# define	PCCF_FLABEL	157	/* an f77 label */

# endif	PCC_TOKENS


/*
 * Types, as encoded in intermediate file cookies.
 */
# define	PCCT_UNDEF	0
# define	PCCT_FARG	1	/* function argument */
# define	PCCT_CHAR	2
# define	PCCT_SHORT	3
# define	PCCT_INT	4
# define	PCCT_LONG	5
# define	PCCT_FLOAT	6
# define	PCCT_DOUBLE	7
# define	PCCT_STRTY	8
# define	PCCT_UNIONTY	9
# define	PCCT_ENUMTY	10
# define	PCCT_MOETY	11	/* member of enum */
# define	PCCT_UCHAR	12
# define	PCCT_USHORT	13
# define	PCCT_UNSIGNED	14
# define	PCCT_ULONG	15

/*
 * Type modifiers.
 */
# define	PCCTM_PTR	020
# define	PCCTM_FTN	040
# define	PCCTM_ARY	060
# define	PCCTM_BASETYPE	017
# define	PCCTM_TYPESHIFT	2


/*
 * Useful macros.  'PCCOM' macros apply to ops.
 */
# define	PCCOM_ASG	1+
# define	PCCOM_UNARY	2+
# define	PCCOM_NOASG	(-1)+
# define	PCCOM_NOUNARY	(-2)+

# define	PCCM_TRIPLE(op, var, type)	\
	((op) | ((var) << 8) | (long) (type) << 16)
# define	PCCM_TEXT(s)			\
	PCCM_TRIPLE(PCCF_FTEXT, (strlen(s) + 3) / 4, 0)
# define	PCCM_ADDTYPE(t, m)		\
	((((t) &~ PCCTM_BASETYPE) << PCCTM_TYPESHIFT) | \
	(m) | ((t) & PCCTM_BASETYPE))
