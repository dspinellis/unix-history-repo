# ifndef	TOKENS
# define	TOKENS	0
# define	ERROR	1	/* an error node */
# define	FREE	2	/* an unused node */
# define	STRING	3	/* a string constant */
# define	ICON	4	/* an integer constant */
# define	FCON	5	/* a floating point constant */
# define	DCON	6	/* a double precision f.p. constant */
# define	NAME	7	/* an identifier */
# define	REG		8	/* a register */
# define	OREG	9	/* register and offset */
# define	CCODES	10	/* condition codes */
# define	FLD		11	/* a bit field */
# define	PLUS	12	/* + */
# define	PLUSEQ	13	/* += */
# define	UPLUS	14	/* unary + (for completeness) */
# define	MINUS	15	/* - */
# define	MINUSEQ	16	/* -= */
# define	UMINUS	17	/* unary - */
# define	MUL		18	/* * */
# define	MULEQ	19	/* *= */
# define	DIV		21	/* / */
# define	DIVEQ	22	/* /= */
# define	MOD		23	/* % */
# define	MODEQ	24	/* %= */
# define	INCR	25	/* ++ */
# define	DECR	26	/* -- */
# define	ASSIGN	27	/* = (these last 3 are stretching it) */
# define	AND		28	/* & */
# define	ANDEQ	29	/* &= */
# define	OR		31	/* | */
# define	OREQ	32	/* |= */
# define	ER		33	/* ^ */
# define	EREQ	34	/* ^= */
# define	LS		35	/* << */
# define	LSEQ	36	/* <<= */
# define	RS		37	/* >> */
# define	RSEQ	38	/* >>= */
# define	COMPL	39	/* ~ */
# define	EQ		40	/* == */
# define	NE		41	/* != */
# define	LE		42	/* <= */
# define	LT		43	/* < */
# define	GE		44	/* >= */
# define	GT		45	/* > */
# define	ULE		46	/* unsigned <= */
# define	ULT		47	/* unsigned < */
# define	UGE		48	/* unsigned >= */
# define	UGT		49	/* unsigned > */
# define	QUEST	50	/* ? (for conditional expressions) */
# define	COLON	51	/* : (for conditional expressions) */
# define	ANDAND	52	/* && */
# define	OROR	53	/* || */
# define	NOT		54	/* ! */
# define	CALL	55	/* call by value */
# define	UCALL	57	/* call with no arguments */
# define	FORTCALL	58	/* call by reference? */
# define	UFORTCALL	60	/* ??? */
# ifdef INLINE
# define	INLINE	61	/* inline function */
# define	UINLINE	63	/* inline with no arguments */
# endif INLINE
# define	DEREF	20	/* * */
# define	ADDROF	30	/* & */
# define	DOT		64	/* . */
# define	STREF	65	/* -> */
# define	STASG	66	/* structure assignment */
# define	STARG	67	/* an argument of type structure */
# define	STCALL	68	/* a function of type structure */
# define	USTCALL	70	/* unary structure function */
# define	SCONV	71	/* scalar conversion */
# define	PCONV	72	/* pointer conversion */
# define	PMCONV	73	/* pointer multiply conversion */
# define	PVCONV	74	/* pointer divide conversion */
# define	CAST	75	/* redundant? */
# define	LB		76	/* [ */
# define	RB		77	/* ] */
# define	COMOP	78	/* , (in expressions) */
# define	CM		79	/* , (in argument lists) */
# define	FORCE	80	/* result of last expression goes in r0 */
# define	GOTO	81	/* unconditional goto */
# define	CBRANCH	82	/* goto label if !test */
# define	RETURN	83	/* return from function */
# define	INIT	84	/* initialized data */
# define	TYPE	85	/* a type */
# define	CLASS	86	/* a storage class */
# define	MAXOP	86	/* highest numbered PCC op */
# define	FORTOPS	150
# define	FTEXT	150	/* pass literal assembler text */
# define	FEXPR	151	/* a statement */
# define	FSWITCH	152	/* not implemented */
# define	FLBRAC	153	/* beginning of subroutine */
# define	FRBRAC	154	/* end of subroutine */
# define	FEOF	155	/* end of file */
# define	FARIF	156	/* not implemented */
# define	FLABEL	157	/* an f77 label */
# endif	TOKENS
# define	UNDEF	0
# define	FARG	1	/* function argument */
# define	CHAR	2
# define	SHORT	3
# define	INT	4
# define	LONG	5
# define	FLOAT	6
# define	DOUBLE	7
# define	STRTY	8
# define	UNIONTY	9
# define	ENUMTY	10
# define	MOETY	11	/* member of enum */
# define	UCHAR	12
# define	USHORT	13
# define	UNSIGNED	14
# define	ULONG	15
# define	PTR	020
# define	FTN	040
# define	ARY	060
# define	BASETYPE	017
# define	TYPESHIFT	2
# define	ASG	1+
# define	UNARY	2+
# define	NOASG	(-1)+
# define	NOUNARY	(-2)+
#ifndef	LOCALTOKENS
#define	LOCALTOKENS	100
#ifdef	_PASS1_
#define	ASOP	100	/* assignment ops */
#define	RELOP	101	/* <=, <, >=, > */
#define	EQUOP	102	/* ==, != */
#define	DIVOP	103	/* /, % */
#define	SHIFTOP	104	/* <<, >> */
#define	INCOP	105	/* ++, -- */
#define	UNOP	106	/* !, ~ */
#define	STROP	107	/* ., -> */
#define	LP		108	/* ( */
#define	RP		109	/* ) */
#define	LC		110	/* { */
#define	RC		111	/* } */
#endif	_PASS1_
#define	STRUCT	112
#define	IF		113
#define	ELSE	114
#define	SWITCH	115
#define	BREAK	116
#define	CONTINUE	117
#define	WHILE	118
#define	DO		119
#define	FOR		120
#define	DEFAULT	121
#define	CASE	122
#define	SIZEOF	123
#define	ENUM	124
#define	SM		125
#endif	LOCALTOKENS
