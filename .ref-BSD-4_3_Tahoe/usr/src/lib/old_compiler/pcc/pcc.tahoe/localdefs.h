/*	localdefs.h	1.1	86/02/02	*/

/*
 * Definitions of symbols local to PCC.
 *
 * This file is not included directly by PCC sources -- instead it is
 *	preprocessed along with <pcc.h> to yield two files, pcclocal.h
 *	and pcctokens.  The preprocessing strips off the PCC_ prefix
 *	from all the symbols, since the PCC has no potential clashes...
 */
#ifndef	PCC_LOCALTOKENS

#define	PCC_LOCALTOKENS	100

#ifdef	_PASS1_
/*
 * Leftover operators.
 */
#define	PCC_ASOP	100	/* assignment ops */
#define	PCC_RELOP	101	/* <=, <, >=, > */
#define	PCC_EQUOP	102	/* ==, != */
#define	PCC_DIVOP	103	/* /, % */
#define	PCC_SHIFTOP	104	/* <<, >> */
#define	PCC_INCOP	105	/* ++, -- */
#define	PCC_UNOP	106	/* !, ~ */
#define	PCC_STROP	107	/* ., -> */

#define	PCC_LP		108	/* ( */
#define	PCC_RP		109	/* ) */
#define	PCC_LC		110	/* { */
#define	PCC_RC		111	/* } */
#endif	_PASS1_

/*
 * C keywords.
 */
#define	PCC_STRUCT	112
#define	PCC_IF		113
#define	PCC_ELSE	114
#define	PCC_SWITCH	115
#define	PCC_BREAK	116
#define	PCC_CONTINUE	117
#define	PCC_WHILE	118
#define	PCC_DO		119
#define	PCC_FOR		120
#define	PCC_DEFAULT	121
#define	PCC_CASE	122
#define	PCC_SIZEOF	123
#define	PCC_ENUM	124
#define	PCC_SM		125
#endif	PCC_LOCALTOKENS
