/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)shtype.h	1.1 */

/*
 *	UNIX shell
 *
 *	S. R. Bourne
 *	AT&T Bell Laboratories
 *
 */

#define const

/* table 1 */
#define QUOTE	(~0177)
#define T_SUB	01
#define T_MET	02
#define	T_SPC	04
#define T_DIP	010
#define T_EXP	020	/* characters which require expansion or substitution */
#define T_EOR	040
#define T_QOT	0100
#define T_ESC	0200

/* table 2 */
#define T_ALP	01	/* alpha, but not upper or lower */
#define T_DEF	02
#define T_AST	04	/* * or @ */
#define	T_DIG	010	/* digit */
#define T_UPC	020	/* uppercase only */
#define T_SHN	040	/* legal parameter characters */
#define	T_LPC	0100	/* lowercase only */
#define T_SET	0200

/* for single chars */
#define _TAB	(T_SPC)
#define _SPC	(T_SPC)
#define _ALP	(T_ALP)
#define _UPC	(T_UPC)
#define _LPC	(T_LPC)
#define _DIG	(T_DIG)
#define _EOF	(T_EOR)
#define _EOR	(T_EOR)
#define _BAR	(T_DIP)
#define _BRA	(T_MET|T_DIP)
#define _KET	(T_MET)
#define _AMP	(T_DIP)
#define _SEM	(T_DIP)
#define _LT	(T_DIP)
#define _GT	(T_DIP)
#define _LQU	(T_QOT|T_ESC|T_EXP)
#define _QU1	T_EXP
#define _AST1	T_EXP
#define _BSL	(T_ESC)
#define _DQU	(T_QOT)
#define _DOL1	(T_SUB|T_ESC|T_EXP)

#define _CBR	T_SHN
#define _CKT	T_DEF
#define _AST	(T_AST)
#define _EQ	(T_DEF)
#define _MIN	(T_DEF|T_SHN)
#define _PCS	(T_DEF|T_SHN|T_SET)
#define _NUM	(T_DEF|T_SHN|T_SET)
#define _DOL2	(T_SHN)
#define _PLS	(T_DEF|T_SET)
#define _AT	(T_AST)
#define _QU	(T_DEF|T_SHN)
#define _LPAR	T_SHN
#define _SS2	T_ALP
#define _SS3	T_ALP

/* abbreviations for tests */
#define _IDCH	(T_UPC|T_LPC|T_DIG|T_ALP)
#define _META	(T_SPC|T_DIP|T_MET|T_EOR)

extern char	_ctype1[];

/* these args are not call by value !!!! */
#define	isspace(c)	(_ctype1[c]&(T_SPC))
#define ismeta(c)	(_ctype1[c]&(_META))
#define isqmeta(c)	(_ctype1[c]&(_META|T_QOT))
#define qotchar(c)	(_ctype1[c]&(T_QOT))
#define eolchar(c)	(_ctype1[c]&(T_EOR))
#define dipchar(c)	(_ctype1[c]&(T_DIP))
#define subchar(c)	(_ctype1[c]&(T_SUB|T_QOT))
#define escchar(c)	(_ctype1[c]&(T_ESC))
#define expchar(c)	(_ctype1[c]&(T_EXP|T_SPC))
#define isexp(c)	(_ctype1[c]&T_EXP)

extern char	_ctype2[];

#define	isprint(c)	(((c)&0340) && ((c)!=0177))
#define	isdigit(c)	(_ctype2[c]&(T_DIG))
#define dolchar(c)	(_ctype2[c]&(T_AST|_IDCH|T_SHN))
#define defchar(c)	(_ctype2[c]&(T_DEF))
#define setchar(c)	(_ctype2[c]&(T_SET))
#define digchar(c)	(_ctype2[c]&(T_AST|T_DIG))
#define	isalpha(c)	(_ctype2[c]&(T_UPC|T_LPC|T_ALP))
#define isalnum(c)	(_ctype2[c]&(_IDCH))
#define isupper(c)	(_ctype2[c]&(T_UPC))
#define islower(c)	(_ctype2[c]&(T_LPC))
#define astchar(c)	(_ctype2[c]&(T_AST))
#define toupper(c)	((c) + 'A' - 'a')
#define tolower(c)	((c) + 'a' - 'A')
