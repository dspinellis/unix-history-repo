#
#include "c0h.c"
/*
 *  info on operators:
 *   01-- is binary operator
 *   02-- left (or only) operand must be lvalue
 *   04-- is relational operator
 *  010-- is assignment-type operator
 *  020-- non-float req. on left
 *  040-- non-float req. on right
 * 0100-- is commutative
 * 0200-- is right, not left-associative
 * 0400-- is leaf of tree
 * *0XX000-- XX is priority of operator
 */
int opdope[] {
	000000,	/* EOF */
	000000,	/* ; */
	000000,	/* { */
	000000,	/* } */
	036000,	/* [ */
	002000,	/* ] */
	036000,	/* ( */
	002000,	/* ) */
	014201,	/* : */
	007001,	/* , */
	000000,	/* field selection */
	000000,	/* 11 */
	000000,	/* 12 */
	000000,	/* 13 */
	000000,	/* 14 */
	000000,	/* 15 */
	000000,	/* 16 */
	000000,	/* 17 */
	000000,	/* 18 */
	000000,	/* 19 */
	000400,	/* name */
	000400,	/* short constant */
	000400,	/* string */
	000400,	/* float */
	000400,	/* double */
	000000,	/* 25 */
	000000,	/* 26 */
	000000,	/* 27 */
	000000,	/* 28 */
	000000, /* 29 */
	034203,	/* ++pre */
	034203,	/* --pre */
	034203,	/* ++post */
	034203,	/* --post */
	034220,	/* !un */
	034202,	/* &un */
	034220,	/* *un */
	034200,	/* -un */
	034220,	/* ~un */
	036001,	/* . (structure reference) */
	030101,	/* + */
	030001,	/* - */
	032101,	/* * */
	032001,	/* / */
	032001,	/* % */
	026061,	/* >> */
	026061,	/* << */
	020161,	/* & */
	017161,	/* | */
	017161,	/* ^ */
	036001,	/* -> */
	000000, /* int -> double */
	000000, /* double -> int */
	016001, /* && */
	015001, /* || */
	030001, /* &~ */
	000000, /* 56 */
	000000, /* 57 */
	000000, /* 58 */
	000000,	/* 59 */
	022005,	/* == */
	022005,	/* != */
	024005,	/* <= */
	024005,	/* < */
	024005,	/* >= */
	024005,	/* > */
	024005,	/* <p */
	024005,	/* <=p */
	024005,	/* >p */
	024005,	/* >=p */
	012213,	/* =+ */
	012213,	/* =- */
	012213,	/* =* */
	012213,	/* =/ */
	012213,	/* =% */
	012253,	/* =>> */
	012253,	/* =<< */
	012253,	/* =& */
	012253,	/* =| */
	012253,	/* =^ */
	012213,	/* = */
	000000,	/* 81 */
	000000,	/* 82 */
	000000,	/* 83 */
	000000,	/* 84 */
	000000,	/* 85 */
	000000,	/* 86 */
	000000,	/* 87 */
	000000,	/* 88 */
	000000,	/* 89 */
	014201,	/* ? */
	034200,	/* sizeof */
	000000,	/* 92 */
	000000,	/* 93 */
	000000,	/* 94 */
	000000,	/* 95 */
	000000,	/* 96 */
	000000,	/* 97 */
	000000,	/* 98 */
	000000,	/* 99 */
	036001,	/* call */
	036001,	/* mcall */
	000000,	/* goto */
	000000,	/* jump cond */
	000000,	/* branch cond */
	000000,	/* 105 */
	000000, /* 106 */
	000000,	/* 107 */
	000000,	/* 108 */
	000000,	/* 109 */
	000000	/* force r0 */
};

/*
 * conversion table:
 * FTI: float (or double) to integer
 * ITF: integer to float
 * ITP: integer to pointer
 * ITL: integer to long
 * LTI: long to integer
 * LTF: long to float
 * FTL: float to long
 * PTI: pointer to integer
 * XX: usually illegal
 * When FTI, LTI, FTL are added in they specify
 * that it is the left operand that should be converted.
 * For + this is done and the conversion is turned back into
 * ITF, ITL, LTF.
 * For = however the left operand can't be converted
 * and the specified conversion is applied to the rhs.
 */
char cvtab[4][4] {
/*		int	double		long		ptr */
/* int */	0,	(FTI<<4)+ITF,	(LTI<<4)+ITL,	(ITP<<4)+ITP,	
/* double */	ITF,	0,		LTF,		XX,
/* long */	ITL,	(FTL<<4)+LTF,	0,		XX,
/* ptr */	ITP,	XX,		XX,		PTI
};

/*
 * character type table
 */
char ctab[] {
	EOF,	INSERT,	UNKN,	UNKN,	UNKN,	UNKN,	UNKN,	UNKN,
	UNKN,	SPACE,	NEWLN,	UNKN,	UNKN,	UNKN,	UNKN,	UNKN,
	UNKN,	UNKN,	UNKN,	UNKN,	UNKN,	UNKN,	UNKN,	UNKN,
	UNKN,	UNKN,	UNKN,	UNKN,	UNKN,	UNKN,	UNKN,	UNKN,
	SPACE,	EXCLA,	DQUOTE,	UNKN,	UNKN,	MOD,	 AND,	SQUOTE,
	LPARN,	RPARN,	TIMES,	PLUS,	COMMA,	MINUS,	PERIOD,	DIVIDE,
	DIGIT,	DIGIT,	DIGIT,	DIGIT,	DIGIT,	DIGIT,	DIGIT,	DIGIT,
	DIGIT,	DIGIT,	COLON,	SEMI,	LESS,	ASSIGN,	GREAT,	QUEST,
	UNKN,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,
	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,
	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,
	LETTER,	LETTER,	LETTER,	LBRACK,	UNKN,	RBRACK,	EXOR,	LETTER,
	UNKN,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,
	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,
	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,	LETTER,
	LETTER,	LETTER,	LETTER,	LBRACE,	OR,	RBRACE,	COMPL,	UNKN
};
