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
	000000,	/* 10 */
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
	034200,	/* sizeof */
	034202,	/* ++pre */
	034202,	/* --pre */
	034202,	/* ++post */
	034202,	/* --post */
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
	000000, /* 55 */
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
	000000,	/* 91 */
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
 * 0100-- convert left operand
 * 0*0XX-- XX is conversion number, to wit:
 *  000: none
 *  001: int -> ptr
 *  002: ptr -> int
 *  003: int -> double
 *  004: double -> int
 *  077: generally illegal
 */
char cvtab[] {
	0000,	/* i : i */
	0000,	/* i : c */
	0103,	/* i : f */
	0103,	/* i : d */
	0077,	/* i : s */
	0101,	/* i : *i */
	0000,	/* i : *c */
	0101,	/* i : *f */
	0101,	/* i : *d */
	0101,	/* i : *s */
	0101,	/* i : ** */

	0000,	/* c : i */
	0000,	/* c : c */
	0103,	/* c : f */
	0103,	/* c : d */
	0077,	/* c : s */
	0101,	/* c : *i */
	0000,	/* c : *c */
	0101,	/* c : *f */
	0101,	/* c : *d */
	0101,	/* c : *s */
	0101,	/* c : ** */

	0003,	/* f : i */
	0003,	/* f : c */
	0000,	/* f : f */
	0000,	/* f : d */
	0077,	/* f : s */
	0077,	/* f : *i */
	0077,	/* f : *c */
	0077,	/* f : *f */
	0077,	/* f : *d */
	0077,	/* f : *s */
	0077,	/* f : ** */

	0003,	/* d : i */
	0003,	/* d : c */
	0000,	/* d : f */
	0000,	/* d : d */
	0077,	/* d : s */
	0077,	/* d : *i */
	0077,	/* d : *c */
	0077,	/* d : *f */
	0077,	/* d : *d */
	0077,	/* d : *s */
	0077,	/* d : ** */

	0077,	/* s : i */
	0077,	/* s : c */
	0077,	/* s : f */
	0077,	/* s : d */
	0077,	/* s : s */
	0077,	/* s : *i */
	0077,	/* s : *c */
	0077,	/* s : *f */
	0077,	/* s : *d */
	0077,	/* s : *s */
	0077,	/* s : ** */

	0001,	/* *i : i */
	0001,	/* *i : c */
	0077,	/* *i : f */
	0077,	/* *i : d */
	0077,	/* *i : s */
	0002,	/* *i : *i */
	0077,	/* *i : *c */
	0077,	/* *i : *f */
	0077,	/* *i : *d */
	0077,	/* *i : *s */
	0002,	/* *i : ** */

	0000,	/* *c : i */
	0000,	/* *c : c */
	0077,	/* *c : f */
	0077,	/* *c : d */
	0077,	/* *c : s */
	0077,	/* *c : *i */
	0000,	/* *c : *c */
	0077,	/* *c : *f */
	0077,	/* *c : *d */
	0077,	/* *c : *s */
	0077,	/* *c : ** */

	0001,	/* *f : i */
	0001,	/* *f : c */
	0077,	/* *f : f */
	0077,	/* *f : d */
	0077,	/* *f : s */
	0077,	/* *f : *i */
	0077,	/* *f : *c */
	0002,	/* *f : *f */
	0077,	/* *f : *d */
	0077,	/* *f : *s */
	0077,	/* *f : ** */

	0001,	/* *d : i */
	0001,	/* *d : c */
	0077,	/* *d : f */
	0077,	/* *d : d */
	0077,	/* *d : s */
	0077,	/* *d : *i */
	0077,	/* *d : *c */
	0077,	/* *d : *f */
	0002,	/* *d : *d */
	0077,	/* *d : *s */
	0077,	/* *d : ** */

	0001,	/* *s : i */
	0001,	/* *s : c */
	0077,	/* *s : f */
	0077,	/* *s : d */
	0077,	/* *s : s */
	0077,	/* *s : *i */
	0077,	/* *s : *c */
	0077,	/* *s : *f */
	0077,	/* *s : *d */
	0002,	/* *s : *s */
	0077,	/* *s : ** */

	0001,	/* ** : i */
	0001,	/* ** : c */
	0077,	/* ** : f */
	0077,	/* ** : d */
	0077,	/* ** : s */
	0002,	/* ** : *i */
	0077,	/* ** : *c */
	0077,	/* ** : *f */
	0077,	/* ** : *d */
	0077,	/* ** : *s */
	0002	/* ** : ** */
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
