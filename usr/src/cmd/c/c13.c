/*
 * C second pass -- tables
 */
#include "c1.h"
/*
 * Operator dope table-- see description in c0.
 */
int opdope[] {
	000000,	/* EOFC */
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
	000000,	/* reverse field selection */
	000001,	/* temporary field selection */
	000001,	/* int->ptr */
	000001,	/* ptr->int */
	000001,	/* long->ptr */
	000001,	/* field assignment */
	000001,	/* >> unsigned */
	000001,	/* >> unsigned */
	000000,	/* 19 */
	000400,	/* name */
	000400,	/* short constant */
	000400,	/* string */
	000400,	/* float */
	000400,	/* double */
	0000400,	/* long const */
	000400,	/* long const <= 16 bits */
	000400,	/* autoi, *r++ */
	000400,	/* autod, *--r */
	000000,	/* 29 */
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
	016161,	/* | */
	016161,	/* ^ */
	036001,	/* -> */
	001000,	/* int -> double */
	001000,	/* double -> int */
	000001,	/* && */
	000001,	/* || */
	030001, /* &~ */
	001000,	/* double -> long */
	001000,	/* long -> double */
	001000,	/* integer -> long */
	000000,	/* long -> integer */
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
	030001, /* & for tests */
	032001,	/*  * (long) */
	032001,	/*  / (long) */
	032001,	/* % (long) */
	012253,	/* =& ~ */
	012213,	/* =* (long) */
	012213,	/* / (long) */
	012213,	/* % (long) */
	000000,	/* 89 */
	014201,	/* ? */
	026061,	/* long << */
	012253,	/* long =<< */
	000101,	/* max */
	000101,	/* maxp */
	000101,	/* min */
	000101,	/* minp */
	000001,	/* , */
	000000,	/* 98 */
	000000,	/* 99 */
	036001,	/* call */
	036000,	/* mcall */
	000000,	/* goto */
	000000,	/* jump cond */
	000000,	/* branch cond */
	000400,	/* set nregs */
	000000, /* 106 */
	000000,	/* 107 */
	000000,	/* 108 */
	000000,	/* int->char */
	000000,	/* force r0 */
	000000,	/* 111 */
	000000,	/* 112 */
	000000,	/* 113 */
	000000,	/* 114 */
	000000,	/* structure assign */
	000001,	/* struct assignment setup */
};

char	*opntab[] {
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	":",
	",",
	"field select",
	0,
	0,
	"int->ptr",
	"ptr->int",
	"long->ptr",
	"field assign",
	">>",
	">>",
	0,
	"name",
	"short constant",
	"string",
	"float",
	"double",
	"long constant",
	"long constant",
	"*r++",
	"*--r",
	0,
	"++pre",
	"--pre",
	"++post",
	"--post",
	"!un",
	"&",
	"*",
	"-",
	"~",
	".",
	"+",
	"-",
	"*",
	"/",
	"%",
	">>",
	"<<",
	"&",
	"|",
	"^",
	"->",
	"int->double",
	"double->int",
	"&&",
	"||",
	"&~",
	"double->long",
	"long->double",
	"integer->long",
	"long->integer",
	"==",
	"!=",
	"<=",
	"<",
	">=",
	">",
	"<p",
	"<=p",
	">p",
	">=p",
	"+=",
	"-=",
	"*=",
	"/=",
	"%=",
	">>=",
	"<<=",
	"&=",
	"|=",
	"^=",
	"=",
	"& for tests",
	"*",
	"/",
	"%",
	"&= ~",
	"*=",
	"/=",
	"%=",
	0,
	"?",
	"<<",
	"<<=",
	"\\/",
	"\\/",
	"/\\",
	"/\\",
	0,
	"call",
	"call",
	"call",
	0,
	"goto",
	"jump cond",
	"branch cond",
	"set nregs",
	"load value",
	0,
	0,
	"int->char",
	"force register",
	"",
	"",
	"",
	"",
	"=structure",
	"= (struct setup)",
};

/*
 * Strings for instruction tables.
 */
char	mov[]	"mov";
char	clr[]	"clr";
char	cmp[]	"cmp";
char	tst[]	"tst";
char	add[]	"add";
char	sub[]	"sub";
char	inc[]	"inc";
char	dec[]	"dec";
char	mul[]	"mul";
char	div[]	"div";
char	asr[]	"asr";
char	ash[]	"ash";
char	asl[]	"asl";
char	bic[]	"bic";
char	bic1[]	"bic $1,";
char	bit[]	"bit";
char	bit1[]	"bit $1,";
char	bis[]	"bis";
char	bis1[]	"bis $1,";
char	xor[]	"xor";
char	neg[]	"neg";
char	com[]	"com";
char	stdol[]	"*$";
char	ashc[]	"ashc";
char	slmul[]	"lmul";
char	sldiv[]	"ldiv";
char	slrem[]	"lrem";
char	almul[]	"almul";
char	aldiv[]	"aldiv";
char	alrem[]	"alrem";
char	jeq[]	"jeq";
char	jne[]	"jne";
char	jle[]	"jle";
char	jgt[]	"jgt";
char	jlt[]	"jlt";
char	jge[]	"jge";
char	jlos[]	"jlos";
char	jhi[]	"jhi";
char	jlo[]	"jlo";
char	jhis[]	"jhis";
char	nop[]	"/nop";
char	jbr[]	"jbr";

/*
 * Instruction tables, accessed by
 * I (first operand) or I' (second) macros.
 */

struct instab instab[] {
	LOAD,	mov,	tst,
	ASSIGN,	mov,	clr,
	EQUAL,	cmp,	tst,
	NEQUAL,	cmp,	tst,
	LESSEQ,	cmp,	tst,
	LESS,	cmp,	tst,
	GREATEQ,cmp,	tst,
	GREAT,	cmp,	tst,
	LESSEQP,cmp,	tst,
	LESSP,	cmp,	tst,
	GREATQP,cmp,	tst,
	GREATP,	cmp,	tst,
	PLUS,	add,	inc,
	ASPLUS,	add,	inc,
	MINUS,	sub,	dec,
	ASMINUS,sub,	dec,
	INCBEF,	add,	inc,
	DECBEF,	sub,	dec,
	INCAFT,	add,	inc,
	DECAFT,	sub,	dec,
	TIMES,	mul,	mul,
	ASTIMES,mul,	mul,
	DIVIDE,	div,	div,
	ASDIV,	div,	div,
	MOD,	div,	div,
	ASMOD,	div,	div,
	PTOI,	div,	div,
	RSHIFT,	ash,	asr,
	ASRSH,	ash,	asr,
	LSHIFT,	ash,	asl,
	ASLSH,	ash,	asl,
	AND,	bic,	bic1,
	ANDN,	bic,	bic1,
	ASANDN,	bic,	bic1,
	TAND,	bit,	bit1,
	OR,	bis,	bis1,
	ASOR,	bis,	bis1,
	EXOR,	xor,	xor,
	ASXOR,	xor,	xor,
	NEG,	neg,	neg,
	COMPL,	com,	com,
	CALL1,	stdol,	stdol,
	CALL2,	"",	"",
	LLSHIFT,ashc,	ashc,
	ASLSHL,	ashc,	ashc,
	LTIMES,	slmul,	slmul,
	LDIV,	sldiv,	sldiv,
	LMOD,	slrem,	slrem,
	LASTIMES,almul,	almul,
	LASDIV,	aldiv,	aldiv,
	LASMOD,	alrem,	alrem,
	ULSH,	ashc,	ashc,
	ASULSH,	ashc,	ashc,
	0,	0,	0};

/*
 * Similar table for relationals.
 * The first string is for the positive
 * test, the second for the inverted one.
 * The '200+' entries are 
 * used in tests against 0 where a 'tst'
 * instruction is used; it clears the c-bit
 * the c-bit so ptr tests are funny.
 */
struct instab branchtab[] {
	EQUAL,	jeq,	jne,
	NEQUAL,	jne,	jeq,
	LESSEQ,	jle,	jgt,
	LESS,	jlt,	jge,
	GREATEQ,jge,	jlt,
	GREAT,	jgt,	jle,
	LESSEQP,jlos,	jhi,
	LESSP,	jlo,	jhis,
	GREATQP,jhis,	jlo,
	GREATP,	jhi,	jlos,
	200+EQUAL,	jeq,	jne,
	200+NEQUAL,	jne,	jeq,
	200+LESSEQ,	jle,	jgt,
	200+LESS,	jlt,	jge,
	200+GREATEQ,	jge,	jlt,
	200+GREAT,	jgt,	jle,
	200+LESSEQP,	jeq,	jne,
	200+LESSP,	nop,	jbr,
	200+GREATQP,	jbr,	nop,
	200+GREATP,	jne,	jeq,
	0,	0,	0 };
