/* Copyright (c) 1980 Regents of the University of California */
/* "@(#)astoks.H 4.1 %G%" */
/*
 *	Message to the wary:  The order of tokens has been
 *	hand optimized and assigned to that all switch statements
 *	are implemented by a casel instruction on the VAX. 
 *	there are 4 switch statements that have to be worried about:
 *	l)	Per character switch in the character scanner (scan_dot_s)
 *	2)	Per token switch in the buffer manager (yylex)
 *	3)	Per keyword switch in the parser (yyparse)
 *	4)	Leading token switch for argments to opcodes
 *	
 *	You can't just add new tokens willy-nilly; make sure that you
 *	add them into the proper order!
 */
# define FIRSTTOKEN	AWKFIXESME

/*
 *	Tokens between ISPACE and INSTn are used by the per keyword switch
 */
# define ISPACE 	AWKFIXESME
# define IBYTE 		AWKFIXESME
# define IWORD 		AWKFIXESME
# define IINT 		AWKFIXESME
# define ILONG 		AWKFIXESME
# define IQUAD 		AWKFIXESME
# define IOCTA 		AWKFIXESME
# define IDATA 		AWKFIXESME
# define IGLOBAL 	AWKFIXESME
# define ISET 		AWKFIXESME
# define ITEXT 		AWKFIXESME
# define ICOMM 		AWKFIXESME
# define ILCOMM 	AWKFIXESME
# define IFFLOAT 	AWKFIXESME
# define IDFLOAT 	AWKFIXESME
# define IGFLOAT 	AWKFIXESME
# define IHFLOAT 	AWKFIXESME
# define IORG 		AWKFIXESME
# define IASCII 	AWKFIXESME
# define IASCIZ 	AWKFIXESME
# define ILSYM 		AWKFIXESME
# define IFILE 		AWKFIXESME
# define ILINENO 	AWKFIXESME
# define IABORT 	AWKFIXESME
# define IFILL		AWKFIXESME
/*
 *	Tokens between ISTAB and REG are used in the per token switch
 */
# define ISTAB 		AWKFIXESME
# define ISTABSTR 	AWKFIXESME
# define ISTABNONE 	AWKFIXESME
# define ISTABDOT 	AWKFIXESME
# define IJXXX 		AWKFIXESME
# define IALIGN 	AWKFIXESME
# define INST0 		AWKFIXESME
# define INSTn 		AWKFIXESME

# define BFINT		AWKFIXESME
# define PARSEEOF	AWKFIXESME
# define ILINESKIP	AWKFIXESME
# define VOID		AWKFIXESME
# define SKIP		AWKFIXESME
# define INT		AWKFIXESME
# define BIGNUM		AWKFIXESME
# define NAME		AWKFIXESME
# define STRING		AWKFIXESME
/*
 *	Tokens between SIZESPEC and REGOP are used in the instruction
 *	argument switch
 */
# define SIZESPEC 	AWKFIXESME
# define REG		AWKFIXESME
# define MUL		AWKFIXESME
# define LITOP		AWKFIXESME
# define LP		AWKFIXESME
# define MP		AWKFIXESME
/*
 *	Tokens between REGOP and DIV are used in the per character switch
 */
# define NEEDSBUF	AWKFIXESME	/*signal refilling the input buffer*/
# define REGOP		AWKFIXESME	/*the percent sign*/
# define NL		AWKFIXESME
# define SCANEOF	AWKFIXESME
# define BADCHAR	AWKFIXESME
# define SP		AWKFIXESME
# define ALPH		AWKFIXESME
# define DIG		AWKFIXESME
# define SQ		AWKFIXESME
# define DQ		AWKFIXESME
# define SH		AWKFIXESME
# define LSH		AWKFIXESME
# define RSH		AWKFIXESME
# define MINUS		AWKFIXESME
# define SIZEQUOTE	AWKFIXESME
/*
 *	Tokens between XOR and RP are used at random (primarily by the
 *	expression analyzer), and not used in any switch
 */
# define XOR		AWKFIXESME
# define DIV		AWKFIXESME

# define SEMI		AWKFIXESME
# define COLON		AWKFIXESME
# define PLUS		AWKFIXESME
# define IOR		AWKFIXESME 
# define AND		AWKFIXESME
# define TILDE		AWKFIXESME
# define ORNOT		AWKFIXESME
# define CM		AWKFIXESME
# define LB		AWKFIXESME
# define RB		AWKFIXESME
# define RP		AWKFIXESME

# define LASTTOKEN	AWKFIXESME
