/* Copyright (c) 1979 Regents of the University of California */
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
# define FIRSTTOKEN	0

/*
 *	Tokens between ISPACE and INSTn are used by the per keyword switch
 */
# define ISPACE 	1
# define IBYTE 		2
# define IWORD 		3
# define IINT 		4
# define ILONG 		5
# define IDATA 		6
# define IGLOBAL 	7
# define ISET 		8
# define ITEXT 		9
# define ICOMM 		10
# define ILCOMM 	11
# define IFLOAT 	12
# define IDOUBLE 	13
# define IORG 		14
# define IASCII 	15
# define IASCIZ 	16
# define ILSYM 		17
# define IFILE 		18
# define ILINENO 	19
# define IABORT 	20
/*
 *	Tokens between ISTAB and REG are used in the per token switch
 */
# define ISTAB 		23
# define ISTABSTR 	24
# define ISTABNONE 	25
# define ISTABDOT 	26
# define IJXXX 		27
# define IALIGN 	28
# define INST0 		29
# define INSTn 		30

# define PARSEEOF	32
# define ILINESKIP	33
# define VOID		34
# define SKIP		35
# define INT		36
# define FLTNUM		37
# define NAME		38
# define STRING		39
/*
 *	Tokens between SIZESPEC and REGOP are used in the instruction
 *	argument switch
 */
# define SIZESPEC 	41
# define REG		42
# define MUL		43
# define LITOP		44
# define LP		45
# define MP		46
/*	
 *	Tokens between REGOP and DIV are used in the per character switch
 */
# define NEEDSBUF	48	/*signal refilling the input buffer*/
# define REGOP		49	/*the percent sign*/
# define NL		50
# define SCANEOF	51
# define BADCHAR	52
# define SP		53
# define ALPH		54
# define DIG		55
# define SQ		56
# define DQ		57
# define SH		58
# define LSH		59
# define RSH		60
# define MINUS		61
# define SIZEQUOTE	62
/*
 *	Tokens between XOR and RP are used at random (primarily by the
 *	expression analyzer), and not used in any switch
 */
# define XOR		64
# define DIV		65

# define SEMI		66
# define COLON		67
# define PLUS		68
# define IOR		69 
# define AND		70
# define TILDE		71
# define ORNOT		72
# define CM		73
# define LB		74
# define RB		75
# define RP		76

# define LASTTOKEN	80
