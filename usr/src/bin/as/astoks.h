/* Copyright (c) 1980 Regents of the University of California */
/* "@(#)astoks.H 4.3 2/14/82" */
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
# define FIRSTTOKEN 0

/*
 *	Tokens between ISPACE and INSTn are used by the per keyword switch
 */
# define ISPACE 1
# define IBYTE 2
# define IWORD 3
# define IINT 4
# define ILONG 5
# define IQUAD 6
# define IOCTA 7
# define IDATA 8
# define IGLOBAL 9
# define ISET 10
# define ITEXT 11
# define ICOMM 12
# define ILCOMM 13
# define IFFLOAT 14
# define IDFLOAT 15
# define IGFLOAT 16
# define IHFLOAT 17
# define IORG 18
# define IASCII 19
# define IASCIZ 20
# define ILSYM 21
# define IFILE 22
# define ILINENO 23
# define IABORT 24
# define IFILL 25
/*
 *	Tokens between ISTAB and REG are used in the per token switch
 */
# define ISTAB 26
# define ISTABSTR 27
# define ISTABNONE 28
# define ISTABDOT 29
# define IJXXX 30
# define IALIGN 31
# define INST0 32
# define INSTn 33

# define BFINT 34
# define PARSEEOF 35
# define ILINESKIP 36
# define VOID 37
# define SKIP 38
# define INT 39
# define BIGNUM 40
# define NAME 41
# define STRING 42
/*
 *	Tokens between SIZESPEC and REGOP are used in the instruction
 *	argument switch
 */
# define SIZESPEC 43
# define REG 44
# define MUL 45
# define LITOP 46
# define LP 47
# define MP 48
/*
 *	Tokens between REGOP and DIV are used in the per character switch
 */
# define NEEDSBUF 49 /*signal refilling the input buffer*/
# define REGOP 50 /*the percent sign*/
# define NL 51
# define SCANEOF 52
# define BADCHAR 53
# define SP 54
# define ALPH 55
# define DIG 56
# define SQ 57
# define DQ 58
# define SH 59
# define LSH 60
# define RSH 61
# define MINUS 62
# define SIZEQUOTE 63
/*
 *	Tokens between XOR and RP are used at random (primarily by the
 *	expression analyzer), and not used in any switch
 */
# define XOR 64
# define DIV 65

# define SEMI 66
# define COLON 67
# define PLUS 68
# define IOR 69
# define AND 70
# define TILDE 71
# define ORNOT 72
# define CM 73
# define LB 74
# define RB 75
# define RP 76

# define LASTTOKEN 77
