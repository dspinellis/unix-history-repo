/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)pccdefs.h	5.1 (Berkeley) 6/7/85
 */

#define P2BAD -1
#define P2NAME 2
#define P2ICON 4
#define P2PLUS 9
#define P2PLUSEQ 10
#define P2MINUS 11
#define P2NEG 13
#define P2STAR 14
#define P2STAREQ 15
#define P2INDIRECT 16
#define P2BITAND 17
#define P2BITOR 20
#define P2BITXOR 22
#define P2QUEST 24
#define P2COLON 25
#define P2ANDAND 26
#define P2OROR 27
#define P2GOTO 40
#define P2LISTOP 59
#define P2ASSIGN 61
#define P2COMOP 62
#define P2SLASH 63
#define P2MOD 65
#define P2LSHIFT 67
#define P2RSHIFT 69
#define P2CALL 73
#define P2CALL0 75

#define P2NOT 79
#define P2BITNOT 80
#define P2EQ 83
#define P2NE 84
#define P2LE 85
#define P2LT 86
#define P2GE 87
#define P2GT 88
#define P2REG 97
#define P2OREG 98
#define P2CONV 107
#define P2FORCE 111
#define P2CBRANCH 112

/* special operators included only for fortran's use */

#define P2PASS 200
#define P2STMT 201
#define P2SWITCH 202
#define P2LBRACKET 203
#define P2RBRACKET 204
#define P2EOF 205
#define P2ARIF 206
#define P2LABEL 207

#if TARGET==PDP11
#	define P2SHORT 4
#	define P2INT 4
#	define P2LONG 5
#else
#	define P2SHORT 3
#	define P2INT 4
#	define P2LONG 4
#endif

#define P2CHAR 2
#define P2REAL 6
#define P2DREAL 7
#define P2PTR 020
#define P2FUNCT 040
