/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)operators.h	8.1 (Berkeley) %G%
 */

#define	NOT		0
#define	ISBLOCK		1
#define	ISCHAR		2
#define	ISDIR		3
#define	ISEXIST		4
#define	ISFILE		5
#define	ISSETGID	6
#define	ISSYMLINK	7
#define	ISSTICKY	8
#define	STRLEN		9
#define	ISFIFO		10
#define	ISREAD		11
#define	ISSIZE		12
#define	ISTTY		13
#define	ISSETUID	14
#define	ISWRITE		15
#define	ISEXEC		16
#define	NULSTR		17

#define	FIRST_BINARY_OP	18
#define	OR1		18
#define	OR2		19
#define	AND1		20
#define	AND2		21
#define	STREQ		22
#define	STRNE		23
#define	EQ		24
#define	NE		25
#define	GT		26
#define	LT		27
#define	LE		28
#define	GE		29


#define	OP_INT		1	/* arguments to operator are integer */
#define	OP_STRING	2	/* arguments to operator are string */
#define	OP_FILE		3	/* argument is a file name */

extern char *const unary_op[];
extern char *const binary_op[];
extern const char op_priority[];
extern const char op_argflag[];
