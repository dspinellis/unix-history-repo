/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 *
 *	@(#)optab.h	5.1 (Berkeley) 4/4/91
 */

/*
 * Argument access types
 */
#define ACCA	(8<<3)		/* address only */
#define ACCR	(1<<3)		/* read */
#define ACCW	(2<<3)		/* write */
#define ACCM	(3<<3)		/* modify */
#define ACCB	(4<<3)		/* branch displacement */
#define ACCI	(5<<3)		/* XFC code */

/*
 * Argument data types
 */
#define TYPB	0		/* byte */
#define TYPW	1		/* word */
#define TYPL	2		/* long */
#define TYPQ	3		/* quad */
#define TYPF	4		/* float */
#define TYPD	5		/* double */

/*
 * Special instructions
 */
#define	KCALL	0xcf
#define	CASEL	0xfc

extern struct optab {
	char	*iname;
	u_char	val;
	u_char	nargs;
	u_char	argtype[6];
} optab[];

extern char *regname[];
