/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jim R. Oldroyd at The Instruction Set and Keith Gabryelski at
 * Commodore Business Machines.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)quiz.h	8.1 (Berkeley) %G%
 */

#define	TRUE		1
#define	FALSE		0

/* Length of compiled regexp machine; increase if not big enough. */
#define	RXP_LINE_SZ	8192

/* Maximum line length for data files. */
#define	LINE_SZ		1024

/* Linked list for holding index and data file information. */
typedef struct qentry {
	struct qentry *q_next;		/* next one */
	char	*q_text;		/* category text string from file */
	int	 q_asked;		/* TRUE if question's been asked */
	int	 q_answered;		/* TRUE if question's been answered */
} QE;

extern char rxperr[];

int	 rxp_compile __P((char *));
char	*rxp_expand __P((void));
int	 rxp_match __P((char *));
