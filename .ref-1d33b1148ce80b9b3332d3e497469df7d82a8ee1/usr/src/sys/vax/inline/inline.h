/*-
 * Copyright (c) 1984, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)inline.h	7.2 (Berkeley) %G%
 */

/*
 * COMMENTCHAR is the character delimiting comments in the assembler.
 * LABELCHAR is the character that separates labels from instructions.
 * ARGSEPCHAR is the character that separates arguments in instructions.
 */
#define COMMENTCHAR	'#'
#define LABELCHAR	':'
#define ARGSEPCHAR	','

/*
 * Expansion parameters:
 *   QUEUESIZE is the number of instructions to be considered for 
 *	integration of argument pushes and pops
 *   MAXLINELEN is the longest expected input line
 *   MAXARGS is the maximum number of arguments in an assembly instruction
 */
#define QUEUESIZE	16
#define MAXLINELEN	1024
#define MAXARGS		10

/*
 * The following global variables are used to manipulate the queue of
 * recently seen instructions.
 *	line - The queue of instructions.
 *	bufhead - Pointer to next availble queue slot. It is not
 *		considered part of te instruction stream until
 *		bufhead is advanced.
 *	buftail - Pointer to last instruction in queue.
 * Note that bufhead == buftail implies that the queue is empty.
 */
int bufhead, buftail;
char line[QUEUESIZE][MAXLINELEN];

#define SUCC(qindex) ((qindex) + 1 == QUEUESIZE ? 0 : (qindex) + 1)
#define PRED(qindex) ((qindex) - 1 < 0 ? QUEUESIZE - 1 : (qindex) - 1)

/*
 * Hash table headers should be twice as big as the number of patterns.
 * They must be a power of two.
 */
#define HSHSIZ	128

/*
 * These tables specify the substitutions that are to be done.
 */
struct pats {
	int	args;
	char	*name;
	char	*replace;
	struct	pats *next;
	int	size;
};
struct pats *patshdr[HSHSIZ];
extern struct pats language_ptab[], libc_ptab[], machine_ptab[];
extern struct pats vax_libc_ptab[], vaxsubset_libc_ptab[];
extern struct pats vax_ptab[], vaxsubset_ptab[];

/*
 * This table defines the set of instructions that demark the
 * end of a basic block.
 */
struct inststoptbl {
	char	*name;
	struct	inststoptbl *next;
	int	size;
};
struct inststoptbl *inststoptblhdr[HSHSIZ];
extern struct inststoptbl inststoptable[];

/*
 * Miscellaneous functions.
 */
char *newline(), *copyline(), *doreplaceon();
