/*-
 * Copyright (c) 1984, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)inline.h	7.2 (Berkeley) 5/8/91
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
