/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)input.h	8.2 (Berkeley) %G%
 */

/* PEOF (the end of file marker) is defined in syntax.h */

/*
 * The input line number.  Input.c just defines this variable, and saves
 * and restores it when files are pushed and popped.  The user of this
 * package must set its value.
 */
extern int plinno;
extern int parsenleft;		/* number of characters left in input buffer */
extern char *parsenextc;	/* next character in input buffer */
extern int init_editline;	/* 0 == not setup, 1 == OK, -1 == failed */

char *pfgets __P((char *, int));
int pgetc __P((void));
int preadbuffer __P((void));
void pungetc __P((void));
void pushstring __P((char *, int, void *));
void popstring __P((void));
void setinputfile __P((char *, int));
void setinputfd __P((int, int));
void setinputstring __P((char *, int)); 
void popfile __P((void));
void popallfiles __P((void));
void closescript __P((void));

#define pgetc_macro()	(--parsenleft >= 0? *parsenextc++ : preadbuffer())
