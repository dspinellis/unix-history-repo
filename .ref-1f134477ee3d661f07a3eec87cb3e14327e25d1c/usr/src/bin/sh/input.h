/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)input.h	8.1 (Berkeley) %G%
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


#ifdef __STDC__
char *pfgets(char *, int);
int pgetc(void);
int preadbuffer(void);
void pungetc(void);
void pushstring(char *, int, void *);
void setinputfile(char *, int);
void setinputfd(int, int);
void setinputstring(char *, int);
void popfile(void);
void popallfiles(void);
void closescript(void);
#else
char *pfgets();
int pgetc();
int preadbuffer();
void pungetc();
void setinputfile();
void setinputfd();
void setinputstring();
void popfile();
void popallfiles();
void pushstring();
void closescript();
#endif

#define pgetc_macro()	(--parsenleft >= 0? *parsenextc++ : preadbuffer())
