/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)histedit.h	5.1 (Berkeley) %G%
 */

/*
 * editline.h: Line editor interface
 */
#ifndef _h_editline
#define _h_editline

#include <sys/types.h>
#include <stdio.h>

typedef struct editline EditLine;

/*
 * For user-defined function interface
 */
typedef struct lineinfo {
    const char *buffer;
    const char *cursor;
    const char *lastchar;
} LineInfo;

/*
 * el_set/el_get parameters
 */
#define EL_PROMPT	0
#define EL_FILE		1
#define EL_TERMINAL	2
#define EL_EDITOR	3
#define EL_SIGNAL	4
#define	EL_BIND		5
#define	EL_TELLTC	6
#define	EL_SETTC	7
#define	EL_ECHOTC	8
#define	EL_SETTY	9
#define	EL_ADDFN	10
#define EL_HIST		11

/*
 * EditLine editor function return codes.
 * For user-defined function interface
 */
#define	CC_NORM		0
#define	CC_NEWLINE	1
#define	CC_EOF		2
#define CC_ARGHACK	3
#define CC_REFRESH	4
#define	CC_CURSOR	5
#define	CC_ERROR	6
#define CC_FATAL	7

/*
 * Initialization, cleanup, and resetting
 */
EditLine	*el_init	__P((const char *, FILE *, FILE *));
void		 el_reset	__P((EditLine *));
void		 el_end		__P((EditLine *));


/*
 * Get a line, a character or push a string back in the input queue
 */
const char      *el_gets	__P((EditLine *, int *));
int		 el_getc	__P((EditLine *, char *));
void		 el_push	__P((EditLine *, const char *));

/*
 * High level function internals control
 * Parses argc, argv array and executes builtin editline commands
 */
int		 el_parse	__P((EditLine *, int, char **)); 

/*
 * Low level editline access function
 */
int 		 el_set		__P((EditLine *, int, ...));

/*
 * Must be called when the terminal changes size; If EL_SIGNAL
 * is set this is done automatically otherwise it is the responsibility
 * of the application
 */
void		 el_resize	__P((EditLine *));


/*
 * User-defined function interface.
 */
const LineInfo	*el_line	__P((EditLine *));
int   		 el_insertstr	__P((EditLine *, char *));
void		 el_deletestr	__P((EditLine *, int));

#endif /* _h_editline */
