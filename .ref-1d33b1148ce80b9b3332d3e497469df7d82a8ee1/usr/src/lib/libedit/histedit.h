/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)histedit.h	8.2 (Berkeley) %G%
 */

/*
 * histedit.h: Line editor and history interface.
 */
#ifndef _h_editline
#define _h_editline

#include <sys/types.h>
#include <stdio.h>

/*
 * ==== Editing ====
 */
typedef struct editline EditLine;

/*
 * For user-defined function interface
 */
typedef struct lineinfo {
    __const char *buffer;
    __const char *cursor;
    __const char *lastchar;
} LineInfo;


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
__const char    *el_gets	__P((EditLine *, int *));
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
 * el_set/el_get parameters
 */
#define EL_PROMPT	0	/* , el_pfunc_t);		*/
#define EL_TERMINAL	1	/* , const char *);		*/
#define EL_EDITOR	2	/* , const char *);		*/
#define EL_SIGNAL	3	/* , int);			*/
#define	EL_BIND		4	/* , const char *, ..., NULL);	*/
#define	EL_TELLTC	5	/* , const char *, ..., NULL);	*/
#define	EL_SETTC	6	/* , const char *, ..., NULL);	*/
#define	EL_ECHOTC	7	/* , const char *, ..., NULL);	*/
#define	EL_SETTY	8	/* , const char *, ..., NULL);	*/
#define	EL_ADDFN	9	/* , const char *, const char *	*/
				/* , el_func_t);		*/
#define EL_HIST		10	/* , hist_fun_t, const char *);	*/

/*
 * Source named file or $PWD/.editrc or $HOME/.editrc
 */
int		el_source	__P((EditLine *, const char *));

/*
 * Must be called when the terminal changes size; If EL_SIGNAL
 * is set this is done automatically otherwise it is the responsibility
 * of the application
 */
void		 el_resize	__P((EditLine *));


/*
 * User-defined function interface.
 */
__const LineInfo *el_line	__P((EditLine *));
int   		  el_insertstr	__P((EditLine *, char *));
void		  el_deletestr	__P((EditLine *, int));

/*
 * ==== History ====
 */

typedef struct history History;

typedef struct HistEvent {
    int 	  num;
    __const char *str;
} HistEvent;

/*
 * History access functions.
 */
History *		history_init	__P((void));
void 			history_end	__P((History *));

__const HistEvent *	history		__P((History *, int, ...));

#define H_FUNC		 0	/* , UTSL		*/
#define H_EVENT		 1	/* , const int);	*/
#define H_FIRST		 2	/* , void);		*/
#define H_LAST		 3	/* , void);		*/
#define H_PREV		 4	/* , void);		*/
#define H_NEXT		 5	/* , void);		*/
#define H_CURR		 6	/* , void);		*/
#define H_ADD		 7	/* , const char*);	*/
#define H_ENTER		 8	/* , const char*);	*/
#define H_END		 9	/* , void);		*/
#define H_NEXT_STR	10	/* , const char*);	*/
#define H_PREV_STR	11	/* , const char*);	*/
#define H_NEXT_EVENT	12	/* , const int);	*/
#define H_PREV_EVENT	13	/* , const int);	*/

#endif /* _h_editline */
