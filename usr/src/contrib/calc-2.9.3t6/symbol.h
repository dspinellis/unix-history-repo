/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 */

#ifndef	SYMBOL_H
#define	SYMBOL_H

#include "zmath.h"


/*
 * Symbol Declarations.
 */
#define SYM_UNDEFINED	0	/* undefined symbol */
#define SYM_PARAM	1	/* parameter symbol */
#define SYM_LOCAL	2	/* local symbol */
#define SYM_GLOBAL	3	/* global symbol */
#define	SYM_STATIC	4	/* static symbol */

#define	SCOPE_GLOBAL	0	/* file scope level for global variables */
#define	SCOPE_STATIC	1	/* lowest file scope for static variables */


typedef struct global GLOBAL;
struct global {
	int g_len;		/* length of symbol name */
	short g_filescope;	/* file scope level of symbol (0 if global) */
	short g_funcscope;	/* function scope level of symbol */
	char *g_name;		/* global symbol name */
	VALUE g_value;		/* global symbol value */
	GLOBAL *g_next;		/* next symbol in hash chain */
};


/*
 * Routines to search for global symbols.
 */
extern GLOBAL *addglobal MATH_PROTO((char *name, BOOL isstatic));
extern GLOBAL *findglobal MATH_PROTO((char *name));


/*
 * Routines to return names of variables.
 */
extern char *localname MATH_PROTO((long n));
extern char *paramname MATH_PROTO((long n));
extern char *globalname MATH_PROTO((GLOBAL *sp));


/*
 * Routines to handle entering and leaving of scope levels.
 */
extern void resetscopes MATH_PROTO((void));
extern void enterfilescope MATH_PROTO((void));
extern void exitfilescope MATH_PROTO((void));
extern void enterfuncscope MATH_PROTO((void));
extern void exitfuncscope MATH_PROTO((void));


/*
 * Other routines.
 */
extern long addlocal MATH_PROTO((char *name));
extern long findlocal MATH_PROTO((char *name));
extern long addparam MATH_PROTO((char *name));
extern long findparam MATH_PROTO((char *name));
extern void initlocals MATH_PROTO((void));
extern void initglobals MATH_PROTO((void));
extern int writeglobals MATH_PROTO((char *name));
extern int symboltype MATH_PROTO((char *name));
extern void showglobals MATH_PROTO((void));

#endif

/* END CODE */
