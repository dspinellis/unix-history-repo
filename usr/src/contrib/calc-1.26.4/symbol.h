/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 */


/*
 * Symbol Declarations.
 */
#define SYM_UNDEFINED	0	/* undefined symbol */
#define SYM_PARAM	1	/* paramater symbol */
#define SYM_LOCAL	2	/* local symbol */
#define SYM_GLOBAL	3	/* global symbol */


typedef struct global GLOBAL;
struct global {
	int g_len;		/* length of symbol name */
	char *g_name;		/* global symbol name */
	VALUE g_value;		/* global symbol value */
	GLOBAL *g_next;		/* next symbol in hash chain */
};


/*
 * Routines to search for global symbols.
 */
extern GLOBAL *addglobal(), *findglobal();


/*
 * Routines to return names of variables.
 */
extern char *localname(), *globalname(), *paramname();


/*
 * Other routines.
 */
extern long addlocal(), findlocal(), addparam(), findparam();
extern void initlocals();
extern void initglobals();
extern void initfunctions();
extern int writeglobals();
extern int symboltype();	/* return the type of a variable name */
extern void showglobals();	/* show the value of all global variables */

/* END CODE */
