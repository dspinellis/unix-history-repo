/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)defs.h	5.3 (Berkeley) %G%
 */

/*
 * Global debugger defines.
 *
 * All files include this header.
 */

#include <stdio.h>

/*
 * Since C does not allow forward referencing of types,
 * all the global types are declared here.
 */

#define LOCAL static
#define NIL 0

typedef int BOOLEAN;

#define FALSE 0
#define TRUE 1

typedef unsigned int ADDRESS;		/* object code addresses */
typedef short LINENO;			/* source file line numbers */
typedef struct sym SYM;			/* symbol information structure */
typedef struct symtab SYMTAB;		/* symbol table */
typedef struct node NODE;		/* expression tree node */
typedef short OP;			/* tree operator */
typedef struct opinfo OPINFO;		/* tree operator information table */
typedef unsigned int WORD;		/* machine word */
typedef unsigned char BYTE;		/* machine byte */
typedef struct frame FRAME;		/* runtime activation record */

/*
 * Definitions of standard C library routines that aren't in the
 * standard I/O library, but which are generally useful.
 */

extern long atol();		/* ascii to long */
extern double atof();		/* ascii to floating point */
extern char *mktemp();		/* make a temporary file name */

/*
 * Definitions of library routines.
 */

char *cmdname;			/* name of command for error messages */
char *errfilename;		/* current file associated with error */
short errlineno;		/* line number associated with error */

int error();			/* print an error message */
int panic();			/* print error message and exit */
short numerrors();		/* return number of errors since last call */

/*
 * defintions for doing memory allocation
 */

extern char *malloc();

#define alloc(n, type)	((type *) malloc((unsigned) (n) * sizeof(type)))
#define dispose(p)	{ free((char *) p); p = NIL; }

/*
 * macro for doing freads
 */

#define get(fp, var)	fread((char *) &(var), sizeof(var), 1, fp)

/*
 * string definitions
 */

extern char *strcpy();
extern int strlen();

#define strdup(s)		strcpy(malloc((unsigned) strlen(s) + 1), s)
#define streq(s1, s2)	(strcmp(s1, s2) == 0)
