/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 */


#ifndef	FUNC_H
#define	FUNC_H

#include "calc.h"
#include "label.h"


/*
 * Structure of a function.
 * The f_opcodes array is actually of variable size.
 */
typedef struct func FUNC;
struct func {
	FUNC *f_next;			/* next function in list */
	unsigned long f_opcodecount;	/* size of opcode array */
	unsigned int f_localcount;	/* number of local variables */
	unsigned int f_paramcount;	/* max number of parameters */
	char *f_name;			/* function name */
	VALUE f_savedvalue;		/* saved value of last expression */
	long f_opcodes[1];		/* array of opcodes (variable length) */
};


/*
 * Amount of space needed to allocate a function of n opcodes.
 */
#define funcsize(n) (sizeof(FUNC) + (n) * sizeof(long))


/*
 * Size of a character pointer rounded up to a number of opcodes.
 */
#define PTR_SIZE ((sizeof(char *) + sizeof(long) - 1) / sizeof(long))


/*
 * The current function being compiled.
 */
extern FUNC *curfunc;


/*
 * Functions to handle functions.
 */
extern FUNC *findfunc MATH_PROTO((long index));
extern char *namefunc MATH_PROTO((long index));
extern BOOL evaluate MATH_PROTO((BOOL nestflag));
extern long adduserfunc MATH_PROTO((char *name));
extern void beginfunc MATH_PROTO((char *name, BOOL newflag));
extern int builtinopcode MATH_PROTO((long index));
extern char *builtinname MATH_PROTO((long index));
extern int dumpop MATH_PROTO((long *pc));
extern void addop MATH_PROTO((long op));
extern void endfunc MATH_PROTO((void));
extern void addopone MATH_PROTO((long op, long arg));
extern void addoptwo MATH_PROTO((long op, long arg1, long arg2));
extern void addoplabel MATH_PROTO((long op, LABEL *label));
extern void addopptr MATH_PROTO((long op, char *ptr));
extern void writeindexop MATH_PROTO((void));
extern void showbuiltins MATH_PROTO((void));
extern int getbuiltinfunc MATH_PROTO((char *name));
extern void builtincheck MATH_PROTO((long index, int count));
extern void addopfunction MATH_PROTO((long op, long index, int count));
extern void showfunctions MATH_PROTO((void));
extern void initfunctions MATH_PROTO((void));
extern void clearopt MATH_PROTO((void));
extern void updateoldvalue MATH_PROTO((FUNC *fp));
extern void calculate MATH_PROTO((FUNC *fp, int argcount));
extern VALUE builtinfunc MATH_PROTO((long index, int argcount, VALUE *stck));

#endif

/* END CODE */
