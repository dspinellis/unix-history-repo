/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 */


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


extern FUNC *curfunc;		/* current function being compiled */
extern FUNC *findfunc();	/* return function given index */
extern char *namefunc();	/* return function name given index */
extern BOOL evaluate();		/* evaluate a line */
extern long adduserfunc();
extern void beginfunc();	/* initialize a function for definition */
extern int builtinopcode();	/* return the opcode for a built-in function */
extern void addop();		/* add an opcode to the current function */
extern void endfunc();		/* commit the just defined function for use */
extern void addopindex();	/* add an opcode & index to the current func */
extern void addoplabel();	/* add jump-type opcode + label to the func */
extern void addopptr();		/* add an opcode + char ptr to the func */
extern void showbuiltins();	/* show list of primitive builtin funcs */
extern int getbuiltinfunc();	/* return the index of a built-in func */
extern void builtincheck();	/* determine if the # of arguments are legal */
extern void addopfunction();	/* add opcode + index + arg count to the func */
extern void showfunctions();	/* show the list of user defined functs */
extern void clearopt();		/* clear optimization done for next opcode */

/* END CODE */
