/* Copyright (c) 1982 Regents of the University of California */

/* static char sccsid[] = "@(#)tree.h 1.3 %G%"; */

/*
 * This file contains the declarations of the variables and routines
 * within the "tree" subdirectory that are accessible from outside.
 */

#include "tree/opinfo.h"

/*
 * Evaluation stack manipulation macros.  These are publically
 * available because "eval" leaves it's result on the stack.
 *
 * These macros allow one to operate on stacks of arbitrary types
 * (including a stack of different typed objects).
 *
 * Sadly, underflow and overflow are not checked for.
 */

typedef char STACK;

#define WMASK			(sizeof(int) - 1)

#define push(type, value)	((type *) (sp += sizeof(type)))[-1] = (value)
#define pop(type)		(*((type *) (sp -= sizeof(type))))
#define alignstack()		sp = (char *) (( ((int) sp) + WMASK)&~WMASK)

STACK stack[];
STACK *sp;

NODE *build();		/* create a node in the parse tree */
prtree();		/* print a tree in source form */
eval();			/* evaluate a tree, leaving value on stack */
tfree();		/* release storage for a tree */
BOOLEAN tr_equal();	/* test if two trees are structurally equivalent */
BOOLEAN cond();		/* evaluate a node for a conditional */
ADDRESS lval();		/* return the object address of a node */
BOOLEAN isredirected();	/* TRUE if output is being redirected */
