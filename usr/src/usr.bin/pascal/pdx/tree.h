/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tree.h	5.3 (Berkeley) %G%
 */

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

#ifdef tahoe
#define push(type, value)	((*(type *)sp) = value, sp += (sizeof(type) + WMASK) & ~WMASK, value)
#define	pop(type)		(sp -= (sizeof(type) + WMASK) & ~WMASK, (*((type *) sp)))
#else
#define push(type, value)	((type *) (sp += sizeof(type)))[-1] = (value)
#define pop(type)		(*((type *) (sp -= sizeof(type))))
#endif
#define alignstack()		sp = (char *) (( ((int) sp) + WMASK)&~WMASK)
#define downalignstack()	sp = (char *) (( ((int) sp))&~WMASK)

STACK stack[];
STACK *sp;

NODE *build();		/* create a node in the parse tree */
int prtree();		/* print a tree in source form */
int eval();		/* evaluate a tree, leaving value on stack */
long popsmall();	/* pop a small item from the stack given its type */
int tfree();		/* release storage for a tree */
BOOLEAN tr_equal();	/* test if two trees are structurally equivalent */
BOOLEAN cond();		/* evaluate a node for a conditional */
ADDRESS lval();		/* return the object address of a node */
BOOLEAN isredirected();	/* TRUE if output is being redirected */
