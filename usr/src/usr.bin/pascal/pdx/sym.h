/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)sym.h	8.1 (Berkeley) 6/6/93
 */

/*
 * This header file declares the variables and routines that
 * are defined within the "sym" subdirectory and that can be
 * accessed from outside.
 */

SYM *program;

/*
 * attributes
 */

char *name();			/* symbol string name */
char *classname();		/* class name of a symbol */
int toknum();			/* token number of reserved word */
int tokval();			/* associated token value */
int size();			/* size in bytes of a type */
SYM *rtype();			/* the reduced type, i.e. no type names */
SYM *container();		/* symbol (block) that contains given symbol */
ADDRESS codeloc();		/* address of the beginning of a function */

/*
 * predicates
 */

BOOLEAN isblock();		/* test if a symbol is a block name */
BOOLEAN isbuiltin();		/* test if a symbol is a builtin type */
BOOLEAN isparam();		/* test if a symbol is a parameter */
BOOLEAN isvarparam();		/* test if a symbol is a var parameter */
BOOLEAN isvariable();		/* test if a symbol is a variable */
BOOLEAN isambiguous();		/* test if a symbol name is not unique */
BOOLEAN compatible();		/* test to see if two types are compatible */
BOOLEAN should_print();		/* test if a symbol should be printed */

SYM *readsym();			/* read in a symbol from object file */
SYM *which();			/* find the appropriate symbol of given name */
SYM *findsym();			/* find a symbol for a given block */
SYM *findclass();		/* find symbol with given class */
NODE *dot();			/* construct a tree for the dot operator */
NODE *subscript();		/* construct a tree for subscripting */
SYM *treetype();		/* return the type of a tree, checking also */
long evalindex();		/* evaluate a subscript index */
int unmkstring();		/* free a constant string type */
int chkboolean();		/* check if a tree is boolean-valued */
int printdecl();		/* print out a symbol's declaration */
int printparams();		/* print out values of a fn's parameters */
int printentry();		/* note entrance of a block */
int printexit();		/* note exiting from a block */
int printcall();		/* note call of a function */
int printval();			/* print an eval result */
int printv();			/* print the name and value of a variable */
int printwhich();		/* print the full "path" of an identifier */
int maketypes();		/* initialize basic types */
int make_keyword();		/* create a keyword in a given symbol table */
